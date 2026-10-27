# ==============================================================================
# 專案名稱：台灣實價登錄自動化 ETL 系統（new 版）
# 原始檔：All_data_ETL.R（保留不動，方便回退）
#
# 與舊版差異（重點修正）：
#   1. 【嚴重】舊版合併時沒有記錄「縣市」→ 中正區/東區/北區/信義區 等同名行政區會混在一起，
#      而且約 30 萬筆地址本身沒寫縣市。新版從檔名第一個字母 (a=臺北市, e=高雄市...) 補上 city。
#   2. 【嚴重】建築完成年月 6 碼 (例 "087129") 舊版會被解析成民國 8 年 → 西元 1919。
#      "000000" 會被解析成 1911 年並通過 >=1900 的檢查。新版改成「取前段數字 + 判斷是否補零」。
#   3. 【Bug】fact_residential_house 裡 include_parking = 'Tier_1' 永遠不成立，
#      導致「扣車位純單價」從來沒有被計算。已改回 '是'。
#   4. 讀檔失敗不再靜默吞掉，會列出失敗檔案。
#   5. 改成「以季為單位」增量寫入 (DELETE 該季 → INSERT)，可重複執行不會重複資料 (idempotent)，
#      之後每週自動更新只需要處理新的一季。
#   6. 保留最近 N 季 (預設 20 季 = 5 年)，最舊的季自動從資料庫移除。
#   7. 在 R 端先做輕量標準化：全形數字→半形、台→臺、補縣市、去樓層 (給地理編碼用的 addr_key)、
#      中文樓層「十一層」→ 11。
#
# 執行順序：整支檔案 source() 即可；每一段都可以單獨重跑。
# ==============================================================================
library(httr)
library(tidyverse)
library(DBI)
library(RPostgres)

# --- 0. 參數設定 --------------------------------------------------------------
# 共用設定 (路徑、資料庫連線)：本機和 GitHub Actions 都用同一份程式
source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "config.R"))
base_path <- project_path
setwd(base_path)

history_dir    <- "raw_history_zips"
temp_unzip_dir <- "temp_unzip_all"
walk(c(history_dir, temp_unzip_dir), ~ if (!dir.exists(.x)) dir.create(.x))

keep_n_seasons <- 20                                   # 保留最近 20 季 (5 年)
current_roc    <- as.integer(format(Sys.Date(), "%Y")) - 1911   # 今年的民國年

# 檔名第一個字母 → 縣市 (內政部固定代碼；l, r, s, y 已停用)
city_code <- c(
  a = "臺北市", b = "臺中市", c = "基隆市", d = "臺南市", e = "高雄市", f = "新北市",
  g = "宜蘭縣", h = "桃園市", i = "嘉義市", j = "新竹縣", k = "苗栗縣", m = "南投縣",
  n = "彰化縣", o = "新竹市", p = "雲林縣", q = "嘉義縣", t = "屏東縣", u = "花蓮縣",
  v = "臺東縣", w = "金門縣", x = "澎湖縣", z = "連江縣"
)

# --- 1. 下載 (沿用舊版邏輯，只改成「最近 N 季」的滾動視窗) -------------------
# 季資料要等季結束後才會公布，所以「本季」不抓。
all_periods <- expand.grid(s = 1:4, y = 109:current_roc) %>%
  mutate(id = paste0(y, "S", s)) %>%
  filter(!(y == current_roc & s >= ceiling(as.integer(format(Sys.Date(), "%m")) / 3))) %>%
  arrange(y, s) %>%
  pull(id)

download_season <- function(p) {
  dest_file  <- file.path(history_dir, paste0(p, ".zip"))
  unzip_path <- file.path(temp_unzip_dir, p)

  if (file.exists(dest_file) && file.info(dest_file)$size >= 40000) {
    if (!dir.exists(unzip_path)) unzip(dest_file, exdir = unzip_path)
    return(TRUE)                                       # 已經有了
  }
  url <- paste0("https://plvr.land.moi.gov.tw//DownloadSeason?season=", p,
                "&type=zip&fileName=lvr_landcsv.zip")
  ok <- tryCatch({
    GET(url, write_disk(dest_file, overwrite = TRUE), timeout(300))
    if (file.info(dest_file)$size < 40000) {           # 政府空包彈 (尚未公布)
      file.remove(dest_file); FALSE
    } else {
      unzip(dest_file, exdir = unzip_path); TRUE
    }
  }, error = function(e) { message("   [錯誤] ", p, "：", e$message); FALSE })
  if (ok) { message(">> [下載完成] ", p); Sys.sleep(runif(1, 2, 4)) }
  ok
}

available <- all_periods[map_lgl(all_periods, download_season)]
periods   <- tail(available, keep_n_seasons)          # 只取最近 N 季
message(">> 使用季別：", paste(range(periods), collapse = " ~ "), "（共 ", length(periods), " 季）")

# --- 2. 欄位對照 (沿用舊版的英文短名) ----------------------------------------
# 用位置對應，而不是用英文標題字串對應：政府的英文標題有大小寫不一致的問題，
# 但 33 個欄位的「順序」在 109S1~115S1 都一樣 (已逐季檢查)。
std_names <- c(
  "district", "trans_type", "address", "land_area", "zoning", "non_metro_district",
  "non_metro_use", "trans_date", "trans_pen_number", "floor_level", "total_floors",
  "building_state", "main_use", "main_building_materials", "built_date", "building_area",
  "room_count", "hall_count", "bath_count", "has_compartment", "has_management", "price",
  "unit_price", "berth_type", "berth_area", "berth_price", "note", "serial_number",
  "main_building_area", "aux_building_area", "balcony_area", "has_elevator", "trans_number"
)

# --- 3. R 端輕量標準化工具 ----------------------------------------------------
# 中文數字 → 整數：「十一層」→11、「二十層」→20、「地下二層」→-2、「全」→NA
cn_to_int <- function(x) {
  digits <- c("一" = 1, "二" = 2, "三" = 3, "四" = 4, "五" = 5,
              "六" = 6, "七" = 7, "八" = 8, "九" = 9)
  one <- function(s) {
    if (is.na(s)) return(NA_integer_)
    neg <- grepl("^地下", s)
    s <- sub("^地下", "", s)
    s <- sub("層.*$", "", s)                          # 「十一層，十二層」只取第一個
    if (s == "" || !grepl("^[一二三四五六七八九十]+$", s)) return(NA_integer_)
    if (s == "十") v <- 10
    else if (grepl("^十", s)) v <- 10 + digits[substr(s, 2, 2)]
    else if (grepl("十", s)) {
      parts <- strsplit(s, "十")[[1]]
      v <- digits[parts[1]] * 10 + ifelse(length(parts) > 1 && parts[2] != "", digits[parts[2]], 0)
    } else v <- digits[s]
    as.integer(if (neg) -v else v)
  }
  u <- unique(x)                                       # 只算不重複值 → 快很多
  unname(setNames(vapply(u, one, integer(1)), u)[x])
}

# 地址標準化 + 產生地理編碼用的 key
normalize_address <- function(addr, city, district) {
  a <- chartr("０１２３４５６７８９－～", "0123456789-~", addr)
  a <- gsub("台", "臺", a)
  a <- gsub("巿", "市", a)                             # 「巿」(U+5DFF) 長得像「市」但是不同字，原始資料真的有
  a <- gsub("\\s", "", a)
  # 沒寫縣市的補上縣市；沒寫區的補上區
  no_city <- !startsWith(a, city) & !is.na(a)
  a[no_city] <- ifelse(startsWith(a[no_city], district[no_city]),
                       paste0(city[no_city], a[no_city]),
                       paste0(city[no_city], district[no_city], a[no_city]))
  a
}

# addr_key：到「號」為止 (去掉樓層)，同一棟樓的所有交易共用一組經緯度
make_addr_key <- function(a) {
  k <- sub("(號).*$", "\\1", a)
  k[grepl("地號$", a)] <- NA                           # 純土地 (地段地號) 無法用門牌定位
  k
}

# 預售屋 (b 檔) 的欄位：前 28 欄和 a 檔一樣 (到「編號」為止)，後面接「建案名稱、棟及號」，
# 112S3 之後又多了一欄「解約情形」→ 欄位數 30 或 31，讀檔時依實際欄位數決定名稱。
std_names_b <- c(std_names[1:28], "build_case", "building_no", "termination")

# kind = "a"：不動產買賣 (成屋、土地、車位)；kind = "b"：預售屋買賣契約
read_one_season <- function(p, kind = "a") {
  files <- list.files(file.path(temp_unzip_dir, p), pattern = paste0("^[a-z]_lvr_land_", kind, "\\.csv$"),
                      full.names = TRUE)
  map_dfr(files, function(f) {
    nm <- std_names
    if (kind == "b") {
      n_col <- length(strsplit(readLines(f, n = 1, encoding = "UTF-8"), ",")[[1]])
      nm <- std_names_b[seq_len(n_col)]
    }
    df <- tryCatch(
      read_csv(f, skip = 2, col_names = nm, col_types = cols(.default = "c"),
               locale = locale(encoding = "UTF-8"), show_col_types = FALSE),
      error = function(e) { warning("讀檔失敗：", f, " → ", e$message); NULL })
    if (is.null(df) || nrow(df) == 0) return(NULL)
    if (kind == "b" && !"termination" %in% names(df)) df$termination <- NA_character_
    df$city   <- unname(city_code[substr(basename(f), 1, 1)])
    df$season <- p
    df
  })
}

# R 端標準化 (a、b 共用)
standardize <- function(df) {
  df %>% mutate(
    district         = gsub("台", "臺", district),     # 原始資料混用「台西鄉/臺西鄉」，統一成「臺」
    address_norm     = normalize_address(address, city, district),
    addr_key         = make_addr_key(address_norm),
    floor_num        = cn_to_int(floor_level),
    total_floors_num = cn_to_int(total_floors)
  )
}

# --- 4. 寫入資料庫 (以季為單位，可重複執行) -----------------------------------
# 【資料庫小教室】
#   raw_trade = 「原始層 (staging / bronze)」：所有欄位都存成文字，盡量保持原貌。
#   好處：清洗規則改了，只要重跑 SQL，不用重新下載、重新讀 CSV。
#   這就是 ELT (先 Load 再 Transform) 的精神。
con <- db_connect()

# 【資料庫小教室】schema = 資料庫裡的「資料夾」。新版全部放在 re_v2，
# 舊版放在 public 的 real_estate_final / real_estate_clean 等表完全不會被動到。
# search_path 告訴 PostgreSQL：沒寫 schema 的表名，去 re_v2 找。
#
# ⚠️ 血的教訓 (2026-10-27)：這裡原本寫 "re_v2, public"。
# 結果 DROP TABLE IF EXISTS real_estate_clean 在 re_v2 找不到這張表時，
# 會「順著 search_path 往下找」→ 找到 public.real_estate_clean → 把舊版的表刪掉了。
# 所以會執行 DROP 的程式，search_path 只能放「自己要寫的那一個 schema」。
dbExecute(con, "CREATE SCHEMA IF NOT EXISTS re_v2;")
dbExecute(con, "SET search_path TO re_v2;")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS raw_trade (
    city TEXT, season TEXT,
    district TEXT, trans_type TEXT, address TEXT, land_area TEXT, zoning TEXT,
    non_metro_district TEXT, non_metro_use TEXT, trans_date TEXT, trans_pen_number TEXT,
    floor_level TEXT, total_floors TEXT, building_state TEXT, main_use TEXT,
    main_building_materials TEXT, built_date TEXT, building_area TEXT, room_count TEXT,
    hall_count TEXT, bath_count TEXT, has_compartment TEXT, has_management TEXT, price TEXT,
    unit_price TEXT, berth_type TEXT, berth_area TEXT, berth_price TEXT, note TEXT,
    serial_number TEXT, main_building_area TEXT, aux_building_area TEXT, balcony_area TEXT,
    has_elevator TEXT, trans_number TEXT,
    -- 以下是 R 端標準化後新增的欄位
    address_norm TEXT, addr_key TEXT, floor_num INTEGER, total_floors_num INTEGER
  );")
# 索引：讓「刪除某一季」「用 addr_key 對照經緯度」變快
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_raw_season  ON raw_trade(season);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_raw_addrkey ON raw_trade(addr_key);")

loaded <- dbGetQuery(con, "SELECT DISTINCT season FROM raw_trade")$season
to_load <- setdiff(periods, loaded)
# 想全部重灌時：to_load <- periods

for (p in to_load) {
  df <- standardize(read_one_season(p, "a"))
  # 交易 (transaction) 包起來：DELETE 和 INSERT 要嘛一起成功、要嘛一起失敗，
  # 不會出現「刪了舊資料但新資料沒寫進去」的半套狀態。
  dbWithTransaction(con, {
    dbExecute(con, "DELETE FROM raw_trade WHERE season = $1", params = list(p))
    dbAppendTable(con, "raw_trade", as.data.frame(df))
  })
  message(">> [寫入 成屋/土地/車位] ", p, "：", nrow(df), " 筆")
}

# --- 4b. 預售屋契約 (b 檔) ----------------------------------------------------
# 預售屋是「還沒蓋好就先簽約」的交易，和成屋 (a 檔) 是不同的檔案、不同的欄位，所以獨立一張原始表。
# a 檔裡也有「預售屋」：那是預售屋蓋好後「過戶」的紀錄 (簽約日早於完工日)，見下方 trading_type。
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS raw_presale (
    city TEXT, season TEXT,
    district TEXT, trans_type TEXT, address TEXT, land_area TEXT, zoning TEXT,
    non_metro_district TEXT, non_metro_use TEXT, trans_date TEXT, trans_pen_number TEXT,
    floor_level TEXT, total_floors TEXT, building_state TEXT, main_use TEXT,
    main_building_materials TEXT, built_date TEXT, building_area TEXT, room_count TEXT,
    hall_count TEXT, bath_count TEXT, has_compartment TEXT, has_management TEXT, price TEXT,
    unit_price TEXT, berth_type TEXT, berth_area TEXT, berth_price TEXT, note TEXT,
    serial_number TEXT,
    build_case TEXT,      -- 建案名稱
    building_no TEXT,     -- 棟及號
    termination TEXT,     -- 解約情形 (112S3 起才有)
    address_norm TEXT, addr_key TEXT, floor_num INTEGER, total_floors_num INTEGER
  );")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_rawp_season  ON raw_presale(season);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_rawp_addrkey ON raw_presale(addr_key);")

to_load_b <- setdiff(periods, dbGetQuery(con, "SELECT DISTINCT season FROM raw_presale")$season)
for (p in to_load_b) {
  df <- standardize(read_one_season(p, "b"))
  dbWithTransaction(con, {
    dbExecute(con, "DELETE FROM raw_presale WHERE season = $1", params = list(p))
    dbAppendTable(con, "raw_presale", as.data.frame(df))
  })
  message(">> [寫入 預售屋契約] ", p, "：", nrow(df), " 筆")
}

# 滾動視窗：移除不在最近 N 季內的舊資料
for (tbl in c("raw_trade", "raw_presale")) {
  old <- setdiff(dbGetQuery(con, paste("SELECT DISTINCT season FROM", tbl))$season, periods)
  for (p in old) {
    dbExecute(con, paste("DELETE FROM", tbl, "WHERE season = $1"), params = list(p))
    message(">> [移除舊季] ", tbl, " ", p)
  }
}

# --- 5. Transform：清洗成黃金母表 real_estate_clean ---------------------------
# 【民國年轉換規則】(依實際資料長相整理，見檔尾的檢查結果)
#   交易年月日：7 碼 yyyMMdd (99.99%)；6 碼 yyMMdd (民國 100 年前)；其他 → NULL
#   建築完成年月：先取「開頭連續數字」(處理 "05910-"、"066  25" 這類髒值)，再：
#     - 開頭是 0 或 1 且長度 >= 3 → 前 3 碼是民國年 ("0870312"、"087129"、"06701"、"1060602")
#     - 否則長度 >= 4 → 前 2 碼是民國年 (沒補零的舊格式 "850312")
#     - 長度 1~3 → 整串就是民國年 ("045")
#     - 結果必須介於 1 ~ 今年民國年，否則 NULL ("000000"、"-450101" 會被擋掉)
sql_clean <- "
DROP TABLE IF EXISTS real_estate_clean CASCADE;
CREATE TABLE real_estate_clean AS
WITH parsed AS (
  SELECT r.*,
    regexp_replace(TRIM(trans_date), '[^0-9]', '', 'g') AS td,
    substring(TRIM(built_date) FROM '^[0-9]+')          AS bd
  FROM raw_trade r
),
years AS (
  SELECT p.*,
    CASE WHEN length(td) = 7 THEN substr(td, 1, 3)::INT
         WHEN length(td) = 6 THEN substr(td, 1, 2)::INT END AS t_roc,
    CASE WHEN length(td) = 7 THEN substr(td, 4, 2)::INT
         WHEN length(td) = 6 THEN substr(td, 3, 2)::INT END AS t_mon,
    CASE WHEN bd IS NULL OR bd = '' THEN NULL
         WHEN length(bd) >= 3 AND left(bd, 1) IN ('0', '1') THEN substr(bd, 1, 3)::INT
         WHEN length(bd) >= 4 THEN substr(bd, 1, 2)::INT
         ELSE bd::INT END AS b_roc
  FROM parsed p
),
typed AS (
  SELECT y.*,
    CASE WHEN t_roc BETWEEN 1 AND __CUR_ROC__ THEN t_roc + 1911 END AS t_year,
    CASE WHEN t_mon BETWEEN 1 AND 12 THEN t_mon END                AS t_month,
    CASE WHEN b_roc BETWEEN 1 AND __CUR_ROC__ THEN b_roc + 1911 END AS b_year,
    NULLIF(price, '')::NUMERIC           AS price_n,
    NULLIF(unit_price, '')::NUMERIC      AS unit_price_n,
    NULLIF(building_area, '')::NUMERIC   AS building_area_n,
    NULLIF(land_area, '')::NUMERIC       AS land_area_n,
    NULLIF(main_building_area, '')::NUMERIC AS main_area_n,
    NULLIF(berth_area, '')::NUMERIC      AS berth_area_n,
    NULLIF(berth_price, '')::NUMERIC     AS berth_price_n
  FROM years y
  WHERE price ~ '^[0-9]+$'                 -- 總價必須是純數字
    AND (unit_price IS NULL OR unit_price ~ '^[0-9.]*$')
    AND (building_area IS NULL OR building_area ~ '^[0-9.]*$')
    AND (land_area IS NULL OR land_area ~ '^[0-9.]*$')
    AND (main_building_area IS NULL OR main_building_area ~ '^[0-9.]*$')
    AND (berth_area IS NULL OR berth_area ~ '^[0-9.]*$')
    AND (berth_price IS NULL OR berth_price ~ '^[0-9.]*$')
)
SELECT
  serial_number, season, city, district,
  city || district AS city_district,        -- 給 App 用的唯一行政區名稱
  -- 【核心分類標籤】
  CASE
    WHEN trans_type = '車位' THEN '純車位'
    WHEN trans_type = '土地' OR COALESCE(building_area_n, 0) = 0 THEN '純土地'
    -- 預售屋蓋好後過戶的紀錄 (簽約日早於完工日)。真正的「預售契約」在 b 檔 → fact_presale_contract
    WHEN t_year < b_year OR note LIKE '%預售屋%' THEN '預售交屋'
    WHEN b_year IS NULL THEN '年份不明成屋'
    ELSE '房地產成屋'
  END AS trading_type,
  -- 【特殊交易標籤】這些交易價格不代表市場行情，訓練模型時要排除
  CASE WHEN note ~ '親友|員工|共有人|持分|借名|急買|急賣|特殊關係|債權債務|瑕疵|凶宅|事故|毛胚|含增建|法拍'
       THEN '是' ELSE '否' END AS is_special_deal,
  trans_type AS original_trans_type,
  address, address_norm, addr_key, zoning, main_use, building_state,
  CASE
    WHEN building_state LIKE '%透天%'     THEN '透天厝'
    WHEN building_state LIKE '%住宅大樓%' THEN '電梯大樓'
    WHEN building_state LIKE '%華廈%'     THEN '華廈'
    WHEN building_state LIKE '%公寓%'     THEN '公寓'
    WHEN building_state LIKE '%套房%'     THEN '套房'
    WHEN building_state LIKE '%店面%'     THEN '店面'
    WHEN building_state LIKE '%辦公%'     THEN '辦公商業'
    WHEN building_state LIKE '%農舍%'     THEN '農舍'
    WHEN building_state ~ '廠辦|工廠|倉庫'   THEN '廠辦工廠'
    ELSE '其他'
  END AS b_category,
  price_n AS total_price,
  ROUND(price_n / 10000.0, 1)            AS total_price_ten_thousand,
  ROUND(unit_price_n / 0.3025, 0)        AS unit_price_ping,   -- 元/㎡ ÷ 0.3025 = 元/坪
  ROUND(building_area_n * 0.3025, 2)     AS total_ping,         -- ㎡ × 0.3025 = 坪
  ROUND(main_area_n * 0.3025, 2)         AS main_ping,
  ROUND(land_area_n * 0.3025, 2)         AS land_ping,
  berth_type AS parking_type,
  berth_price_n AS parking_price,
  ROUND(berth_area_n * 0.3025, 2)        AS parking_ping,
  CASE WHEN COALESCE(berth_area_n, 0) > 0 OR berth_type IS NOT NULL THEN '是' ELSE '否' END AS include_parking,
  NULLIF(regexp_replace(room_count, '[^0-9]', '', 'g'), '')::INT AS rooms,
  NULLIF(regexp_replace(hall_count, '[^0-9]', '', 'g'), '')::INT AS halls,
  NULLIF(regexp_replace(bath_count, '[^0-9]', '', 'g'), '')::INT AS baths,
  has_management, has_elevator,
  floor_level, total_floors, floor_num, total_floors_num,
  t_year AS trans_year, t_month AS trans_month,
  CASE WHEN t_year IS NOT NULL AND t_month IS NOT NULL THEN make_date(t_year, t_month, 1) END AS trans_ym,
  b_year AS built_year,
  CASE WHEN t_year IS NOT NULL AND b_year IS NOT NULL THEN GREATEST(t_year - b_year, 0) END AS house_age_at_trans,
  CASE WHEN b_year IS NOT NULL THEN __CUR_YEAR__ - b_year END AS house_age_now,
  note
FROM typed
WHERE district IS NOT NULL AND district <> ''
  AND t_year IS NOT NULL
  AND t_year >= 1970;
"
sql_clean <- gsub("__CUR_ROC__", current_roc, sql_clean)
sql_clean <- gsub("__CUR_YEAR__", current_roc + 1911, sql_clean)

# dbExecute 一次只能跑一個 SQL 指令，所以用分號切開
walk(strsplit(sql_clean, ";\\s*\n")[[1]] %>% trimws() %>% discard(~ .x == ""),
     ~ dbExecute(con, .x))
message("✅ real_estate_clean 建立完成")

# 同一個 serial_number 出現在兩季 (政府更正資料) → 只留最新一季
dbExecute(con, "
  DELETE FROM real_estate_clean a
  USING real_estate_clean b
  WHERE a.serial_number = b.serial_number AND a.season < b.season;")

# --- 6. 主題子表 --------------------------------------------------------------
dbExecute(con, "DROP TABLE IF EXISTS fact_residential_house;")
dbExecute(con, "
  CREATE TABLE fact_residential_house AS
  SELECT *,
    -- 純房屋總價 (扣掉車位價) → 給總價模型用
    CASE WHEN include_parking = '是' AND parking_price > 0
         THEN total_price - parking_price ELSE total_price END AS pure_house_total_price,
    -- 純房屋坪數 (扣掉車位坪數)
    CASE WHEN include_parking = '是' AND parking_ping > 0
         THEN total_ping - parking_ping ELSE total_ping END AS pure_house_ping,
    -- 扣車位的純單價 (舊版這裡寫成 'Tier_1'，條件永遠不成立)
    CASE WHEN include_parking = '是' AND parking_price > 0 AND (total_ping - COALESCE(parking_ping, 0)) > 0
         THEN ROUND((total_price - parking_price) / (total_ping - COALESCE(parking_ping, 0)), 0)
         ELSE unit_price_ping END AS pure_house_unit_price_ping,
    -- 車位有登記但沒有拆價：單價被車位坪數稀釋，模型要知道這件事
    (include_parking = '是' AND COALESCE(parking_price, 0) = 0) AS parking_price_unknown
  FROM real_estate_clean
  WHERE trading_type = '房地產成屋'
    AND is_special_deal = '否'
    AND total_ping > 0
    AND unit_price_ping > 10000;")

# 預售交屋 (a 檔)：預售屋完工後的過戶紀錄。價格是「當初簽約的價格」，屋齡幾乎都是 0
dbExecute(con, "DROP TABLE IF EXISTS fact_pre_sale;")              # 舊名稱，改名後移除
dbExecute(con, "DROP TABLE IF EXISTS fact_presale_delivered;")
dbExecute(con, "CREATE TABLE fact_presale_delivered AS SELECT * FROM real_estate_clean WHERE trading_type = '預售交屋';")
# 純土地：地址是「XX段XX地號」，沒有門牌 → 地圖上只能做到「行政區」層級
dbExecute(con, "DROP TABLE IF EXISTS fact_land_only;")
dbExecute(con, "CREATE TABLE fact_land_only AS SELECT * FROM real_estate_clean WHERE trading_type = '純土地';")
# 純車位：單獨買賣的車位
dbExecute(con, "DROP TABLE IF EXISTS fact_parking_only;")
dbExecute(con, "CREATE TABLE fact_parking_only AS SELECT * FROM real_estate_clean WHERE trading_type = '純車位';")

# 預售屋契約 (b 檔)：和 real_estate_clean 的清洗規則一樣，另外多了建案名稱、棟及號、解約情形
sql_presale <- "
DROP TABLE IF EXISTS fact_presale_contract;
CREATE TABLE fact_presale_contract AS
WITH typed AS (
  SELECT r.*,
    CASE WHEN length(td) = 7 AND substr(td, 1, 3)::INT BETWEEN 1 AND __CUR_ROC__ THEN substr(td, 1, 3)::INT + 1911 END AS t_year,
    CASE WHEN length(td) = 7 AND substr(td, 4, 2)::INT BETWEEN 1 AND 12 THEN substr(td, 4, 2)::INT END AS t_month,
    NULLIF(price, '')::NUMERIC         AS price_n,
    NULLIF(unit_price, '')::NUMERIC    AS unit_price_n,
    NULLIF(building_area, '')::NUMERIC AS building_area_n,
    NULLIF(land_area, '')::NUMERIC     AS land_area_n,
    NULLIF(berth_area, '')::NUMERIC    AS berth_area_n,
    NULLIF(berth_price, '')::NUMERIC   AS berth_price_n
  FROM (SELECT *, regexp_replace(TRIM(trans_date), '[^0-9]', '', 'g') AS td FROM raw_presale) r
  WHERE price ~ '^[0-9]+$'
    AND (unit_price IS NULL OR unit_price ~ '^[0-9.]*$')
    AND (building_area IS NULL OR building_area ~ '^[0-9.]*$')
    AND (land_area IS NULL OR land_area ~ '^[0-9.]*$')
    AND (berth_area IS NULL OR berth_area ~ '^[0-9.]*$')
    AND (berth_price IS NULL OR berth_price ~ '^[0-9.]*$')
)
SELECT
  serial_number, season, city, district, city || district AS city_district,
  build_case, building_no, address, address_norm, addr_key, trans_type AS original_trans_type,
  building_state,
  CASE
    WHEN building_state LIKE '%透天%'     THEN '透天厝'
    WHEN building_state LIKE '%住宅大樓%' THEN '電梯大樓'
    WHEN building_state LIKE '%華廈%'     THEN '華廈'
    WHEN building_state LIKE '%公寓%'     THEN '公寓'
    WHEN building_state LIKE '%套房%'     THEN '套房'
    WHEN building_state LIKE '%店面%'     THEN '店面'
    WHEN building_state LIKE '%辦公%'     THEN '辦公商業'
    WHEN building_state LIKE '%農舍%'     THEN '農舍'
    WHEN building_state ~ '廠辦|工廠|倉庫'   THEN '廠辦工廠'
    ELSE '其他'
  END AS b_category,
  -- 解約：有填「解約情形」就代表這份契約後來解約了 (炒作預售屋的指標之一)
  (NULLIF(TRIM(termination), '') IS NOT NULL) AS is_cancelled, termination,
  CASE WHEN note ~ '親友|員工|共有人|持分|借名|特殊關係|債權債務' THEN '是' ELSE '否' END AS is_special_deal,
  price_n AS total_price,
  ROUND(building_area_n * 0.3025, 2) AS total_ping,
  ROUND(land_area_n * 0.3025, 2)     AS land_ping,
  berth_type AS parking_type, berth_price_n AS parking_price, ROUND(berth_area_n * 0.3025, 2) AS parking_ping,
  -- 扣掉車位的純房屋單價 (元/坪)；車位沒拆價時只能用原始單價
  CASE WHEN berth_price_n > 0 AND building_area_n - COALESCE(berth_area_n, 0) > 0
       THEN ROUND((price_n - berth_price_n) / ((building_area_n - COALESCE(berth_area_n, 0)) * 0.3025), 0)
       ELSE ROUND(unit_price_n / 0.3025, 0) END AS pure_house_unit_price_ping,
  CASE WHEN berth_price_n > 0 THEN ROUND((building_area_n - COALESCE(berth_area_n, 0)) * 0.3025, 2)
       ELSE ROUND(building_area_n * 0.3025, 2) END AS pure_house_ping,
  NULLIF(regexp_replace(room_count, '[^0-9]', '', 'g'), '')::INT AS rooms,
  NULLIF(regexp_replace(hall_count, '[^0-9]', '', 'g'), '')::INT AS halls,
  NULLIF(regexp_replace(bath_count, '[^0-9]', '', 'g'), '')::INT AS baths,
  floor_level, floor_num, total_floors_num,
  t_year AS trans_year, t_month AS trans_month,
  CASE WHEN t_year IS NOT NULL AND t_month IS NOT NULL THEN make_date(t_year, t_month, 1) END AS trans_ym,
  note
FROM typed
WHERE district IS NOT NULL AND district <> '' AND t_year IS NOT NULL;
"
sql_presale <- gsub("__CUR_ROC__", current_roc, sql_presale)
walk(strsplit(sql_presale, ";\\s*\n")[[1]] %>% trimws() %>% discard(~ .x == ""), ~ dbExecute(con, .x))
dbExecute(con, "
  DELETE FROM fact_presale_contract a USING fact_presale_contract b
  WHERE a.serial_number = b.serial_number AND a.season < b.season;")

# --- 7. 索引 ------------------------------------------------------------------
# 【資料庫小教室】索引 (INDEX) 就像書的目錄：查詢 WHERE city = '高雄市' 時，
# 不用從第一筆掃到最後一筆，直接跳到對的位置。代價是寫入變慢、多佔空間，
# 所以只替「常被拿來篩選或 JOIN 的欄位」建。複合索引 (city, district) 的順序很重要：
# 它能加速「WHERE city=...」和「WHERE city=... AND district=...」，但不能加速只查 district。
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_res_city_dist ON fact_residential_house(city, district);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_res_trans_year ON fact_residential_house(trans_year);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_res_category  ON fact_residential_house(b_category);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_res_addrkey   ON fact_residential_house(addr_key);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_presale_city_dist ON fact_presale_contract(city, district);")
dbExecute(con, "ANALYZE fact_residential_house;")
dbExecute(con, "ANALYZE fact_presale_contract;")      # 更新統計資訊，讓查詢規劃器選對索引

# --- 8. 驗證 ------------------------------------------------------------------
print(dbGetQuery(con, "
  SELECT 'raw_trade' AS table_name, COUNT(*) AS n FROM raw_trade
  UNION ALL SELECT 'real_estate_clean', COUNT(*) FROM real_estate_clean
  UNION ALL SELECT '  fact_residential_house', COUNT(*) FROM fact_residential_house
  UNION ALL SELECT '  fact_presale_delivered', COUNT(*) FROM fact_presale_delivered
  UNION ALL SELECT '  fact_land_only', COUNT(*) FROM fact_land_only
  UNION ALL SELECT '  fact_parking_only', COUNT(*) FROM fact_parking_only
  UNION ALL SELECT 'raw_presale', COUNT(*) FROM raw_presale
  UNION ALL SELECT '  fact_presale_contract', COUNT(*) FROM fact_presale_contract;"))

# 年份轉換抽查：每種「建築年月原始長度」各抽幾筆，人工確認轉換正確
print(dbGetQuery(con, "
  SELECT length(r.built_date) AS raw_len, r.built_date, c.built_year, r.trans_date, c.trans_year
  FROM real_estate_clean c JOIN raw_trade r USING (serial_number)
  WHERE r.built_date IS NOT NULL
  ORDER BY random() LIMIT 30;"))

# 同名行政區檢查：應該看到 臺北市中正區、基隆市中正區 分開
print(dbGetQuery(con, "
  SELECT city, district, COUNT(*) FROM fact_residential_house
  WHERE district IN ('中正區', '東區', '北區') GROUP BY 1, 2 ORDER BY 2, 1;"))

dbDisconnect(con)

# ==============================================================================
# 附錄：原始資料檢查結果 (2026-10-27，national_trade_final_history.rds，233 萬筆)
#   trans_date 長度：7 碼 2,333,643 / 6 碼 239 / 5 碼 12
#   trans_date 民國年出現 116、117、119、120、122、190 → 打錯字，已用 <= 今年擋掉
#   built_date 長度：7 碼 171 萬 / NULL 62 萬 (多為純土地) / 5 碼 434 / 3 碼 147 / 6 碼 9
#   built_date 髒值範例："05910-"、"066  25"、"-450101"、"000000"、"---"
#   地址：約 58 萬筆是「XX段XX地號」(純土地)；約 30 萬筆沒寫縣市
# ==============================================================================
