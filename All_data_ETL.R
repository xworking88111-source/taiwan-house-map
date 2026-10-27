# ==========================================
# 專案名稱：台灣實價登錄自動化 ETL 系統
# 功能：自動下載、合併、欄位精簡、匯入 PostgreSQL
# ==========================================
library(glue)
library(httr)
library(tidyverse)
library(DBI)
library(RPostgres)

# --- 1. 環境與路徑設定 ---
base_path <- '/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data'
setwd(base_path)

history_dir <- "raw_history_zips"
temp_unzip_dir <- "temp_unzip_all"
dirs <- c(history_dir, temp_unzip_dir)
walk(dirs, ~if(!dir.exists(.x)) dir.create(.x))

# --- 2. 智慧下載 (下載 + 解壓) ---
# A. 邏輯預判：自動計算民國年份與應檢查的季度
current_year_roc <- as.numeric(format(Sys.Date(), "%Y")) - 1911
current_quarter <- ceiling(as.numeric(format(Sys.Date(), "%m")) / 3)

periods <- expand.grid(s = 1:4, y = 109:current_year_roc) %>%
  mutate(id = paste0(y, "S", s)) %>%
  filter(!(y == current_year_roc & s >= current_quarter)) %>% 
  arrange(y, s) %>%
  pull(id)

message(">> [系統偵測] 目標區間：109S1 至 ", tail(periods, 1))

# B. 實體驗證下載循環
for (p in periods) {
  dest_file <- file.path(history_dir, paste0(p, ".zip"))
  unzip_path <- file.path(temp_unzip_dir, p)
  
  # 第一道防線：檢查硬碟是否已有「健康」的檔案
  if(file.exists(dest_file) && file.info(dest_file)$size >= 40000) {
    message(">> [跳過] ", p, " 檔案已存在且大小正常。")
    
    # 補解壓：防止壓縮檔在但資料夾被誤刪
    if(!dir.exists(unzip_path)) {
      unzip(dest_file, exdir = unzip_path)
      message("   [補做] 已重新解壓 ", p)
    }
    next # 跳過後續下載動作，進入下一個迴圈
  }
  
  # 第二道防線：下載並驗證伺服器端資料
  message(">> [執行] 正在請求下載 ", p, " ...")
  target_url <- paste0("https://plvr.land.moi.gov.tw//DownloadSeason?season=", p, "&type=zip&fileName=lvr_landcsv.zip")
  
  tryCatch({
    res <- GET(target_url, write_disk(dest_file, overwrite = TRUE))
    
    # 驗證下載內容是否為有效 ZIP (政府空包彈通常 < 40KB)
    if (file.info(dest_file)$size < 40000) {
      message("   [提醒] ", p, " 伺服器已建立連結但尚未正式存放資料。")
      file.remove(dest_file) # 刪除無效的小檔案，下次執行才能重抓
    } else {
      unzip(dest_file, exdir = unzip_path)
      message("   [成功] ", p, " 下載並解壓完成。")
      Sys.sleep(runif(1, 2, 4)) # 禮貌性延遲
    }
  }, error = function(e) message("   [錯誤] ", p, " 下載過程中斷。"))
}

# --- 3. 欄位自動化映射 (Schema Mapping) ---
# --- 修正後的 3. 欄位自動化映射 ---
# 讀取樣本檔案的第二列 (英文標題)
target_files <- list.files(temp_unzip_dir, pattern = "_lvr_land_a\\.csv$", 
                           full.names = TRUE, recursive = TRUE)
view(target_files)
if (length(target_files) == 0) stop("關鍵錯誤：找不到 CSV 檔案！")
sample_f <- target_files[1]
# 讀取第二列原始英文 (確保讀取進來時不被 R 自動修改名字)
raw_eng_header <- colnames(read_csv(sample_f, skip = 1, n_max = 0, show_col_types = FALSE))

standardize_names <- function(names_vec) {
  # 建立一個完整的對照表 (左邊是原始英文，右邊是你想要的短英文)
  mapping <- c(
    "The villages and towns urban district" = "district",
    "transaction sign" = "trans_type",
    "land sector position building sector house number plate" = "address",
    "land shifting total area square meter" = "land_area",
    "the use zoning or compiles and checks" = "zoning",
    "the non-metropolis land use district" = "non_metro_district",
    "non-metropolis land use" = "non_metro_use",
    "transaction year month and day" = "trans_date",
    "transaction pen number" = "trans_pen_number",
    "shifting level" = "floor_level",
    "total floor number" = "total_floors",
    "building state" = "building_state",
    "main use" = "main_use",
    "main building materials" = "main_building_materials",
    "construction to complete the years" = "built_date",
    "building shifting total area" = "building_area",
    "Building present situation pattern - room" = "room_count",
    "building present situation pattern - hall" = "hall_count",
    "building present situation pattern - health" = "bath_count",
    "building present situation pattern - compartmented" = "has_compartment",
    "Whether there is manages the organization" = "has_management",
    "total price NTD" = "price",
    "the unit price (NTD / square meter)" = "unit_price",
    "the berth category" = "berth_type",
    "berth shifting total area square meter" = "berth_area",
    "the berth total price NTD" = "berth_price",
    "the note" = "note",
    "serial number" = "serial_number",
    "main building area" = "main_building_area",
    "auxiliary building area" = "aux_building_area",
    "balcony area" = "balcony_area",
    "elevator" = "has_elevator",
    "transaction number" = "trans_number"
  )
  
  # 進行轉換：如果在表內就換掉，不在表內就維持原樣並變小寫
  new_names <- mapping[names_vec]
  new_names[is.na(new_names)] <- tolower(make.names(names_vec[is.na(new_names)]))
  
  return(unname(new_names))
}

final_eng_header <- standardize_names(raw_eng_header)

# 執行這個診斷指令
diagnostic_df <- data.frame(
  Original = raw_eng_header,
  After_Standardize = final_eng_header
)

# 顯示對照表
print(diagnostic_df)


# --- 4. 高效合併 ---
all_trade_data <- map_df(target_files, function(f) {
  tryCatch({
    # skip = 3: 跳過前三列標頭
    # col_names = final_eng_header: 強制指定我們洗乾淨的英文欄位名
    read_csv(f, skip = 2, col_names = final_eng_header, 
             col_types = cols(.default = "c"), 
             locale = locale(encoding = "UTF-8"), 
             show_col_types = FALSE)
  }, error = function(e) return(NULL))
})

# 存檔備份
saveRDS(all_trade_data, "national_trade_final_history.rds")
message("✅ 合併完成！總筆數：", nrow(all_trade_data))
all_trade_data<-readRDS("national_trade_final_history.rds")

# --- 5. 資料庫匯入 (PostgreSQL) ---
con <- dbConnect(RPostgres::Postgres(), dbname = 'postgres', 
                 host = 'localhost', port = 5432, user = 'dzzz')
on.exit(dbDisconnect(con)) # 安全機制

# 寫入資料庫
tryCatch({
  dbRemoveTable(con, "real_estate_final")
})
dbRemoveTable(con, "real_estate_final")
dbWriteTable(con, "real_estate_final", 
             as.data.frame(all_trade_data), 
             overwrite = TRUE, row.names = FALSE)

# --- 6. 驗證與測試 ---
start_time <- Sys.time()
test_query <- dbGetQuery(con, 
                         "SELECT * FROM real_estate_final WHERE address LIKE '%高雄市三民區%' LIMIT 10000")
end_time <- Sys.time()

cat("\n--- 系統整合測試成功 ---\n")
cat("查詢耗時：", end_time - start_time, "\n")
cat("抓取筆數：", nrow(test_query), "\n")
print(head(test_query, 2))

# 要刪除的話
# dbRemoveTable(con, "real_estate_final")

# 2. 定義 SQL 指令 (使用 glue 的語法)
# ==============================================================================
# 專案階段：ETL - Transform (T) 階段 [完全體修正版]
# 修正重點：補回車位類別、車位面積、車位價格，優化預測模型基礎欄位
# ==============================================================================

current_year <- as.numeric(format(Sys.Date(), "%Y"))

# --- 【第一步】撰寫嚴謹的黃金母表 SQL (補足車位欄位) ---
sql_master_v2 <- glue("
CREATE TABLE real_estate_clean AS
WITH base_parts AS (
    SELECT 
        *,
        -- 1. 交易年份與月份轉換 (加上 TRIM 去除隱形空格，並提升容錯)
        CASE 
            WHEN TRIM(trans_date) ~ '^\\d{{7}}$' THEN CAST(SUBSTRING(TRIM(trans_date) FROM 1 FOR 3) AS INTEGER) + 1911
            WHEN TRIM(trans_date) ~ '^\\d{{6}}$' THEN CAST(SUBSTRING(TRIM(trans_date) FROM 1 FOR 2) AS INTEGER) + 1911
            -- 容錯機制：如果前面漏空格變 5 位數 (例如 90723 -> 民國90年)
            WHEN LENGTH(TRIM(trans_date)) = 5 AND TRIM(trans_date) ~ '^\\d+$' THEN CAST(SUBSTRING(TRIM(trans_date) FROM 1 FOR 1) AS INTEGER) + 1911
            ELSE NULL 
        END AS t_year,
        
        CASE 
            WHEN TRIM(trans_date) ~ '^\\d{{7}}$' THEN CAST(SUBSTRING(TRIM(trans_date) FROM 4 FOR 2) AS INTEGER)
            WHEN TRIM(trans_date) ~ '^\\d{{6}}$' THEN CAST(SUBSTRING(TRIM(trans_date) FROM 3 FOR 2) AS INTEGER)
            WHEN LENGTH(TRIM(trans_date)) = 5 AND TRIM(trans_date) ~ '^\\d+$' THEN CAST(SUBSTRING(TRIM(trans_date) FROM 2 FOR 2) AS INTEGER)
            ELSE NULL 
        END AS t_month,
        
        -- 2. 建築年份轉換
        CASE 
            WHEN built_date IS NULL OR built_date IN ('', '0', '0000000') OR built_date !~ '^\\d+$' THEN NULL
            ELSE (
                CASE 
                    WHEN LENGTH(built_date::TEXT) <= 3 THEN CAST(built_date AS INTEGER) + 1911
                    WHEN LENGTH(built_date::TEXT) BETWEEN 4 AND 5 THEN CAST(LEFT(built_date::TEXT, LENGTH(built_date::TEXT) - 2) AS INTEGER) + 1911
                    WHEN LENGTH(built_date::TEXT) >= 6 THEN CAST(LEFT(built_date::TEXT, LENGTH(built_date::TEXT) - 4) AS INTEGER) + 1911
                    ELSE NULL
                END
            )
        END AS b_year_raw
    FROM real_estate_final
),
final_parts AS (
    SELECT 
        *,
        CASE 
            WHEN b_year_raw > {current_year} OR b_year_raw < 1900 THEN NULL 
            ELSE b_year_raw 
        END AS b_year
    FROM base_parts
)
SELECT 
    -- 【核心分類標籤】
    CASE 
        WHEN trans_type = '車位' THEN '純車位'
        WHEN (CAST(building_area AS NUMERIC) = 0 OR trans_type LIKE '%土地%') AND trans_type NOT LIKE '%建物%' THEN '純土地'
        WHEN (t_year < b_year) OR (note LIKE '%預售屋%') THEN '預售屋'
        WHEN b_year IS NULL AND trans_type LIKE '%建物%' THEN '年份不明成屋'
        ELSE '房地產成屋'
    END AS trading_type,

    -- 【特殊交易標籤】
    CASE 
        WHEN (note LIKE '%親友%' OR note LIKE '%員工%' OR note LIKE '%共有人%' OR note LIKE '%持分%' OR note LIKE '%借名%' OR note LIKE '%急買%') THEN '是'
        ELSE '否'
    END AS is_special_deal,

    -- 原始基本欄位
    trans_type AS original_trans_type,
    district, address, zoning, main_use, building_state,
    CASE 
      WHEN building_state LIKE '%透天%' THEN '透天厝'
      WHEN building_state LIKE '%住宅大樓%' THEN '電梯大樓'
      WHEN building_state LIKE '%華廈%' THEN '華廈'
      WHEN building_state LIKE '%公寓%' THEN '公寓'
      ELSE '其他'
    END AS b_category,

    -- 數值型態強制轉換與坪數換算 (保留原始總價，新增萬元欄位方便 Shiny 畫圖)
    CAST(price AS NUMERIC) AS total_price,
    ROUND(CAST(price AS NUMERIC) / 10000.0, 1) AS total_price_ten_thousand,
    ROUND(CAST(unit_price AS NUMERIC) / 0.3025, 0) AS unit_price_ping,
    ROUND(CAST(building_area AS NUMERIC) * 0.3025, 2) AS total_ping,
    ROUND(CAST(main_building_area AS NUMERIC) * 0.3025, 2) AS main_ping,
    ROUND(CAST(land_area AS NUMERIC) * 0.3025, 2) AS land_ping,
    
    -- 【補回與優化：車位相關細節欄位】
    berth_type AS parking_type,
    CAST(berth_price AS NUMERIC) AS parking_price,
    ROUND(CAST(berth_area AS NUMERIC) * 0.3025, 2) AS parking_ping,
    CASE WHEN CAST(berth_area AS NUMERIC) > 0 OR berth_type IS NOT NULL THEN '是' ELSE '否' END AS include_parking,
    
    -- 房屋格局
    CAST(room_count AS INTEGER) AS rooms,
    CAST(hall_count AS INTEGER) AS halls,
    CAST(bath_count AS INTEGER) AS baths,
    has_management, has_elevator, floor_level, total_floors,
    
    -- 年份與屋齡
    t_year AS trans_year, t_month AS trans_month, b_year AS built_year,
    CASE WHEN t_year IS NOT NULL AND b_year IS NOT NULL THEN GREATEST(t_year - b_year, 0) ELSE NULL END AS house_age_at_trans,
    CASE WHEN b_year IS NOT NULL THEN {current_year} - b_year ELSE NULL END AS house_age_now,
    note
FROM final_parts
WHERE district IS NOT NULL AND district <> '' 
  AND price ~ '^\\d+$'
  -- 終極封殺：如果交易年份是 00001、40408 等無法解析的髒資料，直接剔除，不進入母表
  AND t_year IS NOT NULL 
  AND t_year >= 1970 AND t_year <= {current_year}
")

# --- 執行母表更新 ---
tryCatch({
  dbExecute(con, "DROP TABLE IF EXISTS real_estate_clean CASCADE;")
  dbExecute(con, sql_master_v2)
  message("✅ [修正步一成功] 黃金母表(補足車位欄位版)建立完成！")
}, error = function(e) message("❌ 修正步一失敗：", e$message))

# --- 【第二步】重新建立三大子表 ---
tryCatch({
  dbExecute(con, "DROP TABLE IF EXISTS fact_residential_house;")
  dbExecute(con, "
    CREATE TABLE fact_residential_house AS 
    SELECT 
        *,
        -- 1. 計算純房屋總價 (專供 AI 預測總價模型使用，排除車位價格干擾)
        CASE 
            WHEN include_parking = '是' AND parking_price > 0 THEN total_price - parking_price
            ELSE total_price
        END AS pure_house_total_price,
        
        -- 2. 計算純房屋坪數 (排除車位坪數)
        CASE 
            WHEN include_parking = '是' AND parking_ping > 0 THEN total_ping - parking_ping
            ELSE total_ping
        END AS pure_house_ping,

        -- 3. 計算最精準的「扣車位純房價每坪單價」
        CASE 
            -- 情況 A：有車位且有獨立車位價，用我們算出來的純房價與純坪數相除
            WHEN include_parking = 'Tier_1' AND parking_price > 0 AND (total_ping - parking_ping) > 0 
                THEN ROUND((total_price - parking_price) / (total_ping - parking_ping), 0)
            -- 情況 B：有車位但車位價登記為 0 (如實證一)，此時原始單價 unit_price_ping 其實已經被污染，但這是唯一的線索
            ELSE unit_price_ping
        END AS pure_house_unit_price_ping
        
    FROM real_estate_clean 
    WHERE trading_type = '房地產成屋' 
      AND is_special_deal = '否' 
      AND total_ping > 0 
      AND unit_price_ping > 10000;
  ")
  
  dbExecute(con, "DROP TABLE IF EXISTS fact_pre_sale;")
  dbExecute(con, "
    CREATE TABLE fact_pre_sale AS 
    SELECT * FROM real_estate_clean 
    WHERE trading_type = '預售屋';
  ")
  
  dbExecute(con, "DROP TABLE IF EXISTS fact_land_only;")
  dbExecute(con, "
    CREATE TABLE fact_land_only AS 
    SELECT * FROM real_estate_clean 
    WHERE trading_type = '純土地';
  ")
  
  message("✅ [修正步二成功] 三大子表連帶更新完成！")
}, error = function(e) message("❌ 修正步二失敗：", e$message))

# --- 【驗證程式碼】立刻審視資料庫分流結果 ---
verify_summary <- dbGetQuery(con, "
  SELECT '1. 原始總表 (real_estate_final)' AS table_name, COUNT(*) AS rows_count FROM real_estate_final
  UNION ALL
  SELECT '2. 黃金母表 (real_estate_clean)', COUNT(*) FROM real_estate_clean
  UNION ALL
  SELECT '   └─ 子表：房地產成屋 (fact_residential_house)', COUNT(*) FROM fact_residential_house
  UNION ALL
  SELECT '   └─ 子表：預售屋 (fact_pre_sale)', COUNT(*) FROM fact_pre_sale
  UNION ALL
  SELECT '   └─ 子表：純土地 (fact_land_only)', COUNT(*) FROM fact_land_only;
")

print("📊 === 數據分流與清洗結果驗證面板 ===")
print(verify_summary)

# 驗證機制：深度年份轉換對比抽查
# ==============================================================================

# 為了同時看到原始 built_date，我們必須把子表跟原始總表 join 起來對比
# 修正後驗證機制：深度年份轉換對比抽查（修正欄位為全英文）
# ==============================================================================

year_check_query_fixed <- "
SELECT 
    f.district, 
    f.address, 
    o.built_date AS raw_built_date,     -- 原始總表裡的原始民國字串
    f.built_year AS clean_built_year,   -- 母表洗出來的西元整數年份
    o.trans_date AS raw_trans_date,
    f.house_age_now,
    f.trans_year,
    f.house_age_at_trans,
    f.trading_type
FROM fact_residential_house f
JOIN real_estate_final o ON f.address = o.address AND f.district = o.district
WHERE o.built_date IS NOT NULL AND o.built_date <> ''
ORDER BY LENGTH(o.built_date::TEXT) DESC, f.house_age_at_trans DESC
LIMIT 200;
"

preview_years_fixed <- dbGetQuery(con, year_check_query_fixed)
print("🧐 === 20筆年份轉換深度對比面板 (修正欄位版) ===")
print(preview_years_fixed)

# 檢視資料庫目前有哪些 Table
current_tables <- dbGetQuery(con, "
  SELECT table_name 
  FROM information_schema.tables 
  WHERE table_schema = 'public' 
    AND table_type = 'BASE TABLE';
")

print("🔍 === 目前資料庫存在的所有資料表 ===")
print(current_tables)

# 檢視目前資料庫中所有的索引 (INDEX)
index_list <- dbGetQuery(con, "
  SELECT 
    tablename AS table_name,
    indexname AS index_name,
    indexdef AS index_definition
  FROM pg_indexes
  WHERE schemaname = 'public'
  ORDER BY tablename, indexname;
")

print("⚡ === 目前資料庫存在的所有索引 (INDEX) 清單 ===")
print(index_list)

# ==============================================================================
# 【新階段】為全新的主題子表建立優化索引 (INDEX)
# ==============================================================================
tryCatch({
  # 1. 針對 Shiny App 最常篩選的「行政區 (district)」建立索引 (未來地圖選區、區域統計秒讀的關鍵)
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_res_district ON fact_residential_house(district);")
  
  # 2. 針對地圖與趨勢圖表最常篩選的「交易年份 (trans_year)」建立索引
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_res_trans_year ON fact_residential_house(trans_year);")
  
  # 3. 針對 AI 房價預測模型高頻率運算的「純房屋單價」建立索引
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_res_pure_price ON fact_residential_house(pure_house_unit_price_ping);")
  
  message("⚡ [索引優化成功] 已為全新的成屋子表配置高效能 INDEX！")
}, error = function(e) {
  message("❌ 建立新索引失敗：", e$message)
})

# ==============================================================================
# 資料庫大掃除：清除舊時代的實驗資料表
# ==============================================================================

# clear_old_tables <- function() {
#   # 這些是從你的圖片中識別出來、目前新架構已經不需要的舊表名稱
#   tables_to_drop <- c(
#     "real_estate_clean_old", # 如果有之前版本
#     "temp_residential",
#     "test_residential_house",
#     "real_estate",
#     "raw_real_estate",
#     "district_summary",
#     "real_estate_eng"
#   )
#   
#   for(tbl in tables_to_drop) {
#     tryCatch({
#       dbExecute(con, glue("DROP TABLE IF EXISTS {tbl} CASCADE;"))
#       message(paste0("🧹 已成功完全剔除舊表與舊索引: ", tbl))
#     }, error = function(e) {
#       message(paste0("⚠️ 嘗試刪除 ", tbl, " 時跳過（可能已被手動刪除）: ", e$message))
#     })
#   }
# }
# 
# # 執行大掃除
# clear_old_tables()


