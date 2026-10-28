# ==============================================================================
# 匯出 App 用的精簡資料：PostgreSQL (完整資料倉儲) → app/data/*.rds
#
# 為什麼要這一步？
#   免費的 shinyapps.io 連不到你電腦上的 PostgreSQL。
#   所以把 App 需要的欄位「匯出成檔案」，跟著 App 一起上傳。
#   資料庫負責「存全部、做清洗」；App 只拿「展示需要的那一小份」。
#   這在業界叫做「資料集市 (data mart)」或「服務層 (serving layer)」。
#
# 產出：
#   app/data/trades.rds       成屋交易 (最近 5 年，有經緯度)，地圖與圖表用
#   app/data/towns.rds        鄉鎮市區界線 (簡化過的多邊形)，面量圖用
#   app/data/road_lookup.rds  道路代表點，估價頁「選路名 → 經緯度」用
#   app/models/price_model.rds  App 版模型 (XGBoost)
# ==============================================================================
library(tidyverse)
library(DBI)
library(RPostgres)
library(sf)

# 共用設定 (路徑、資料庫連線)：本機和 GitHub Actions 都用同一份程式
source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "config.R"))
setwd(project_path)
dir.create("app/data", recursive = TRUE, showWarnings = FALSE)
dir.create("app/models", recursive = TRUE, showWarnings = FALSE)

con <- db_connect()
dbExecute(con, "SET search_path TO re_v2;")    # 只讀 re_v2

# --- 1. 交易資料 (五種類別合成一張表) ----------------------------------------
# 在 SQL 端就先篩選、先換單位，傳回 R 的資料量越少越快 (把運算推給資料庫 = push-down)
#
# category (交易類別)：
#   成屋       ：一般中古屋/新成屋買賣 (含「建築年份不明」的成屋)
#   預售交屋   ：預售屋蓋好後的過戶紀錄 (a 檔)，價格是當初簽約價
#   預售屋     ：預售屋買賣契約 (b 檔)，有建案名稱、是否解約
#   土地       ：純土地買賣，單價 = 每坪「土地」價格；地址是地號 → 只能畫到行政區
#   車位       ：單獨買賣的車位，看總價 (每個車位多少錢)
# 所有類別都排除了「親友、持分」等特殊交易 (價格不代表市場行情)
# UNION ALL：把幾個 SELECT 的結果「上下接起來」，欄位數量和順序必須一樣
trades <- dbGetQuery(con, "
  WITH homes AS (
    SELECT serial_number AS id,
           CASE WHEN trading_type = '預售交屋' THEN '預售交屋' ELSE '成屋' END AS category,
           city, district, b_category, trans_ym, total_price,
           -- 扣車位單價 (元/坪)，和 fact_residential_house 的算法相同
           CASE WHEN include_parking = '是' AND parking_price > 0 AND total_ping - COALESCE(parking_ping, 0) > 0
                THEN (total_price - parking_price) / (total_ping - COALESCE(parking_ping, 0))
                ELSE unit_price_ping END AS unit_price,
           CASE WHEN include_parking = '是' AND parking_price > 0 THEN total_ping - COALESCE(parking_ping, 0)
                ELSE total_ping END AS ping,
           land_ping, house_age_at_trans AS age, floor_num, total_floors_num, rooms, halls, baths,
           (include_parking = '是') AS parking, (has_elevator = '有') AS elevator,
           address_norm, addr_key, NULL::TEXT AS build_case, FALSE AS cancelled
    FROM real_estate_clean
    WHERE trading_type IN ('房地產成屋', '年份不明成屋', '預售交屋') AND is_special_deal = '否'
  )
  SELECT h.*, g.lng, g.lat, g.geo_level FROM homes h LEFT JOIN geo_cache g USING (addr_key)
  WHERE h.ping BETWEEN 3 AND 500 AND h.unit_price BETWEEN 10000 AND 5000000
    AND h.b_category IN ('透天厝', '電梯大樓', '華廈', '公寓', '套房', '店面', '辦公商業', '農舍', '廠辦工廠')

  UNION ALL
  SELECT p.serial_number, '預售屋', p.city, p.district, p.b_category, p.trans_ym, p.total_price,
         p.pure_house_unit_price_ping, p.pure_house_ping, p.land_ping, NULL, p.floor_num, p.total_floors_num,
         p.rooms, p.halls, p.baths, (p.parking_price > 0 OR p.parking_ping > 0), NULL,
         p.address_norm, p.addr_key, p.build_case, p.is_cancelled, g.lng, g.lat, g.geo_level
  FROM fact_presale_contract p LEFT JOIN geo_cache g USING (addr_key)
  WHERE p.is_special_deal = '否' AND p.pure_house_ping BETWEEN 3 AND 500
    AND p.pure_house_unit_price_ping BETWEEN 10000 AND 5000000
    AND p.b_category IN ('透天厝', '電梯大樓', '華廈', '公寓', '套房', '店面', '辦公商業', '農舍', '廠辦工廠')

  UNION ALL
  SELECT serial_number, '土地', city, district, '土地', trans_ym, total_price,
         unit_price_ping, land_ping, land_ping, NULL, NULL, NULL, NULL, NULL, NULL, FALSE, NULL,
         address_norm, NULL, NULL, FALSE, NULL, NULL, 'district'
  FROM fact_land_only
  WHERE is_special_deal = '否' AND land_ping > 0 AND unit_price_ping > 0

  UNION ALL
  SELECT serial_number, '車位', city, district, '車位', trans_ym, total_price,
         NULL, parking_ping, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE, NULL,
         address_norm, addr_key, NULL, FALSE, NULL, NULL, 'district'
  FROM fact_parking_only
  WHERE is_special_deal = '否' AND total_price > 0;") %>% as_tibble()

trades <- trades %>%
  filter(!is.na(trans_ym), trans_ym >= seq(Sys.Date(), length.out = 2, by = "-5 years")[2]) %>%
  mutate(total_wan = round(total_price / 1e4, 1),       # 總價 (萬)
         unit_wan  = round(unit_price / 1e4, 2),        # 單價 (萬/坪)
         ping = round(ping, 2), land_ping = round(land_ping, 2)) %>%
  select(-total_price, -unit_price) %>%
  rename(address = address_norm)

# 新竹市、嘉義市的原始資料「沒有分區」：鄉鎮市區欄直接寫「新竹市」，地址寫成「新竹市新竹市大學路…」。
# 但地理編碼已經算出經緯度 → 用「點落在哪個行政區多邊形裡」(point-in-polygon，空間連接) 反推出東區/北區/香山區。
# 土地、車位沒有經緯度，只能維持原本的「新竹市」。
towns_full <- st_read("geo_data/towns.json", layer = "towns", quiet = TRUE) %>%
  st_set_crs(4326) %>%
  transmute(city_t = gsub("台", "臺", COUNTYNAME), district_t = gsub("台", "臺", TOWNNAME)) %>%
  st_make_valid()
valid_pairs <- paste(towns_full$city_t, towns_full$district_t)
fix <- which(!paste(trades$city, trades$district) %in% valid_pairs & !is.na(trades$lng))
if (length(fix) > 0) {
  pts <- st_as_sf(trades[fix, c("lng", "lat")], coords = c("lng", "lat"), crs = 4326)
  hit <- st_join(pts, towns_full, join = st_within)
  hit <- hit[!duplicated(rep(seq_len(nrow(pts)), length.out = nrow(hit))), ]   # 邊界上的點只取一個區
  ok  <- !is.na(hit$district_t) & hit$city_t == trades$city[fix]
  trades$district[fix[ok]] <- hit$district_t[ok]
  message("用經緯度補回行政區：", sum(ok), " 筆 (", paste(unique(trades$city[fix[ok]]), collapse = "、"), ")")
}

# 同一條巷子的「不同建物」共用同一個代表點 → 地圖上會疊在一起。
# 依 addr_key (一棟建物一個 key) 加上固定的小抖動 (約 ±30 公尺)，讓不同建物散開；
# 同一棟建物的多筆交易仍然在同一個點 → App 上一棟建物 = 一個標記。
# 這不會讓位置「變準」，只是讓顯示比較清楚；精準度仍以 geo_level 為準。
# ⚠️ 抖動只能用在「地圖顯示」：估價要用真實座標 (glng / glat)，否則到捷運、學校的距離會和訓練時差幾十公尺
#    (tests/serving_consistency.R 抓到的 training-serving skew：估價曾經用了抖動後的座標)
trades <- trades %>% mutate(glng = lng, glat = lat)
set.seed(2026)
keys <- unique(na.omit(trades$addr_key))
jit  <- tibble(addr_key = keys, dx = runif(length(keys), -3e-4, 3e-4), dy = runif(length(keys), -3e-4, 3e-4))
trades <- trades %>%
  left_join(jit, by = "addr_key") %>%
  mutate(lng = lng + coalesce(dx, 0), lat = lat + coalesce(dy, 0)) %>%
  select(-dx, -dy) %>%
  # 文字欄位轉成 factor：同樣的字串只存一次，記憶體省很多 (200 萬筆的「高雄市」只存 1 份)
  mutate(across(c(category, city, district, b_category, geo_level), as.factor),
         trans_ym = as.Date(trans_ym), age = as.integer(age))
# 【記憶體】免費的 shinyapps.io 只有 1GB 記憶體。
# 如果 App 讀一個大檔再自己切成 5 類，啟動那一瞬間「完整版 + 切好的版本」同時存在，會衝到 1.5GB 被強制關閉。
# 所以在這裡 (本機) 就先：
#   1. 拿掉 App 用不到的欄位 (交易編號 id 就占 148MB)
#   2. 依交易類別切成 5 個檔案，App 直接讀，不用自己切
#   3. 需要「全部資料」才算得出來的東西 (縣市清單、土地比例表、各類筆數) 先算好存成 meta.rds
# 土地比例表要用 land_ping，所以在刪欄位之前先算
land_ratio_tbl <- trades %>% filter(category == "成屋", !is.na(land_ping), ping > 0) %>%
  mutate(r = land_ping / ping) %>%
  group_by(city, district, b_category) %>% summarise(r = median(r), n = n(), .groups = "drop") %>%
  mutate(across(c(city, district, b_category), as.character))
#   4. 地址只留「建物門牌」(addr_key，不含樓層) 一個欄位：同一棟的交易共用同一個字串，省記憶體；
#      土地、車位沒有門牌，保留原本的地址文字
trades <- trades %>%
  mutate(address = coalesce(addr_key, address), year = as.integer(format(trans_ym, "%Y"))) %>%
  select(-id, -addr_key, -rooms, -halls, -baths, -elevator, -parking, -land_ping)   # App 用不到的欄位
unlink(list.files("app/data", pattern = "^trades.*\\.rds$", full.names = TRUE))   # 刪掉舊的檔案
#   5. 每個類別只留 App 用得到的欄位：
#      土地、車位：只畫面量圖和統計圖，用不到經緯度、地址 (34 萬筆地號字串很占空間)、屋齡、樓層
#      成屋、預售交屋：沒有建案名稱、解約
cols_drop <- list(
  "土地"     = c("lng", "lat", "glng", "glat", "geo_level", "address", "age", "floor_num", "total_floors_num", "build_case", "cancelled"),
  "車位"     = c("lng", "lat", "glng", "glat", "geo_level", "address", "age", "floor_num", "total_floors_num", "build_case", "cancelled"),
  "成屋"     = c("build_case", "cancelled"),                     # glng / glat：估價用的真實座標 (同棟歷史)
  "預售交屋" = c("build_case", "cancelled"),
  "預售屋"   = c("glng", "glat"))                                # 預售屋不會被拿來當「同棟歷史」，只需要顯示用座標
for (cc in unique(as.character(trades$category))) {
  d <- trades[trades$category == cc, setdiff(names(trades), cols_drop[[cc]])]
  saveRDS(d, file.path("app/data", paste0("trades_", cc, ".rds")), compress = "xz")
}
meta <- list(
  data_months   = range(trades$trans_ym),
  city_levels   = levels(trades$city),
  district_list = lapply(split(as.character(trades$district), as.character(trades$city)), \(x) sort(unique(x))),
  cat_counts    = table(trades$category),
  land_ratio_tbl = land_ratio_tbl
)
saveRDS(meta, "app/data/meta.rds")
message("trades：", nrow(trades), " 筆 → 依類別切成 ", length(unique(trades$category)), " 個檔案")
print(count(trades, category))
dbDisconnect(con)

# --- 2. 鄉鎮界線 (簡化) -------------------------------------------------------
# 原始界線很細緻，直接畫會讓網頁變慢。st_simplify 會把多邊形的頂點減少，
# dTolerance = 80 表示「允許邊界偏移 80 公尺以內」，肉眼幾乎看不出差別，檔案小很多。
towns <- st_read("geo_data/towns.json", layer = "towns", quiet = TRUE) %>%
  st_set_crs(4326) %>%
  transmute(city = gsub("台", "臺", COUNTYNAME), district = gsub("台", "臺", TOWNNAME)) %>%
  st_make_valid() %>%
  st_transform(3826) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 80) %>%
  st_transform(4326)
# 新竹市、嘉義市的土地、車位沒有經緯度、無法分區 (district 就是「新竹市」)
# → 另外加一個「整個市」的多邊形 (把各區合併 = st_union)，面量圖才畫得出來
whole_city <- towns %>% filter(city %in% c("新竹市", "嘉義市")) %>%
  group_by(city) %>% summarise(geometry = st_union(geometry), .groups = "drop") %>%
  mutate(district = city)
towns <- bind_rows(towns, whole_city)
saveRDS(towns, "app/data/towns.rds", compress = "xz")

# --- 3. 道路對照表、模型 ------------------------------------------------------
file.copy("geo_data/road_lookup.rds", "app/data/road_lookup.rds", overwrite = TRUE)
# 模型：兩個 XGBoost 模型各存成 .ubj (XGBoost 官方二進位格式)，App 直接 xgb.load() 讀成模型物件。
# (如果把原始格式包在 .rds 裡，App 載入時「原始格式 + 模型物件」會同時存在，記憶體尖峰多 100MB 以上)
library(xgboost)
m <- readRDS("models/price_model_v2_app.rds")
xgb.save(xgb.load.raw(m$xgb_raw),  "app/models/xgb_point.ubj")
xgb.save(xgb.load.raw(m$xgbq_raw), "app/models/xgb_quant.ubj")
m$xgb_raw <- NULL; m$xgbq_raw <- NULL
saveRDS(m, "app/models/price_model.rds")                         # 只剩特徵清單、類別編號、成績等資訊
# 空間特徵：設施座標 (估價時算距離) + 附近行情表 (估價時查附近 9 格)
file.copy("models/nbr_table.rds", "app/data/nbr_table.rds", overwrite = TRUE)
file.copy("geo_data/facilities.rds", "app/data/facilities.rds", overwrite = TRUE)
file.copy("models/road_class.rds", "app/data/road_class.rds", overwrite = TRUE)    # 臨路等級 (v3)
# 一致性測試的標準答案 (部署前 tests/serving_consistency.R 用；不會上傳到網站，見 deploy_app.R 的清單)
file.copy("models/serving_check.rds", "app/models/serving_check.rds", overwrite = TRUE)

# 最後檢查：App 需要的檔案一個都不能少。
# file.copy() 在來源檔不存在時「不會報錯」(只回傳 FALSE)，如果不檢查，流程會顯示成功、
# 但線上 App 一打開就當掉 → 比失敗更糟。所以少任何一個檔案就直接停止。
required <- c(paste0("app/data/", c("meta.rds", "towns.rds", "road_lookup.rds", "facilities.rds",
                                    "nbr_table.rds", "condition_adjust.rds", "road_class.rds")),
              paste0("app/models/", c("price_model.rds", "xgb_point.ubj", "xgb_quant.ubj", "serving_check.rds")),
              file.path("app/data", paste0("trades_", c("成屋", "預售屋", "預售交屋", "土地", "車位"), ".rds")))
missing <- required[!file.exists(required)]
if (length(missing) > 0) stop("匯出不完整，缺少：", paste(missing, collapse = "、"))
message("✅ 匯出完成，App 需要的 ", length(required), " 個檔案都在")
