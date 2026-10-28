# ==============================================================================
# 模型函式庫：讀資料、清理、特徵工程、模型輸入格式 —— 訓練、回測、實驗、屋況調整「共用同一份」
#
# 為什麼要有這個檔案？(重構 refactoring：功能不變，只改程式的組織方式)
#   舊做法：回測、實驗、屋況調整三支程式用 readLines() 讀訓練程式的「文字」，
#           再用 eval(parse()) 挑出第 1~3 節、或名字叫 prep_x 的那段來執行。
#   問題：  訓練程式改一個章節標題 (例如「# --- 4.」改成「# --- 5.」)，另外三支就默默壞掉；
#           而且 prep_x() 讀的是「全域變數」features、fac_levels，誰在什麼時候改了它很難追。
#   新做法：共用的部分寫成「函數」放在這裡，每支程式 source() 這個檔、呼叫函數。
#           函數需要什麼就從參數傳進去 (feats、levels)，不偷看全域變數 → 行為可預測、可以單獨測試。
#
# 載入：source(file.path(project_path, "R", "model_lib.R"))
#   會一併載入 config.R (路徑、資料庫連線) 和 new_features.R (空間特徵函數)
# ==============================================================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(DBI)
  library(RPostgres)
})
source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "config.R"))
# features.as_library = TRUE：只載入 new_features.R 的函數，不執行它的主程式 (不會重算整個特徵庫)
options(features.as_library = TRUE)
source(file.path(project_path, "new_features.R"))

# --- 1. 讀資料 ----------------------------------------------------------------
# 從資料庫拿「成屋 + 非特殊交易」的子表，並接上經緯度。
# 這段 SQL 就是資料工程 (ETL) 和資料科學 (模型) 的交界：模型只吃乾淨的資料。
load_training_data <- function(schema = "re_v2") {
  con <- db_connect()
  on.exit(dbDisconnect(con))                 # 函數結束時一定會關連線 (就算中途出錯)
  dbExecute(con, paste0("SET search_path TO ", schema, ";"))
  # 設施特徵欄位名稱由 new_features.R 定義 (dist_cols / count_cols)，這裡自動組成 SQL
  fac_sql <- paste0("gf.d_", dist_cols, collapse = ", ")
  cnt_sql <- paste0("gf.n_", count_cols, "_500", collapse = ", ")
  dbGetQuery(con, paste0("
    SELECT f.addr_key, f.address_norm, f.city, f.district, f.b_category, f.trans_ym,
           f.house_age_at_trans, f.pure_house_ping, f.land_ping, f.main_ping,
           f.floor_num, f.total_floors_num, f.rooms, f.halls, f.baths,
           f.has_elevator, f.has_management, f.include_parking, f.parking_price_unknown,
           f.pure_house_unit_price_ping, f.pure_house_total_price,
           g.lng, g.lat, g.geo_level,
           gf.x AS tx, gf.y AS ty,          -- TWD97 公尺座標 (改名避免和目標變數 y 撞名)
           ", fac_sql, ", ", cnt_sql, "
    FROM fact_residential_house f
    JOIN geo_cache g ON f.addr_key = g.addr_key
    JOIN geo_features gf ON f.addr_key = gf.addr_key   -- 設施距離 (特徵庫，見 new_features.R)
    WHERE f.b_category IN ('透天厝', '電梯大樓', '華廈', '公寓', '套房')
      AND f.house_age_at_trans IS NOT NULL
      AND g.geo_level <> 'none'
    -- 固定排序：SQL 沒寫 ORDER BY 時，資料庫回傳的順序「不保證」每次一樣。
    -- 順序一變，XGBoost 隨機抽樣抽到的就是不同的房子 → 就算固定種子，模型也不一樣 (估價跳動的隱藏原因)
    ORDER BY f.season, f.serial_number;")) %>% as_tibble()
}

# --- 2. 清理離群值 ------------------------------------------------------------
# 實價登錄有很多「怪交易」沒寫在備註：例如 1 坪 1 萬的透天 (其實是持分)、
# 或 1 坪 500 萬的豪宅頂樓。這些點會把模型往奇怪的方向拉。
# 做法：在「同縣市 + 同建物型態」裡，砍掉單價最低 1% 和最高 1%。
# 為什麼分組砍？台北的 1% 低價可能是台東的正常價，不能全台一起砍。
base_filter <- function(df_raw) {
  df_raw %>% filter(!is.na(trans_ym),                             # 交易月份無效 (例如 13 月) 的少數資料：
                                                                  # 留著會讓 max() 變成 NA，時間切分整個失效
                    pure_house_ping >= 5, pure_house_ping <= 300, # 5 坪以下多半是車位/持分
                    house_age_at_trans <= 70)
}
# 每個「縣市 × 型態」的單價門檻 (第 1、99 百分位)。
# 獨立成一個函數並存進模型資訊：網站算「同棟歷史」時要用同一組門檻篩選成交，才會和訓練時一致
price_bounds <- function(df_raw) {
  base_filter(df_raw) %>% group_by(city, b_category) %>%
    summarise(lo = quantile(pure_house_unit_price_ping, 0.01), hi = quantile(pure_house_unit_price_ping, 0.99),
              .groups = "drop")
}
clean_trades <- function(df_raw) {
  base_filter(df_raw) %>%
    inner_join(price_bounds(df_raw), by = c("city", "b_category")) %>%
    filter(between(pure_house_unit_price_ping, lo, hi)) %>%
    select(-lo, -hi)
}

# --- 3. 特徵工程 (Feature Engineering) ----------------------------------------
# 「特徵」= 模型看得到的欄位。好的特徵往往比換模型更有效。
engineer_features <- function(df) {
  df %>% mutate(
    # 目標變數取 log：房價是「右偏」分布 (少數極貴)，取 log 後接近常態，
    # 而且模型的誤差會變成「百分比誤差」→ 1000 萬的房子差 10% 和 300 萬差 10% 一樣重要
    y = log(pure_house_unit_price_ping),
    # 時間趨勢：交易月份換成「距離 2020-01 幾個月」，讓模型學到房價逐月上漲
    t_month_idx = (as.integer(format(as.Date(trans_ym), "%Y")) - 2020) * 12 +
                  as.integer(format(as.Date(trans_ym), "%m")),
    # 樓層比：同樣 10 樓，在 10 層樓的頂樓 vs 在 30 層樓的中間，價值不同
    floor_ratio = if_else(!is.na(floor_num) & total_floors_num > 0, floor_num / total_floors_num, NA_real_),
    is_top_floor = as.integer(!is.na(floor_num) & floor_num == total_floors_num),
    # 土地坪數 / 建物坪數：透天厝的土地比例高 = 比較值錢
    land_ratio = if_else(pure_house_ping > 0, land_ping / pure_house_ping, NA_real_),
    # coalesce(..., FALSE)：原始資料這兩欄有空值，NA == "有" 會得到 NA，線性迴歸遇到 NA 就預測不出來
    elevator   = as.integer(coalesce(has_elevator == "有", FALSE)),
    management = as.integer(coalesce(has_management == "有", FALSE)),
    parking    = as.integer(include_parking == "是"),
    parking_unknown = as.integer(parking_price_unknown),
    city_district = paste0(city, district)
  )
}

# --- 4. 和「時間切點」有關的特徵：附近行情 + 同棟歷史 -----------------------------------
# cap_month：回測 / 訓練時，切點之後的成交在「上線當下」還不存在 → 時間窗最多用到切點 (防資料洩漏)
#            屋況調整 (交叉預測) 不模擬上線，用 Inf 即可
add_history_features <- function(df, cap_month = Inf) {
  df <- df %>% select(-any_of(c("nbr_lp", "nbr_n", "nbr_lp_all", "bld_lp", "bld_n", "bld_dt")))  # 重算前先移除舊的
  bind_cols(df,
            neighbor_price(df$tx, df$ty, as.character(df$b_category), df$t_month_idx, df$y, cap_month = cap_month),
            building_history(df$addr_key, df$t_month_idx, df$y, cap_month = cap_month))
}

# 臨路等級：只和地址、座標有關 (和時間無關) → 每份資料算一次就好
# road_tbl = build_road_tables() 的結果 (讀 OSM 約 10 秒，呼叫端算一次重複使用)
add_road_features <- function(df, road_tbl) {
  rest <- str_remove(str_remove(df$address_norm, fixed(df$city)), fixed(df$district))
  bind_cols(df %>% select(-any_of(c("lane_depth", "road_type", "road_class", "d_major_road"))),
            road_features(rest, cbind(df$tx, df$ty), road_tbl))
}

# --- 5. 特徵清單 ---------------------------------------------------------------
features_v1 <- c("city", "district", "b_category", "house_age_at_trans", "pure_house_ping",
                 "land_ratio", "floor_num", "total_floors_num", "floor_ratio", "is_top_floor",
                 "rooms", "baths", "elevator", "management", "parking", "parking_unknown",
                 "t_month_idx", "lng", "lat")
# v2 新增：附近行情 + 交通 (捷運、輕軌、台鐵、高鐵、機場、公車) + 嫌惡設施 + 生活機能
features_v2 <- c(features_v1, "nbr_lp", "nbr_n", "nbr_lp_all",
                 paste0("d_", dist_cols), paste0("n_", count_cols, "_500"))
# v3 新增 (誤差優化實驗，紀錄見 models/experiments.csv)：同棟歷史成交 + 臨路等級
features_v3 <- c(features_v2, "bld_lp", "bld_n", "bld_dt", "lane_depth", "road_type", "road_class", "d_major_road")
features <- features_v3                                  # 正式模型目前用的版本

# --- 6. 模型輸入格式 -----------------------------------------------------------
# 類別欄位的「level 清單」要固定，訓練、測試、App 預測都用同一份。
# ⚠️ 舊版這裡寫 as.factor()：測試集少了某些區時，levels 會變短，
#    轉成整數編號後「鳳山區」在訓練集是 152 號、在測試集可能變成 140 號 → XGBoost 看錯區！
#    (隨機森林用名稱對應不受影響，這也是當初 XGBoost 分數輸給隨機森林的原因之一)
make_fac_levels <- function(df) map(df[c("city", "district", "b_category")], ~ sort(unique(as.character(.x))))

# feats、levels 一定要「傳進來」：同一個 R 程序裡可能同時比較 v2、v3 兩組特徵 (回測、實驗)
prep_x <- function(d, feats, levels) {
  d %>% select(all_of(feats)) %>%
    mutate(across(any_of(names(levels)), ~ factor(.x, levels = levels[[cur_column()]])),
           across(where(is.numeric), ~ replace_na(.x, -1)))   # 樹模型可以把 -1 當成「缺值」這個類別
}
# XGBoost 只吃數字矩陣：類別欄位轉成整數編號 (樹模型可以接受這種編碼)
to_matrix <- function(d, feats, levels) {
  prep_x(d, feats, levels) %>% mutate(across(where(is.factor), as.integer)) %>% as.matrix()
}

# --- 7. XGBoost 參數 (正式模型、回測、實驗、屋況調整共用) -------------------------------
xgb_params <- list(
  objective = "reg:squarederror",
  eta = 0.1,                # 學習率：每棵樹只修正一點點，慢慢逼近 → 比較不會過度擬合
                            # (原本 0.05 + 3000 棵樹，模型在記憶體裡 126MB，免費主機只有 1GB；
                            #  改 0.1 + 最多 1500 棵，大小約減半，準確度差異見學習筆記)
  max_depth = 8,            # 每棵樹最多幾層：越深越能抓複雜關係，也越容易過擬合
  subsample = 0.8,          # 每棵樹只用 80% 資料
  colsample_bytree = 0.8,   # 每棵樹只用 80% 欄位
  min_child_weight = 10,
  # 固定 XGBoost 自己的亂數種子 (subsample / colsample 抽樣用)。
  # 搭配 SQL 的 ORDER BY (資料順序固定)：同一份資料重訓 = 同一個模型，估價不會因為「運氣」跳動。
  # 剩下的變動才是「新資料帶來的真實行情變化」(實驗 01 量過：沒固定時同一間房子可以差 13%)
  seed = 42
)

# --- 8. 訓練與網站一致性測試的「標準答案」 ----------------------------------------------
# 從最新月份挑 n 筆真實成交，用「訓練的算法」算出模型輸入 → 存起來。
# 部署前 tests/serving_consistency.R 會把同樣的條件餵給網站的 make_estimate_input()，
# 兩邊算出來的每個特徵都要一樣，否則就是 training-serving skew (這個專案踩過兩次：土地持分、inp$road)
#
# 怎麼模擬「網站估價的當下」？
#   網站一律站在「資料最新月份 latest」看過去：附近行情看 [latest-12, latest-1]、同棟歷史看 [latest-36, latest-1]，
#   月份特徵 t_month_idx = latest。所以這裡用 query_month = latest 重算這些特徵，並把 t_month_idx 改成 latest。
#   (最新一個月因為申報延遲只有個位數筆成交，所以從「最近 3 個月」抽樣)
# 為什麼排除 parking_unknown = 1？網站沒有「車位價格不明」這個選項 (一律當 0)，這是刻意的設計差異
make_serving_fixture <- function(df, feats, levels, n = 300, seed = 42) {
  latest <- max(df$t_month_idx)
  lm <- rep(latest, nrow(df))
  d <- bind_cols(df %>% select(-any_of(c("nbr_lp", "nbr_n", "nbr_lp_all", "bld_lp", "bld_n", "bld_dt"))),
                 neighbor_price(df$tx, df$ty, as.character(df$b_category), df$t_month_idx, df$y, query_month = lm),
                 building_history(df$addr_key, df$t_month_idx, df$y, query_month = lm)) %>%
    mutate(t_month_idx = latest) %>%
    filter(df$t_month_idx > latest - 3, parking_unknown == 0, grepl("號", address_norm))
  set.seed(seed)
  d <- d[sort(sample.int(nrow(d), min(n, nrow(d)))), ]
  list(rows = d %>% select(city, district, address_norm, b_category, house_age_at_trans, pure_house_ping, land_ping,
                           floor_num, total_floors_num, rooms, baths, parking, elevator, management),
       x = to_matrix(d, feats, levels), features = feats, latest = latest, made_at = Sys.time())
}
