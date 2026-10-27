# ==============================================================================
# 房價預測模型：訓練範本 (教學版)
#
# 目標：輸入「縣市、區、建物型態、屋齡、坪數、樓層、車位、位置」→ 預測「每坪單價」與「總價區間」
#
# 這支檔案依照真實資料科學專案的順序寫：
#   1. 讀資料      2. 清理離群值      3. 特徵工程      4. 切訓練/測試集 (依時間)
#   5. 基準模型    6. 線性迴歸        7. 隨機森林      8. XGBoost
#   9. 比較成績    10. 價格區間       11. 存模型、示範預測
#
# 每個模型都會用同一套指標評分，你可以清楚看到「為什麼要用比較複雜的模型」。
# 想看概念說明 (為什麼取 log、什麼是過度擬合...)，請對照 docs/學習筆記.md。
# ==============================================================================
library(tidyverse)
library(DBI)
library(RPostgres)
library(ranger)    # 隨機森林 (速度快、支援分位數預測)
library(xgboost)   # 梯度提升樹 (表格資料競賽最常見的冠軍模型)

# 共用設定 (路徑、資料庫連線)：本機和 GitHub Actions 都用同一份程式
source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "config.R"))
setwd(project_path)
# 空間特徵的函數 (設施距離、附近行情) 都寫在 new_features.R，訓練、回測、App 共用同一份。
# features.as_library = TRUE：只載入函數，不要執行 new_features.R 的主程式
options(features.as_library = TRUE)
source("new_features.R")
set.seed(42)       # 固定亂數種子：每次執行結果都一樣，才能比較「改了什麼造成差異」

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
    SELECT f.city, f.district, f.b_category, f.trans_ym,
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
      AND g.geo_level <> 'none';")) %>% as_tibble()
}
if (!exists("df_raw")) df_raw <- load_training_data()
message("讀入 ", nrow(df_raw), " 筆")

# --- 2. 清理離群值 ------------------------------------------------------------
# 實價登錄有很多「怪交易」沒寫在備註：例如 1 坪 1 萬的透天 (其實是持分)、
# 或 1 坪 500 萬的豪宅頂樓。這些點會把模型往奇怪的方向拉。
# 做法：在「同縣市 + 同建物型態」裡，砍掉單價最低 1% 和最高 1%。
# 為什麼分組砍？台北的 1% 低價可能是台東的正常價，不能全台一起砍。
df <- df_raw %>%
  filter(!is.na(trans_ym),                                      # 交易月份無效 (例如 13 月) 的少數資料：
                                                                # 留著會讓 max() 變成 NA，時間切分整個失效
         pure_house_ping >= 5, pure_house_ping <= 300,          # 5 坪以下多半是車位/持分
         house_age_at_trans <= 70) %>%
  group_by(city, b_category) %>%
  filter(between(pure_house_unit_price_ping,
                 quantile(pure_house_unit_price_ping, 0.01),
                 quantile(pure_house_unit_price_ping, 0.99))) %>%
  ungroup()
message("清理後 ", nrow(df), " 筆 (移除 ", nrow(df_raw) - nrow(df), " 筆)")

# --- 3. 特徵工程 (Feature Engineering) ----------------------------------------
# 「特徵」= 模型看得到的欄位。好的特徵往往比換模型更有效。
df <- df %>% mutate(
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

# --- 4. 切訓練集 / 測試集：用「時間」切，不是隨機切 ----------------------------
# 為什麼？我們的 App 是用「過去的成交」預測「現在/未來」的房價。
# 如果隨機切，同一棟大樓同一個月的交易會同時出現在訓練和測試，
# 模型等於偷看答案 (資料洩漏 data leakage)，分數會好看但上線就破功。
cut_month <- max(df$t_month_idx) - 6                     # 最近 6 個月當測試集

# 附近行情 (新特徵，定義見 new_features.R 的 neighbor_price)：
#   附近約 500 公尺、同型態、交易月份「之前」12 個月的成交行情。
#   cap_month = cut_month：測試期的交易在上線當下還不存在，時間窗最多只能用到切點，否則就是偷看答案
df <- bind_cols(df %>% select(-any_of(c("nbr_lp", "nbr_n", "nbr_lp_all"))),
                neighbor_price(df$tx, df$ty, as.character(df$b_category), df$t_month_idx, df$y, cap_month = cut_month))
message("附近行情：", scales::percent(mean(!is.na(df$nbr_lp)), 0.1), " 的交易附近有足夠的同型態成交")
train <- df %>% filter(t_month_idx <= cut_month)
test  <- df %>% filter(t_month_idx >  cut_month)
message("訓練集 ", nrow(train), " 筆 / 測試集 ", nrow(test), " 筆")

# 評分函數：同時看三個指標
#   RMSE(log)：模型訓練時真正最小化的東西，越小越好
#   MAPE     ：平均百分比誤差，「平均差幾 %」，業界最好懂的指標
#   命中率   ：預測落在真實價格 ±10% / ±20% 內的比例 (房產估價 AVM 業界標準)
evaluate <- function(name, pred_log, truth_log = test$y) {
  ape <- abs(exp(pred_log) - exp(truth_log)) / exp(truth_log)
  tibble(model = name,
         rmse_log = sqrt(mean((pred_log - truth_log)^2)),
         MAPE     = mean(ape),
         hit_10   = mean(ape <= 0.10),
         hit_20   = mean(ape <= 0.20))
}

# --- 5. 基準模型 (Baseline)：「同區同型態的中位數」 ---------------------------
# 永遠先做一個笨模型。如果複雜模型贏不了它，就代表哪裡做錯了。
baseline_tbl <- train %>% group_by(city_district, b_category) %>% summarise(med = median(y), .groups = "drop")
city_tbl     <- train %>% group_by(city, b_category) %>% summarise(city_med = median(y), .groups = "drop")
pred_base <- test %>%
  left_join(baseline_tbl, by = c("city_district", "b_category")) %>%
  left_join(city_tbl, by = c("city", "b_category")) %>%
  mutate(p = coalesce(med, city_med, median(train$y))) %>% pull(p)
results <- evaluate("0. 基準：區域中位數", pred_base)

# --- 6. 線性迴歸 (Hedonic Pricing Model) --------------------------------------
# 經濟學的「特徵價格模型」：房價 = 各個特徵價值的加總。
# log(單價) = 區域效果 + 型態效果 + b1*屋齡 + b2*屋齡² + ...
# 優點：每個係數都能解釋 (例如屋齡每多 1 年，單價約降 x%)；缺點：抓不到複雜的交互作用。
# 屋齡平方項：房子剛蓋好折舊快、老了以後折舊慢 (甚至都更題材會漲)，關係是曲線不是直線。
lm_fit <- lm(y ~ city_district + b_category + poly(house_age_at_trans, 2) + log(pure_house_ping) +
               land_ratio + elevator + management + parking + parking_unknown + t_month_idx,
             data = train %>% mutate(land_ratio = replace_na(land_ratio, 0)))
test_lm <- test %>% mutate(land_ratio = replace_na(land_ratio, 0)) %>%
  filter(city_district %in% train$city_district)        # 訓練集沒看過的區，線性迴歸無法預測
pred_lm <- predict(lm_fit, test_lm)
results <- bind_rows(results, evaluate("1. 線性迴歸", pred_lm, test_lm$y))
# 看看係數：屋齡、坪數的影響方向合理嗎？
print(summary(lm_fit)$coefficients[!grepl("city_district", names(coef(lm_fit))), ], digits = 3)

# --- 7. 隨機森林 (Random Forest) ----------------------------------------------
# 概念：種幾百棵「決策樹」，每棵樹只看部分資料、部分欄位，最後投票 (平均)。
# 優點：自動抓非線性和交互作用 (例如「台北的屋齡」和「台東的屋齡」影響不同)，
#       不太需要調參數；用經緯度當特徵，就能學到「哪一塊地比較貴」。
features_v1 <- c("city", "district", "b_category", "house_age_at_trans", "pure_house_ping",
                 "land_ratio", "floor_num", "total_floors_num", "floor_ratio", "is_top_floor",
                 "rooms", "baths", "elevator", "management", "parking", "parking_unknown",
                 "t_month_idx", "lng", "lat")
# v2 新增：附近行情 + 交通 (捷運、輕軌、台鐵、高鐵、機場、公車) + 嫌惡設施 + 生活機能
features <- c(features_v1, "nbr_lp", "nbr_n", "nbr_lp_all",
              paste0("d_", dist_cols), paste0("n_", count_cols, "_500"))

# 類別欄位的「level 清單」要固定，訓練、測試、App 預測都用同一份。
# ⚠️ 舊版這裡寫 as.factor()：測試集少了某些區時，levels 會變短，
#    轉成整數編號後「鳳山區」在訓練集是 152 號、在測試集可能變成 140 號 → XGBoost 看錯區！
#    (隨機森林用名稱對應不受影響，這也是當初 XGBoost 分數輸給隨機森林的原因之一)
fac_levels <- map(df[c("city", "district", "b_category")], ~ sort(unique(as.character(.x))))

prep_x <- function(d) {
  d %>% select(all_of(features)) %>%
    mutate(across(c(city, district, b_category), ~ factor(.x, levels = fac_levels[[cur_column()]])),
           across(where(is.numeric), ~ replace_na(.x, -1)))   # 樹模型可以把 -1 當成「缺值」這個類別
}
# 資料太大時先抽樣，學習階段跑得快比較重要；正式版再用全部資料
train_rf <- if (nrow(train) > 300000) slice_sample(train, n = 300000) else train
rf_fit <- ranger(
  x = prep_x(train_rf), y = train_rf$y,
  num.trees = 300,          # 樹的數量：越多越穩定，但超過 300~500 通常就沒什麼進步
  min.node.size = 10,       # 每片葉子至少 10 筆：避免記住個別交易 (過度擬合)
  importance = "impurity",  # 計算特徵重要度
  quantreg = TRUE,          # 開啟「分位數森林」→ 第 10 步用來算價格區間
  num.threads = parallel::detectCores() - 1
)
pred_rf <- predict(rf_fit, prep_x(test))$predictions
results <- bind_rows(results, evaluate("2. 隨機森林", pred_rf))

# --- 8. XGBoost (梯度提升樹) --------------------------------------------------
# 概念：一棵接一棵種樹，每一棵新樹專門修正「前面所有樹還猜錯的部分」。
# 通常是表格資料最準的模型，但參數比較多、比較容易過度擬合，所以要用「驗證集 + 提早停止」。
to_matrix <- function(d) {
  x <- prep_x(d)
  # XGBoost 只吃數字：類別欄位轉成整數編號 (樹模型可以接受這種編碼)
  x %>% mutate(across(where(is.factor), as.integer)) %>% as.matrix()
}
# 從訓練集再切最後 3 個月當「驗證集」，用來決定要種幾棵樹 (early stopping)
valid_idx <- train$t_month_idx > (cut_month - 3)
dtrain <- xgb.DMatrix(to_matrix(train[!valid_idx, ]), label = train$y[!valid_idx])
dvalid <- xgb.DMatrix(to_matrix(train[valid_idx, ]),  label = train$y[valid_idx])

xgb_params <- list(
  objective = "reg:squarederror",
  eta = 0.1,                # 學習率：每棵樹只修正一點點，慢慢逼近 → 比較不會過度擬合
                            # (原本 0.05 + 3000 棵樹，模型在記憶體裡 126MB，免費主機只有 1GB；
                            #  改 0.1 + 最多 1500 棵，大小約減半，準確度差異見學習筆記)
  max_depth = 8,            # 每棵樹最多幾層：越深越能抓複雜關係，也越容易過擬合
  subsample = 0.8,          # 每棵樹只用 80% 資料
  colsample_bytree = 0.8,   # 每棵樹只用 80% 欄位
  min_child_weight = 10
)
xgb_fit <- xgb.train(
  params = xgb_params, data = dtrain, nrounds = 1500,
  evals = list(train = dtrain, valid = dvalid),
  early_stopping_rounds = 100,   # 驗證集分數連續 100 輪沒進步就停 → 自動找到最佳樹數
  verbose = 0
)
pred_xgb <- predict(xgb_fit, to_matrix(test))
results <- bind_rows(results, evaluate("3. XGBoost", pred_xgb))

# --- 9. 比較成績 --------------------------------------------------------------
print(results %>% mutate(across(c(MAPE, hit_10, hit_20), \(x) scales::percent(x, accuracy = 0.1))))

# 特徵重要度：模型最依賴哪些資訊？(面試很常被問：你的模型學到了什麼？)
print(head(sort(rf_fit$variable.importance, decreasing = TRUE), 10))

# 分組檢查：模型在哪些縣市/型態表現特別差？→ 下一輪改進的方向
test %>% mutate(ape = abs(exp(pred_xgb) - exp(y)) / exp(y)) %>%
  group_by(b_category) %>% summarise(n = n(), MAPE = mean(ape), hit_10 = mean(ape <= .1)) %>%
  arrange(desc(MAPE)) %>% print()

# --- 10. 價格區間 (行情區間) --------------------------------------------------
# 一個點估計 (例如 28.5 萬/坪) 對使用者不夠：他想知道「合理範圍」。
# 分位數森林可以直接給出 P10 ~ P90：80% 的同條件房子會落在這個區間內。
q <- predict(rf_fit, prep_x(test), type = "quantiles", quantiles = c(0.1, 0.5, 0.9))$predictions
coverage <- mean(test$y >= q[, 1] & test$y <= q[, 3])
message("[隨機森林] P10~P90 區間實際涵蓋率：", scales::percent(coverage, 0.1), " (理想值約 80%)")

# 方法二：XGBoost 分位數迴歸 (App 上線用的版本)
# 一般迴歸最小化「平方誤差」→ 預測平均值；
# 分位數迴歸最小化「pinball loss」→ 預測第 10%、50%、90% 百分位。
#   例如 alpha = 0.9：猜太低的懲罰是猜太高的 9 倍 → 模型會往上猜到「90% 的房子都低於它」的位置。
# 一個模型同時輸出 3 個分位數，檔案只有幾 MB。
# 模型大小：分位數模型一次輸出 3 個值，3000 棵樹在記憶體裡要 313MB，
# 免費的 shinyapps.io 只有 1GB 放不下。區間不需要像點估計那麼精細 →
# 學習率調高 (0.15)、樹淺一點 (深度 6)、最多 600 棵，大小約原本的 1/10；準確度用保形校正補回來
xgbq_fit <- xgb.train(
  params = modifyList(xgb_params, list(objective = "reg:quantileerror",
                                       quantile_alpha = c(0.1, 0.5, 0.9),
                                       eta = 0.15, max_depth = 6)),
  data = dtrain, nrounds = 600,
  evals = list(valid = dvalid), early_stopping_rounds = 100, verbose = 0
)
qx <- predict(xgbq_fit, to_matrix(test))          # 回傳 n × 3 的矩陣：P10, P50, P90
# 保形校正 (Conformal calibration)：分位數模型的區間常常「太有自信」(太窄)。
# 做法：在驗證集上量「真實值超出區間多少」，取這個超出量的第 80 百分位當作加寬量 q_hat。
#   E = max(P10 - 真實值, 真實值 - P90)：落在區間內時 E 是負的，跑出去時 E 是正的
#   區間改成 [P10 - q_hat, P90 + q_hat] → 理論上約 80% 的新資料會落在裡面
# 這是近年很熱門的「Conformalized Quantile Regression (CQR)」，面試講得出來很加分。
# (注意：驗證集也被拿來做 early stopping，嚴格來說應該再切一份獨立的校正集，這裡為了簡單先共用)
qv <- predict(xgbq_fit, dvalid)
yv <- getinfo(dvalid, "label")
conformal_q <- unname(quantile(pmax(qv[, 1] - yv, yv - qv[, 3]), 0.8))
message("保形校正加寬量 (log 尺度)：", round(conformal_q, 4), "，約等於區間兩端各加寬 ",
        scales::percent(exp(conformal_q) - 1, 0.1))
qx[, 1] <- qx[, 1] - conformal_q
qx[, 3] <- qx[, 3] + conformal_q
coverage_xgb <- mean(test$y >= qx[, 1] & test$y <= qx[, 3])
message("[XGBoost 分位數] P10~P90 區間實際涵蓋率：", scales::percent(coverage_xgb, 0.1))
results <- bind_rows(results, evaluate("4. XGBoost 分位數 (P50)", qx[, 2]))
print(results %>% mutate(across(c(MAPE, hit_10, hit_20), \(x) scales::percent(x, accuracy = 0.1))))

# --- 11. 存模型 + 示範預測 ----------------------------------------------------
dir.create("models", showWarnings = FALSE)
model_meta <- list(features = features,
                   factor_levels = fac_levels, t_month_idx_max = max(df$t_month_idx),
                   trained_at = Sys.time(), metrics = results)
# 完整版 (含隨機森林)：分位數森林會把整份訓練資料存在模型裡 → 檔案約 300MB 以上，只留在本機
# 版本號 v2 = 加入空間特徵的版本；v1 的檔案保留不覆蓋，需要時可以退回比較
saveRDS(c(list(rf = rf_fit, xgb = xgb_fit), model_meta), "models/price_model_v2_full.rds")
# App 版：只存 XGBoost (幾 MB)，免費的 shinyapps.io 才放得下。
# xgb.save.raw 是 XGBoost 官方的存檔格式，換電腦/換版本也讀得回來
saveRDS(c(list(xgb_raw = xgb.save.raw(xgb_fit), xgbq_raw = xgb.save.raw(xgbq_fit),
               interval_coverage = coverage_xgb, conformal_q = conformal_q), model_meta),
        "models/price_model_v2_app.rds")
# App 估價時要查「附近行情」：把最近 12 個月的成交按「格子 × 型態」加總成一張小表 (見 new_features.R)
nbr_table <- build_nbr_table(df$tx, df$ty, as.character(df$b_category), df$t_month_idx, df$y,
                             ref_month = max(df$t_month_idx))
saveRDS(as.data.frame(nbr_table), "models/nbr_table.rds")

# 示範：「高雄市鳳山區 (五甲) 瑞隆東路附近、35 年透天、建物 82 坪、土地 30 坪、無車位」
# 經緯度從 geo_data/road_lookup.rds 查「瑞隆東路」的代表點 → 和 App 用的是同一套方法
lookup <- readRDS("geo_data/road_lookup.rds")
loc <- lookup$roads %>% filter(city == "高雄市", district == "鳳山區", name == "瑞隆東路") %>% slice(1)
case <- tibble(city = "高雄市", district = "鳳山區", b_category = "透天厝",
               house_age_at_trans = 35, pure_house_ping = 82, land_ratio = 30 / 82,
               # 注意要寫 NA_real_ 而不是 NA：單獨的 NA 是「邏輯型」，prep_x() 只會把「數字型」的 NA 填成 -1
               floor_num = NA_real_, total_floors_num = 3, floor_ratio = NA_real_, is_top_floor = 0,
               rooms = 4, baths = 3, elevator = 0, management = 0, parking = 0, parking_unknown = 0,
               t_month_idx = max(df$t_month_idx), lng = loc$lng, lat = loc$lat)
# 空間特徵：和 App 一樣，用 new_features.R 的函數算 (設施距離 + 附近行情)
case_xy <- lnglat_to_xy(case$lng, case$lat)
case <- bind_cols(case, facility_features(case_xy, readRDS("geo_data/facilities.rds")),
                  lookup_nbr(nbr_table, case_xy[1], case_xy[2], "透天厝"))
case_x <- prep_x(case)   # prep_x 已經用固定的 fac_levels，編號一定和訓練時一致
q_case <- exp(predict(rf_fit, case_x, type = "quantiles", quantiles = c(.1, .5, .9))$predictions)
p_xgb  <- exp(predict(xgb_fit, case_x %>% mutate(across(where(is.factor), as.integer)) %>% as.matrix()))
cat(sprintf("\n單價 (XGBoost)：%.1f 萬/坪\n行情區間 (RF P10~P90)：%.1f ~ %.1f 萬/坪\n總價估計：%.0f 萬 (區間 %.0f ~ %.0f 萬)\n",
            p_xgb / 1e4, q_case[1] / 1e4, q_case[3] / 1e4,
            p_xgb * 82 / 1e4, q_case[1] * 82 / 1e4, q_case[3] * 82 / 1e4))
