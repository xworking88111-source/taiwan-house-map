# ==============================================================================
# 模型回測 (Backtesting)：模型在「不同時間點上線」時，會不會一樣準？
#
# new_model_training.R 只用「最近 6 個月」測一次。但只測一次可能剛好運氣好 (或不好)。
# 回測的做法 (Rolling-origin evaluation，時間序列的交叉驗證)：
#
#   ──訓練──────────────────────|測試 6 個月|                     ← 切點 1 (24 個月前)
#   ──訓練────────────────────────────|測試 6 個月|               ← 切點 2 (18 個月前)
#   ──訓練──────────────────────────────────|測試 6 個月|         ← 切點 3 (12 個月前)
#   ──訓練────────────────────────────────────────|測試 6 個月|   ← 切點 4 (6 個月前)
#   ──訓練──────────────────────────────────|測試 12 個月     |   ← 預測得更久，誤差會變大嗎？
#
# 每一次都只用「切點之前」的資料訓練，絕對不偷看未來 → 模擬真實上線的情況。
# 看的重點：
#   1. MAPE 是否穩定 (每次差不多 → 模型可靠；忽高忽低 → 模型不穩定)
#   2. 測試 12 個月的誤差比 6 個月大多少 → 決定「多久要重新訓練一次」
#   3. 訓練資料只用最近 1 年 vs 用全部 → 舊資料是幫助還是干擾？
#
# v3 更新：比較 v2 (空間特徵) vs v3 (再加同棟歷史 + 臨路等級)，並用「配對 bootstrap」算改進幅度的信賴區間
#   → 回答面試官的問題：「你怎麼確定 0.5 個百分點的進步不是運氣？」
#
# 這支程式只用 XGBoost (最準、最快)；隨機森林太慢，不適合跑很多次。
# 執行時間：約 15~30 分鐘
# ==============================================================================
library(xgboost)

# --- 1. 共用函式庫：讀資料、清理、特徵工程、模型輸入格式 (和訓練程式同一份，見 R/model_lib.R) ---
source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "R", "model_lib.R"))
setwd(project_path)
df <- load_training_data() %>% clean_trades() %>% engineer_features()
fac_levels <- make_fac_levels(df)
v3_new <- setdiff(features_v3, features_v2)
# 臨路等級只和地址、座標有關 (和時間無關) → 算一次就好；同棟歷史和附近行情要依切點重算
df_base <- add_road_features(df, build_road_tables())

# --- 2. 回測 ------------------------------------------------------------------
ape_store <- list()      # 每次回測每一筆測試資料的誤差，留著算 v2 vs v3 的配對 bootstrap
backtest_one <- function(cut, test_len, train_months = Inf, feature_set = "v2") {
  # 附近行情、同棟歷史要「每個切點重算」：時間窗最多只能用到切點 (切點之後的成交在上線當下還不存在)
  df <- add_history_features(df_base, cap_month = cut)
  feats <- switch(feature_set, v1 = features_v1, v2 = features_v2, v3 = features_v3)
  train <- df %>% filter(t_month_idx <= cut, t_month_idx > cut - train_months)
  test  <- df %>% filter(t_month_idx > cut, t_month_idx <= cut + test_len)
  # 訓練集最後 3 個月當驗證集 (early stopping 用)
  valid_idx <- train$t_month_idx > (cut - 3)
  dtrain <- xgb.DMatrix(to_matrix(train[!valid_idx, ], feats, fac_levels), label = train$y[!valid_idx])
  dvalid <- xgb.DMatrix(to_matrix(train[valid_idx, ], feats, fac_levels),  label = train$y[valid_idx])
  fit <- xgb.train(params = xgb_params, data = dtrain, nrounds = 3000,
                   evals = list(valid = dvalid), early_stopping_rounds = 100, verbose = 0)
  pred <- predict(fit, to_matrix(test, feats, fac_levels))
  ape <- abs(exp(pred) - exp(test$y)) / exp(test$y)
  ape_store[[paste(feature_set, cut, test_len)]] <<- ape
  by_type <- tapply(ape, as.character(test$b_category), mean)
  tibble(features = feature_set, cut = cut, test_months = test_len,
         train_window = if (is.infinite(train_months)) "全部" else paste(train_months, "個月"),
         n_train = nrow(train), n_test = nrow(test),
         MAPE = mean(ape), hit_10 = mean(ape <= .10), hit_20 = mean(ape <= .20),
         MAPE_透天厝 = by_type[["透天厝"]], MAPE_電梯大樓 = by_type[["電梯大樓"]],
         MAPE_華廈 = by_type[["華廈"]], MAPE_公寓 = by_type[["公寓"]])
}

last <- max(df$t_month_idx)
month_label <- function(idx) format(as.Date(sprintf("%d-%02d-01", 2020 + (idx - 1) %/% 12, (idx - 1) %% 12 + 1)), "%Y-%m")
# 同一個切點，v1 (舊特徵) 和 v2 (加入空間特徵) 都跑一次 → 同一份資料、同一個測試期，公平比較
plan <- tribble(
  ~cut,        ~test_len, ~train_months,
  last - 24,   6,         Inf,
  last - 18,   6,         Inf,
  last - 12,   6,         Inf,
  last - 6,    6,         Inf,
  last - 12,   12,        Inf      # 預測 12 個月
) %>% tidyr::crossing(feature_set = c("v2", "v3"))
results <- pmap_dfr(plan, function(cut, test_len, train_months, feature_set) {
  message(">> [", feature_set, "] 切點 ", month_label(cut), "，測試 ", test_len, " 個月")
  backtest_one(cut, test_len, train_months, feature_set)
}) %>% mutate(cut = month_label(cut))

print(results %>% select(features, cut, test_months, MAPE, hit_10, MAPE_透天厝, MAPE_電梯大樓, MAPE_華廈, MAPE_公寓) %>%
        mutate(across(starts_with(c("MAPE", "hit")), \(x) scales::percent(x, 0.1))), width = 200)
dir.create("models", showWarnings = FALSE)
write_csv(results, "models/backtest_results_v3.csv")

# --- 2b. 改進是不是運氣？配對 bootstrap 信賴區間 ---------------------------------
# 同一批測試房子，v2 和 v3 各有一個誤差 → 配對差 d = v3 誤差 - v2 誤差 (負的 = v3 比較準)
# bootstrap：從測試房子中「有放回」地抽 n 間，算 d 的平均，重複 2000 次 → 取第 2.5% 和 97.5% 百分位
#   如果整個 95% 區間都 < 0 → 改進不太可能是運氣
# 以「建物」為單位抽 (cluster bootstrap)：同一棟的成交高度相關，逐筆抽會讓區間假性變窄
set.seed(42)
boot <- pmap_dfr(distinct(plan, cut, test_len), function(cut, test_len) {
  d   <- ape_store[[paste("v3", cut, test_len)]] - ape_store[[paste("v2", cut, test_len)]]
  key <- df_base$addr_key[df_base$t_month_idx > cut & df_base$t_month_idx <= cut + test_len]
  grp <- split(d, key)
  s <- sapply(grp, sum); n <- lengths(grp)
  bs <- replicate(2000, { i <- sample.int(length(s), replace = TRUE); sum(s[i]) / sum(n[i]) })
  tibble(cut = month_label(cut), test_months = test_len, 改進_pp = -100 * mean(d),
         CI95_low = -100 * quantile(bs, .975, names = FALSE), CI95_high = -100 * quantile(bs, .025, names = FALSE))
})
print(boot %>% mutate(across(where(is.numeric) & !test_months, \(x) round(x, 2))))
write_csv(boot, "models/backtest_bootstrap_v3.csv")

# --- 3. 怎麼解讀 --------------------------------------------------------------
# - 前 4 列 (6 個月測試) 的 MAPE 如果都在 12~15% 之間 → 模型穩定，可以信任。
# - 第 5 列 (12 個月測試) 比第 3 列 (同切點、6 個月) 高很多 → 模型會隨時間「老化」，
#   應該固定頻率重新訓練 (GitHub Actions 每週更新時已經會重訓)。
# - 第 6 列 (只用 1 年資料) 比第 4 列 (全部資料) 差 → 舊資料有幫助，保留 5 年是對的。
