# ==============================================================================
# 模型實驗框架：每個實驗都用「同一份資料、同一個測試期、同一套評分」，結果寫進實驗紀錄表
#
# 為什麼需要框架？
#   之前每次改模型都各自跑一次訓練程式，數字很難公平比較 (資料、切點、評分方式可能不同)。
#   業界的資料科學家會維護「實驗紀錄」(experiment tracking)：每個實驗記下改了什麼、成績如何，
#   有效才放進正式模型。這裡用最簡單的方式實作：一張 CSV (models/experiments.csv)。
#
# 用法：
#   source("new_model_experiments.R")    # 讀資料、準備好訓練集/測試集 (約 2 分鐘)
#   然後呼叫下面的函數做實驗，見檔案最後的「實驗」區塊
# ==============================================================================
library(xgboost)

# --- 1. 共用函式庫：讀資料、清理、特徵工程 (和訓練程式同一份，見 R/model_lib.R) ----------------
source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "R", "model_lib.R"))
setwd(project_path)
df <- load_training_data() %>% clean_trades() %>% engineer_features()
fac_levels <- make_fac_levels(df)
features_base <- features_v2           # 實驗 01~04 的起點 = v2 (當時的正式模型)

# --- 2. 固定的實驗設定：測試期 = 最近 6 個月，驗證集 = 訓練期最後 3 個月 ---------------------
cut_month <- max(df$t_month_idx) - 6
# 附近行情 (v2 就有的特徵)；同棟歷史 bld_* 由需要的實驗自己算 (見 experiments/02)
df <- bind_cols(df, neighbor_price(df$tx, df$ty, as.character(df$b_category), df$t_month_idx, df$y,
                                   cap_month = cut_month))
split_data <- function(d = df) {
  list(train = d %>% filter(t_month_idx <= cut_month - 3),
       valid = d %>% filter(t_month_idx > cut_month - 3, t_month_idx <= cut_month),
       test  = d %>% filter(t_month_idx > cut_month))
}

# --- 3. 訓練與評分 -------------------------------------------------------------------------
# seed：XGBoost 的 subsample / colsample (每棵樹隨機抽 80% 資料和欄位) 用到隨機數。
#       同時設定 R 的 set.seed() 和 XGBoost 自己的 seed 參數，確保「同一台電腦」上結果可重現
fit_xgb <- function(sp, feats = features_base, params = xgb_params, nrounds = 1500, seed = 1) {
  set.seed(seed)
  dtrain <- xgb.DMatrix(to_matrix(sp$train, feats, fac_levels), label = sp$train$y)
  dvalid <- xgb.DMatrix(to_matrix(sp$valid, feats, fac_levels), label = sp$valid$y)
  xgb.train(params = modifyList(params, list(seed = seed)), data = dtrain, nrounds = nrounds,
            evals = list(valid = dvalid), early_stopping_rounds = 100, verbose = 0)
}
predict_xgb <- function(models, d, feats = features_base) {
  x <- to_matrix(d, feats, fac_levels)
  # 多個模型 → 在 log 尺度上平均 (等於價格的幾何平均)
  rowMeans(sapply(if (inherits(models, "xgb.Booster")) list(models) else models, predict, newdata = x))
}
score <- function(pred_log, test) {
  ape <- abs(exp(pred_log) - exp(test$y)) / exp(test$y)
  bt <- tapply(ape, as.character(test$b_category), mean)
  tibble(MAPE = mean(ape), hit_10 = mean(ape <= .10), hit_20 = mean(ape <= .20),
         MAPE_透天厝 = bt[["透天厝"]], MAPE_電梯大樓 = bt[["電梯大樓"]], MAPE_華廈 = bt[["華廈"]], MAPE_公寓 = bt[["公寓"]])
}
# 兩次重新訓練的估價差多少 (穩定性)：同一批房子，兩個模型估價的差距 (%)
disagreement <- function(p1_log, p2_log, test) {
  d <- abs(exp(p1_log - p2_log) - 1)
  dt <- tapply(d, as.character(test$b_category), median)
  tibble(跳動_中位數 = median(d), 跳動_P90 = quantile(d, .9, names = FALSE),
         跳動_超過10pct = mean(d > .10), 跳動_透天厝_中位數 = dt[["透天厝"]])
}
model_size_mb <- function(models) {
  ms <- if (inherits(models, "xgb.Booster")) list(models) else models
  sum(sapply(ms, function(m) length(xgb.save.raw(m)))) / 1e6
}

# --- 4. 實驗紀錄 -----------------------------------------------------------------------------
log_experiment <- function(name, note, metrics) {
  row <- bind_cols(tibble(time = format(Sys.time(), "%Y-%m-%d %H:%M"), experiment = name, note = note), metrics)
  f <- "models/experiments.csv"
  write_csv(row, f, append = file.exists(f))
  print(row %>% mutate(across(where(is.numeric), \(x) round(x, 4))), width = 200)
  invisible(row)
}
message("✅ 實驗資料準備好了：訓練 ", nrow(split_data()$train), " 筆、測試 ", nrow(split_data()$test), " 筆")
