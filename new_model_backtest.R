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
# 這支程式只用 XGBoost (最準、最快)；隨機森林太慢，不適合跑很多次。
# 執行時間：約 15~30 分鐘
# ==============================================================================
library(tidyverse)
library(xgboost)

# --- 1. 直接沿用訓練程式的「讀資料、清理、特徵工程」---------------------------
# 不複製程式碼 (複製會造成兩邊不一致)，而是讀取 new_model_training.R，
# 只執行它的第 1~3 節 (到「# --- 4.」為止)，以及 prep_x / to_matrix 等函數定義。
source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "config.R"))
setwd(project_path)
src <- readLines("new_model_training.R")
eval(parse(text = src[1:(grep("^# --- 4\\.", src) - 1)]))               # 讀資料、清理、特徵工程 → df
# 用 R 的語法解析器 parse() 把整個檔案拆成「一個個運算式」，只執行「名稱符合」的賦值。
# (不能用「到下一個空行為止」來判斷：prep_x 定義後面緊接著隨機森林的訓練程式，會被一起執行)
exprs <- parse("new_model_training.R")
take <- function(name) {
  hit <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(name)), exprs)
  if (length(hit) == 0) stop("在 new_model_training.R 找不到 ", name)
  eval(hit[[1]], globalenv())
}
for (nm in c("features_v1", "features", "fac_levels", "prep_x", "to_matrix", "xgb_params")) take(nm)
features_v2 <- features
df_base <- df

# --- 2. 回測 ------------------------------------------------------------------
backtest_one <- function(cut, test_len, train_months = Inf, feature_set = "v2") {
  # 附近行情要「每個切點重算」：時間窗最多只能用到切點 (切點之後的成交在上線當下還不存在)
  df <- bind_cols(df_base, neighbor_price(df_base$tx, df_base$ty, as.character(df_base$b_category),
                                          df_base$t_month_idx, df_base$y, cap_month = cut))
  features <<- if (feature_set == "v1") features_v1 else features_v2   # prep_x() 讀的是全域的 features
  train <- df %>% filter(t_month_idx <= cut, t_month_idx > cut - train_months)
  test  <- df %>% filter(t_month_idx > cut, t_month_idx <= cut + test_len)
  # 訓練集最後 3 個月當驗證集 (early stopping 用)
  valid_idx <- train$t_month_idx > (cut - 3)
  dtrain <- xgb.DMatrix(to_matrix(train[!valid_idx, ]), label = train$y[!valid_idx])
  dvalid <- xgb.DMatrix(to_matrix(train[valid_idx, ]),  label = train$y[valid_idx])
  fit <- xgb.train(params = xgb_params, data = dtrain, nrounds = 3000,
                   evals = list(valid = dvalid), early_stopping_rounds = 100, verbose = 0)
  pred <- predict(fit, to_matrix(test))
  ape <- abs(exp(pred) - exp(test$y)) / exp(test$y)
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
) %>% tidyr::crossing(feature_set = c("v1", "v2"))
results <- pmap_dfr(plan, function(cut, test_len, train_months, feature_set) {
  message(">> [", feature_set, "] 切點 ", month_label(cut), "，測試 ", test_len, " 個月")
  backtest_one(cut, test_len, train_months, feature_set)
}) %>% mutate(cut = month_label(cut))

print(results %>% select(features, cut, test_months, MAPE, hit_10, MAPE_透天厝, MAPE_電梯大樓, MAPE_華廈, MAPE_公寓) %>%
        mutate(across(starts_with(c("MAPE", "hit")), \(x) scales::percent(x, 0.1))), width = 200)
dir.create("models", showWarnings = FALSE)
write_csv(results, "models/backtest_results_v2.csv")

# --- 3. 怎麼解讀 --------------------------------------------------------------
# - 前 4 列 (6 個月測試) 的 MAPE 如果都在 12~15% 之間 → 模型穩定，可以信任。
# - 第 5 列 (12 個月測試) 比第 3 列 (同切點、6 個月) 高很多 → 模型會隨時間「老化」，
#   應該固定頻率重新訓練 (GitHub Actions 每週更新時已經會重訓)。
# - 第 6 列 (只用 1 年資料) 比第 4 列 (全部資料) 差 → 舊資料有幫助，保留 5 年是對的。
