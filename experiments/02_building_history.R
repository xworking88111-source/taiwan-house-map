# ==============================================================================
# 實驗 02：同棟歷史成交 (這棟建物「以前」賣多少？)
#
# 直覺：同一棟大樓、同一間透天，過去的成交價是最強的參考 —— 估價師也是先查「同社區近期成交」。
# 做法：對每一筆交易，找「同一個門牌 (addr_key，不含樓層)」在它之前 36 個月內的成交，算出：
#   bld_lp = 這些成交 log(單價) 的平均     (同棟行情)
#   bld_n  = 筆數                          (行情可信度：1 筆和 30 筆差很多)
#   bld_dt = 這些成交距今平均幾個月         (讓模型自己學「舊行情要加多少漲幅」)
#
# ⚠️ 防止資料洩漏 (data leakage)：
#   1. 只能看「更早的月份」(嚴格小於)，同月份也不行 —— 實務上估價當下看不到同月還沒登錄的成交
#   2. 測試期的房子，只能看到 cut_month (訓練截止月) 以前的成交，和 nbr_* 鄰近行情的規則一致
#   如果不小心讓模型看到「自己」的價格，回測會漂亮得不可思議，上線後卻完全不準
#
# 比較對象：01A 設定 (1×1500，使用者選擇準確度優先)，同種子重跑基準，唯一差別是多了 bld_* 三個特徵
# ==============================================================================
source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "new_model_experiments.R"))
library(data.table)

# building_history() 定義在 new_features.R (訓練特徵函式庫)

t0 <- Sys.time()
bh <- building_history(df$addr_key, df$t_month_idx, df$y, cap_month = cut_month)
df <- bind_cols(df, bh)
message("同棟歷史計算 ", round(as.numeric(Sys.time() - t0, units = "secs")), " 秒；",
        "有同棟紀錄的比例：全部 ", scales::percent(mean(df$bld_n > 0), .1),
        "、測試期 ", scales::percent(mean(df$bld_n[df$t_month_idx > cut_month] > 0), .1))
print(df %>% filter(t_month_idx > cut_month) %>% group_by(b_category) %>%
        summarise(有同棟紀錄 = mean(bld_n > 0), 平均筆數 = mean(bld_n)))

# 洩漏檢查：同棟有紀錄的資料，bld_lp 和自己的 y 相關係數應該高但「不是 1」
message("洩漏檢查 cor(bld_lp, y) = ", round(cor(df$bld_lp, df$y, use = "complete.obs"), 3))

sp <- split_data(df)
# 基準 = 01A 設定 (1 個模型 × 1500 棵，準確度優先)，兩邊同一個種子、同一份資料順序，唯一差別是特徵
feats02 <- c(features_base, "bld_lp", "bld_n", "bld_dt")
base <- predict_xgb(fit_xgb(sp, feats = features_base, seed = 1), sp$test, feats = features_base)
log_experiment("02 基準 1×1500", "01A 設定、資料固定排序後重跑", bind_cols(score(base, sp$test)))
m02  <- fit_xgb(sp, feats = feats02, seed = 1)
pred <- predict_xgb(m02, sp$test, feats = feats02)
log_experiment("02 同棟歷史成交 1×1500", "基準 + bld_lp/bld_n/bld_dt (36 個月)",
               bind_cols(score(pred, sp$test), tibble(模型大小MB = model_size_mb(m02))))

# 分組看效果：有同棟紀錄 vs 沒有 (沒有紀錄的房子，理論上不會變好也不該變差)
ape <- function(p) abs(exp(p) - exp(sp$test$y)) / exp(sp$test$y)
print(tibble(有同棟 = sp$test$bld_n > 0, 類別 = sp$test$b_category, 原本 = ape(base), 加同棟 = ape(pred)) %>%
        group_by(有同棟, 類別) %>% summarise(n = n(), 原本MAPE = mean(原本), 加同棟MAPE = mean(加同棟), .groups = "drop"))
print(xgb.importance(model = m02) %>% head(10))
saveRDS(list(base = base, pred = pred, test_key = sp$test$addr_key), "models/exp02_preds.rds")
