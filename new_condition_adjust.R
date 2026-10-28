# ==============================================================================
# 屋況 / 產權條件對房價的影響：用實價登錄「備註」的文字量化
#
# 問題：模型不知道「有沒有裝潢」「屋況好不好」「是不是凶宅」(這些沒有獨立欄位)，
#       所以模型的估價代表「一般屋況」的行情。使用者想知道：如果有裝潢 / 需要大整修，價格差多少？
#
# 做法 (從非結構化文字萃取特徵 + 殘差分析)：
#   1. 從備註抓關鍵字分組：含裝潢、毛胚、屋況不佳、帶租約、凶宅、海砂/輻射屋
#   2. 交叉預測 (out-of-fold, OOF)：資料分 3 份，每次用 2 份訓練、預測第 3 份。
#      → 每筆交易的預測都來自「沒看過這筆交易」的模型，殘差才誠實。
#      分份時以「建物」為單位 (同一棟的交易在同一份)，避免同一棟的鄰居交易洩漏答案。
#   3. 殘差 = 實際價格 / 模型預測 - 1。某一組的殘差中位數 = 這個條件讓價格偏離「一般行情」多少
#   4. 扣掉「備註沒有任何特殊條件」那一組的殘差 (基準)，得到淨影響
#
# 注意：凶宅、海砂屋原本被當成「特殊交易」排除在模型訓練之外。
#       這裡把它們撈回來「只拿來量影響、不拿來訓練」(訓練集永遠只用一般交易)。
#
# 產出：models/condition_adjust.csv (給人看)、app/data/condition_adjust.rds (App 用)
# 執行時間：約 10 分鐘
# ==============================================================================
library(xgboost)
# 共用函式庫：清理、特徵工程、特徵清單、XGBoost 參數 (和訓練程式同一份，見 R/model_lib.R)
source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "R", "model_lib.R"))
setwd(project_path)
# 輸出用的資料夾先建好。app/data 在 .gitignore 裡，GitHub Actions 的乾淨主機上一開始不存在
# (第一次在雲端跑時，就是因為這裡沒建資料夾而失敗；本機剛好已經有這個資料夾，所以沒發現)
dir.create("models", showWarnings = FALSE)
dir.create("app/data", recursive = TRUE, showWarnings = FALSE)
set.seed(2026)

# --- 1. 讀資料：一般交易 + 凶宅/海砂屋 (含備註) --------------------------------
# 欄位和 new_model_training.R 的 load_training_data() 一樣，多了 note、addr_key、is_special_deal。
# 純房屋單價等欄位的算法和 new_All_data_ETL.R 的 fact_residential_house 一樣。
con <- db_connect()
dbExecute(con, "SET search_path TO re_v2;")
fac_sql <- paste0("gf.d_", dist_cols, collapse = ", ")
cnt_sql <- paste0("gf.n_", count_cols, "_500", collapse = ", ")
df_raw <- dbGetQuery(con, paste0("
  WITH f AS (
    SELECT *,
      CASE WHEN include_parking = '是' AND parking_price > 0 THEN total_price - parking_price
           ELSE total_price END AS pure_house_total_price,
      CASE WHEN include_parking = '是' AND parking_ping > 0 THEN total_ping - parking_ping
           ELSE total_ping END AS pure_house_ping,
      CASE WHEN include_parking = '是' AND parking_price > 0 AND (total_ping - COALESCE(parking_ping, 0)) > 0
           THEN ROUND((total_price - parking_price) / (total_ping - COALESCE(parking_ping, 0)), 0)
           ELSE unit_price_ping END AS pure_house_unit_price_ping,
      (include_parking = '是' AND COALESCE(parking_price, 0) = 0) AS parking_price_unknown
    FROM real_estate_clean
    WHERE trading_type = '房地產成屋' AND total_ping > 0 AND unit_price_ping > 10000
      AND (is_special_deal = '否' OR note ~ '凶宅|兇宅|非自然|事故|海砂|輻射')
  )
  SELECT f.city, f.district, f.b_category, f.trans_ym,
         f.house_age_at_trans, f.pure_house_ping, f.land_ping, f.main_ping,
         f.floor_num, f.total_floors_num, f.rooms, f.halls, f.baths,
         f.has_elevator, f.has_management, f.include_parking, f.parking_price_unknown,
         f.pure_house_unit_price_ping, f.pure_house_total_price,
         g.lng, g.lat, g.geo_level, gf.x AS tx, gf.y AS ty, ", fac_sql, ", ", cnt_sql, ",
         f.note, f.addr_key, f.address_norm, f.is_special_deal
  FROM f
  JOIN geo_cache g ON f.addr_key = g.addr_key
  JOIN geo_features gf ON f.addr_key = gf.addr_key
  WHERE f.b_category IN ('透天厝', '電梯大樓', '華廈', '公寓', '套房')
    AND f.house_age_at_trans IS NOT NULL AND g.geo_level <> 'none';")) %>% as_tibble()
dbDisconnect(con)

# --- 2. 清理、特徵工程：呼叫共用函式庫 (和訓練程式同一套規則) ------------------------------
df <- df_raw %>% clean_trades() %>% engineer_features()      # 產生 y = log 單價等欄位
fac_levels <- make_fac_levels(df)
# 交叉預測不模擬「上線當下」→ cap_month = Inf；臨路等級和時間無關
df <- df %>% add_history_features(cap_month = Inf) %>% add_road_features(build_road_tables())

# --- 3. 備註關鍵字分組 ------------------------------------------------------------
# 一筆交易可能同時屬於多組 (例如含裝潢 + 帶租約)，所以每組各自一個 TRUE/FALSE 欄位
note <- coalesce(df$note, "")
# ⚠️ 制式免責條款：例如「惟若重大瑕疵除外如：非自然身故、輻射屋、海砂屋」(數百筆)，
#    意思是「如果是凶宅賣方要負責」，並不代表這間是凶宅！第一版沒排除，凶宅組 457 筆裡大多是這種，
#    量出來的影響 (-7%) 被嚴重稀釋。所以出現這些法律用語的備註，不算進凶宅/海砂/屋況不佳。
boilerplate <- "除外|不負|擔保|保固|不論|包括但不限|不得排除|概括承受|是否有"
conds <- tibble(
  含裝潢     = str_detect(note, "含裝潢|有裝潢|裝潢設備|含傢俱|含家具|含家電|加裝潢費") &
               !str_detect(note, "不含裝潢|未含裝潢|無裝潢|非屬不動產"),
  毛胚       = str_detect(note, "毛胚"),
  屋況不佳   = str_detect(note, "屋況(問題|需整理|不佳|待修|非常差|差)|漏水|壁癌|龜裂|不堪居住|需再行整修|老舊不堪") &
               !str_detect(note, boilerplate),
  帶租約     = str_detect(note, "租約|帶租"),
  凶宅       = str_detect(note, "凶宅|兇宅|非自然|事故屋|非疾病") & !str_detect(note, boilerplate),
  海砂輻射   = str_detect(note, "海砂|輻射") & !str_detect(note, boilerplate)
)
df <- bind_cols(df, conds)
df$no_cond <- rowSums(conds) == 0
df$type_grp <- if_else(df$b_category == "套房", "電梯大樓", as.character(df$b_category))
message("各條件筆數：", paste(names(conds), colSums(conds), sep = "=", collapse = "，"))

# --- 4. 交叉預測 (3 份，以建物為單位分) -------------------------------------------
keys <- unique(df$addr_key)
fold_of <- setNames(sample(rep(1:3, length.out = length(keys))), keys)
df$fold <- fold_of[df$addr_key]
df$pred <- NA_real_
for (k in 1:3) {
  # 訓練：其他 2 份裡的「一般交易」(凶宅、海砂屋永遠不進訓練集)
  tr <- df %>% filter(fold != k, is_special_deal == "否")
  va <- runif(nrow(tr)) < 0.05                                 # 5% 當驗證集 (early stopping)
  fit <- xgb.train(params = xgb_params,
                   data = xgb.DMatrix(to_matrix(tr[!va, ], features, fac_levels), label = tr$y[!va]), nrounds = 3000,
                   evals = list(valid = xgb.DMatrix(to_matrix(tr[va, ], features, fac_levels), label = tr$y[va])),
                   early_stopping_rounds = 100, verbose = 0)
  idx <- which(df$fold == k)
  df$pred[idx] <- predict(fit, to_matrix(df[idx, ], features, fac_levels))
  message(">> 第 ", k, " 份完成 (樹的數量 ", xgb.attr(fit, "best_iteration"), ")")
}
df$resid <- exp(df$y) / exp(df$pred) - 1                      # 實際 / 預測 - 1
# 存下每筆交易的殘差：之後調整關鍵字規則，可以直接讀這個檔重算，不用重跑 10 分鐘的模型
saveRDS(df %>% select(addr_key, b_category, type_grp, note, is_special_deal, resid), "models/condition_residuals.rds")

# --- 5. 各條件的影響 (相對於「沒有任何特殊條件」的一般交易) ---------------------------
base <- df %>% filter(no_cond, is_special_deal == "否") %>%
  group_by(type_grp) %>% summarise(base_med = median(resid), .groups = "drop")
base_all <- median(df$resid[df$no_cond & df$is_special_deal == "否"])

adj <- map_dfr(names(conds), function(cn) {
  g <- df[df[[cn]], ]
  by_type <- g %>% group_by(type_grp) %>%
    summarise(n = n(), med = median(resid), q25 = quantile(resid, .25), q75 = quantile(resid, .75), .groups = "drop") %>%
    left_join(base, by = "type_grp") %>%
    mutate(adj = (1 + med) / (1 + base_med) - 1)
  pooled <- tibble(type_grp = "全部", n = nrow(g), med = median(g$resid),
                   q25 = quantile(g$resid, .25), q75 = quantile(g$resid, .75),
                   base_med = base_all, adj = (1 + median(g$resid)) / (1 + base_all) - 1)
  bind_rows(by_type, pooled) %>% mutate(condition = cn, .before = 1)
})
print(adj %>% select(condition, type_grp, n, adj, q25, q75) %>%
        mutate(across(c(adj, q25, q75), \(x) scales::percent(x, 0.1))), n = 100)

# 樣本太少 (< 30 筆) 的型態，改用「全部型態合併」的影響 (並標記)，避免幾筆極端交易決定結果
final <- adj %>% filter(type_grp != "全部") %>%
  left_join(adj %>% filter(type_grp == "全部") %>% select(condition, pooled_adj = adj, pooled_n = n), by = "condition") %>%
  mutate(use_pooled = n < 30, adj_final = if_else(use_pooled, pooled_adj, adj), n_final = if_else(use_pooled, pooled_n, n),
         # 連「全部型態合併」都不到 30 筆 → 樣本不足，App 不給百分比，只提醒使用者另行評估
         insufficient = n_final < 30)
dir.create("models", showWarnings = FALSE)
write_csv(adj, "models/condition_adjust.csv")
saveRDS(final %>% select(condition, type_grp, adj = adj_final, n = n_final, use_pooled, insufficient),
        "app/data/condition_adjust.rds")
message("✅ 已存檔：models/condition_adjust.csv、app/data/condition_adjust.rds")
