# ==============================================================================
# 實驗 05：資料庫裡已經有、但模型還沒用到的欄位
#
# 候選特徵 (都在實價登錄原始資料裡)：
#   zoning_code  都市土地使用分區：住 / 商 / 工 / 農 / 其他 / 非都市 (同一區的透天，商業區通常比住宅區貴)
#   use_code     主要用途：住家用 / 住商用 / 商業用 / 辦公用 / 其他 (店面、住商混合)
#   material     主要建材：鋼骨 / 鋼筋混凝土 / 加強磚造 / 磚木 / 其他 (加強磚造的老屋價值較低)
#   main_ratio   主建物坪數 ÷ 權狀坪數 (= 1 − 公設與附屬比例；大樓單價受公設比影響很大)
#   n_land       交易的土地筆數 (「土地3建物1」：筆數多通常代表面積大或形狀特殊)
#
# 比較對象：正式模型 (features_v3，同種子)。先看單一測試期；有效再跑 4 個時間點的回測確認
# ==============================================================================
source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "new_model_experiments.R"))

# 建材、交易筆棟數只在原始表 raw_trade：用 (season, serial_number) 對回來
con <- db_connect()
raw <- dbGetQuery(con, "SELECT season, serial_number, main_building_materials AS mat, trans_pen_number AS pen
                        FROM re_v2.raw_trade") %>% as_tibble()
dbDisconnect(con)
df <- df %>% left_join(distinct(raw, season, serial_number, .keep_all = TRUE), by = c("season", "serial_number"))

df <- df %>% mutate(
  zoning_code = case_when(zoning == "住" | grepl("住宅區", zoning) ~ 1L, zoning == "商" | grepl("商業區", zoning) ~ 2L,
                          zoning == "工" | grepl("工業區", zoning) ~ 3L, zoning == "農" | grepl("農業區", zoning) ~ 4L,
                          is.na(zoning) | zoning == "" ~ 0L, TRUE ~ 5L),          # 0 = 空白 (多為非都市土地)
  use_code = case_when(grepl("住家", main_use) ~ 1L, grepl("住商", main_use) ~ 2L, grepl("商業", main_use) ~ 3L,
                       grepl("辦公", main_use) ~ 4L, is.na(main_use) | main_use == "" ~ 0L, TRUE ~ 5L),
  material = case_when(grepl("鋼骨", mat) ~ 1L, grepl("鋼筋|RC|ＲＣ", mat) & !grepl("磚", mat) ~ 2L,
                       grepl("磚", mat) ~ 3L, grepl("木|竹|土", mat) ~ 4L, is.na(mat) | mat == "" ~ 0L, TRUE ~ 5L),
  main_ratio = if_else(pure_house_ping > 0 & main_ping > 0, pmin(main_ping / pure_house_ping, 1.5), NA_real_),
  n_land = as.integer(str_match(pen, "土地(\\d+)")[, 2])
)
print(df %>% count(zoning_code)); print(df %>% count(material))

df <- df %>% add_history_features(cap_month = cut_month) %>% add_road_features(build_road_tables())
sp <- split_data(df)
new_feats <- c("zoning_code", "use_code", "material", "main_ratio", "n_land")
base <- predict_xgb(fit_xgb(sp, feats = features_v3, seed = 1), sp$test, feats = features_v3)
log_experiment("05 基準 (正式 v3)", "同種子重跑", score(base, sp$test))
m05 <- fit_xgb(sp, feats = c(features_v3, new_feats), seed = 1)
p05 <- predict_xgb(m05, sp$test, feats = c(features_v3, new_feats))
log_experiment("05 全部新欄位", paste(new_feats, collapse = "/"), score(p05, sp$test))
print(xgb.importance(model = m05) %>% filter(Feature %in% new_feats))
# 逐一拿掉 (ablation)：每個欄位各自貢獻多少？只留有用的，網站要使用者填的欄位越少越好
for (f in new_feats) {
  fs <- c(features_v3, setdiff(new_feats, f))
  p <- predict_xgb(fit_xgb(sp, feats = fs, seed = 1), sp$test, feats = fs)
  log_experiment(paste("05 拿掉", f), "逐一拿掉 (ablation)", score(p, sp$test))
}

# --- 第二輪：只留逐一拿掉時「拿掉會變差」的欄位 (主要用途、建材拿掉反而變好 → 不用) ------------------------
for (fs_new in list(c("zoning_code", "main_ratio", "n_land"), c("zoning_code", "main_ratio"))) {
  fs <- c(features_v3, fs_new)
  p <- predict_xgb(fit_xgb(sp, feats = fs, seed = 1), sp$test, feats = fs)
  log_experiment(paste("05 只留", paste(fs_new, collapse = "+")), "第二輪", score(p, sp$test))
}
print(df %>% count(n_land) %>% mutate(pct = round(100 * n / sum(n), 1)) %>% head(5))
