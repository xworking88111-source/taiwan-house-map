# ==============================================================================
# 實驗 01：估價穩定性 (每次重新訓練，同一間房子的估價會跳多少？)
#
# 現象：同一份資料、同一支程式，本機訓練的模型估鳳山某透天 23.2 萬/坪，雲端訓練的估 20.2 萬/坪。
# 原因推測：XGBoost 每棵樹隨機抽 80% 資料、80% 欄位 (subsample / colsample)，
#           再加上不同電腦的多執行緒計算順序不同 → 每次訓練出來的樹都不一樣。
#
# 量測方式：同一份資料，只換隨機種子，訓練兩次 → 比較兩個模型對「測試集同一批房子」的估價差距
#   (換種子 ≈ 換一台電腦重新訓練；這就是使用者每週會看到的跳動)
#
# 比較的方案 (樹的總數都控制在 1500 棵左右，因為免費主機記憶體只有 1GB)：
#   A. 現行：1 個模型 × 1500 棵 (學習率 0.1)
#   B. 多模型平均：5 個模型 × 300 棵 (學習率 0.3)，估價取平均
#      概念 = bagging：每個模型的隨機誤差方向不同，平均起來會互相抵消
# ==============================================================================
source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "new_model_experiments.R"))
sp <- split_data()
t0 <- Sys.time()

# --- A. 現行設定，種子 1 和 2 -----------------------------------------------------------
a1 <- fit_xgb(sp, seed = 1); a2 <- fit_xgb(sp, seed = 2)
pa1 <- predict_xgb(a1, sp$test); pa2 <- predict_xgb(a2, sp$test)
message("A 完成，樹的數量 ", xgb.get.num.boosted.rounds(a1), "、", xgb.get.num.boosted.rounds(a2),
        "，耗時 ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " 分")
log_experiment("01A 現行 1×1500 (種子1)", "eta 0.1", bind_cols(score(pa1, sp$test), disagreement(pa1, pa2, sp$test),
                                                         tibble(模型大小MB = model_size_mb(a1))))

# --- B. 5 個模型平均：兩組獨立的 ensemble (種子 1~5 vs 6~10) ----------------------------
p_fast <- modifyList(xgb_params, list(eta = 0.3))
ens_b1 <- lapply(1:5,  function(s) fit_xgb(sp, params = p_fast, nrounds = 300, seed = s))
ens_b2 <- lapply(6:10, function(s) fit_xgb(sp, params = p_fast, nrounds = 300, seed = s))
pb1 <- predict_xgb(ens_b1, sp$test); pb2 <- predict_xgb(ens_b2, sp$test)
log_experiment("01B 平均 5×300", "eta 0.3，兩組 ensemble 互比", bind_cols(score(pb1, sp$test), disagreement(pb1, pb2, sp$test),
                                                                     tibble(模型大小MB = model_size_mb(ens_b1))))
message("總耗時 ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " 分")
saveRDS(list(pa1 = pa1, pa2 = pa2, pb1 = pb1, pb2 = pb2), "models/exp01_preds.rds")
