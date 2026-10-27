# ==============================================================================
# 實驗 01b：多模型平均的其他配置 (樹的總數一樣控制在 1500 棵左右)
#   C. 10 個模型 × 150 棵 (學習率 0.3)：平均的模型越多，隨機誤差抵消越多
#   D. 5 個模型 × 300 棵 (學習率 0.2)：每個模型比較精細，看準確度能不能補回來
# 結果見 models/experiments.csv；A、B 見 01_stability.R
# ==============================================================================
source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "new_model_experiments.R"))
sp <- split_data()

run_ensemble_pair <- function(name, n_models, nrounds, eta) {
  p <- modifyList(xgb_params, list(eta = eta))
  e1 <- lapply(seq_len(n_models),              function(s) fit_xgb(sp, params = p, nrounds = nrounds, seed = s))
  e2 <- lapply(n_models + seq_len(n_models),   function(s) fit_xgb(sp, params = p, nrounds = nrounds, seed = s))
  p1 <- predict_xgb(e1, sp$test); p2 <- predict_xgb(e2, sp$test)
  log_experiment(name, sprintf("eta %.1f，兩組 ensemble 互比", eta),
                 bind_cols(score(p1, sp$test), disagreement(p1, p2, sp$test), tibble(模型大小MB = model_size_mb(e1))))
}
run_ensemble_pair("01C 平均 10×150", 10, 150, 0.3)
run_ensemble_pair("01D 平均 5×300", 5, 300, 0.2)
