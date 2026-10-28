# ==============================================================================
# 執行所有自動化測試：Rscript tests/run_tests.R
# 任何一個測試失敗 → 結束碼不是 0 → GitHub Actions 的這一步變紅燈、後面的步驟不會執行
# (stop_on_failure = TRUE：這就是 CI「測試沒過就不准部署」的機制)
# ==============================================================================
Sys.setenv(RE_PROJECT_PATH = Sys.getenv("RE_PROJECT_PATH", normalizePath(".")))
testthat::test_dir("tests/testthat", reporter = testthat::ProgressReporter$new(show_praise = FALSE),
                   stop_on_failure = TRUE)
