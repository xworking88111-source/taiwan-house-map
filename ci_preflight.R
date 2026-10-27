# ==============================================================================
# 部署前檢查 (GitHub Actions 的「事前檢查」工作會執行；本機也可以執行)
#
# 為什麼需要？第一次在 GitHub Actions 部署時，連續遇到這些問題，每一個都要等到最後才失敗：
#   - 金鑰作廢 (401)、缺少 cpp11 套件、套件沒有下載網址 (Unsupported url scheme: RSPM/...)
# 把這些檢查放在流程最前面：有問題 3 分鐘內就失敗，不用等 ETL、訓練跑完 (fail fast)
#
# 檢查項目：
#   1. shinyapps.io 金鑰有效
#   2. App 需要的套件都已安裝
#   3. 每個套件都有 shinyapps.io 能下載的網址 (https://...)
# ==============================================================================

# 和 deploy_app.R 相同的套件庫設定 (說明見 deploy_app.R)
Sys.unsetenv("RENV_CONFIG_REPOS_OVERRIDE")
repos <- getOption("repos")
repos["RSPM"] <- "https://packagemanager.posit.co/cran/latest"
if (is.na(repos["CRAN"]) || repos["CRAN"] == "@CRAN@") repos["CRAN"] <- "https://cloud.r-project.org"
options(repos = repos)

# 1. 金鑰
rsconnect::setAccountInfo(Sys.getenv("SHINYAPPS_NAME"), Sys.getenv("SHINYAPPS_TOKEN"), Sys.getenv("SHINYAPPS_SECRET"))
cat("✓ shinyapps.io 金鑰有效\n")

# 2. 套件都已安裝
code_files <- c("app.R", "shared_features.R")
d <- rsconnect::appDependencies("app", appFiles = code_files)
cat("✓ App 需要的", nrow(d), "個套件都已安裝\n")

# 3. 每個套件都有下載網址 (只用程式碼檔產生清單，不需要資料檔)
check_manifest_urls <- function(app_files) {
  rsconnect::writeManifest(appDir = "app", appFiles = app_files, quiet = TRUE)
  manifest <- jsonlite::read_json("app/manifest.json")
  unlink("app/manifest.json")
  no_url <- Filter(function(p) is.null(p$Repository) || !grepl("^https?://", p$Repository), manifest$packages)
  if (length(no_url) > 0) {
    stop("這些套件沒有可下載的網址 → ",
         paste(sprintf("%s (Source=%s, Repository=%s)", names(no_url),
                       sapply(no_url, function(p) p$Source %||% "NA"),
                       sapply(no_url, function(p) p$Repository %||% "NA")), collapse = "、"))
  }
  length(manifest$packages)
}
cat("✓", check_manifest_urls(code_files), "個套件都有下載網址\n")
