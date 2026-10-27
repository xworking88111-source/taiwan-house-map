# ==============================================================================
# 部署 App 到 shinyapps.io (免費方案)
#
# 第一次使用前 (只要做一次)：
#   1. 到 https://www.shinyapps.io 註冊 (可用 GitHub 帳號登入)
#   2. 右上角帳號 → Tokens → Show → 複製三個值：name、token、secret
#   3. 寫進 ~/.Renviron (在 R 執行 usethis::edit_r_environ() 或直接用文字編輯器打開)：
#        SHINYAPPS_NAME=你的帳號
#        SHINYAPPS_TOKEN=xxxx
#        SHINYAPPS_SECRET=xxxx
#        ANTHROPIC_API_KEY=sk-ant-...   (選填，要啟用 AI 複檢才需要)
#      存檔後重新啟動 R。
#   ⚠️ 這些是密碼等級的資訊：只放在 .Renviron，永遠不要寫進程式碼、不要上傳 GitHub。
#
# 之後每次要更新線上版本：source("deploy_app.R")
# GitHub Actions 每週自動更新時，也是跑這支 (密碼從 GitHub Secrets 讀)。
# ==============================================================================
if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")

source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "config.R"))
setwd(project_path)

acct <- Sys.getenv("SHINYAPPS_NAME")
if (!nzchar(acct)) stop("請先在 ~/.Renviron 設定 SHINYAPPS_NAME / SHINYAPPS_TOKEN / SHINYAPPS_SECRET")
rsconnect::setAccountInfo(name = acct,
                          token = Sys.getenv("SHINYAPPS_TOKEN"),
                          secret = Sys.getenv("SHINYAPPS_SECRET"))

# AI 複檢的金鑰：寫進 app/.Renviron，跟著 App 上傳到 shinyapps.io (伺服器端讀得到，使用者看不到)
# app/.Renviron 已列在 .gitignore，不會被上傳到 GitHub
renv_file <- file.path("app", ".Renviron")
if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
  writeLines(paste0("ANTHROPIC_API_KEY=", Sys.getenv("ANTHROPIC_API_KEY")), renv_file)
}

# 套件來源的網址：rsconnect 會告訴 shinyapps.io「每個套件從哪裡下載」。
# GitHub Actions 是從名為「RSPM」的套件庫安裝套件的，如果沒告訴 rsconnect RSPM 的網址，
# 清單裡只會寫「RSPM」這個名字 → shinyapps.io 不知道去哪裡下載 → 安裝失敗
# (錯誤訊息：Unsupported url scheme: RSPM/src/contrib/RANN_2.6.3.tar.gz)
#
# ⚠️ 只設定 options(repos) 不夠：rsconnect 用的是 renv「盤點」時記下的套件庫清單，
#    而 GitHub Actions 的 setup-r 會設定環境變數 RENV_CONFIG_REPOS_OVERRIDE，
#    「強制覆蓋」renv 的清單 → 清單裡沒有 RSPM 這個名稱 → 還是沒有網址。
#    所以要先把這個覆蓋拿掉 (本機沒有這個變數，不受影響)。
Sys.unsetenv("RENV_CONFIG_REPOS_OVERRIDE")
repos <- getOption("repos")
repos["RSPM"] <- "https://packagemanager.posit.co/cran/latest"
if (is.na(repos["CRAN"]) || repos["CRAN"] == "@CRAN@") repos["CRAN"] <- "https://cloud.r-project.org"
options(repos = repos)

# 部署清單 (路徑相對於 app/)
app_files <- c("app.R", "shared_features.R",
               file.path("data", list.files("app/data", pattern = "^trades_.*\\.rds$")),
               "data/meta.rds", "data/condition_adjust.rds", "data/towns.rds", "data/road_lookup.rds",
               "data/facilities.rds", "data/nbr_table.rds",            # v2：空間特徵
               "models/price_model.rds", "models/xgb_point.ubj", "models/xgb_quant.ubj",
               if (file.exists(renv_file)) ".Renviron")
# 部署前檢查：deployApp() 遇到缺檔只會「警告」、照樣部署 → App 上線後一打開就當掉。
# 所以少任何一個檔案就直接停止，讓問題在部署前就被發現
missing <- app_files[!file.exists(file.path("app", app_files))]
if (length(missing) > 0) stop("部署中止，缺少檔案：", paste(missing, collapse = "、"))
if (sum(grepl("^data/trades_", app_files)) != 5) stop("部署中止：交易資料應該有 5 個類別的檔案")

# 部署前驗證套件來源：先產生 shinyapps.io 要看的套件清單 (manifest.json)，
# 確認「每個套件都有完整的下載網址」。沒有網址的套件，shinyapps.io 會在建置時失敗
# (而且要等上傳、排隊好幾分鐘才會看到錯誤) → 在這裡先擋下，並列出是哪些套件
rsconnect::writeManifest(appDir = "app", appFiles = app_files, quiet = TRUE)
manifest <- jsonlite::read_json("app/manifest.json")
unlink("app/manifest.json")                      # 只是用來檢查，deployApp 會自己重新產生
no_url <- Filter(function(p) is.null(p$Repository) || !grepl("^https?://", p$Repository), manifest$packages)
if (length(no_url) > 0) {
  stop("部署中止：這些套件沒有可下載的網址 → ",
       paste(sprintf("%s (Source=%s, Repository=%s)", names(no_url),
                     sapply(no_url, function(p) p$Source %||% "NA"),
                     sapply(no_url, function(p) p$Repository %||% "NA")), collapse = "、"))
}
message("✓ ", length(manifest$packages), " 個套件都有下載網址")

rsconnect::deployApp(
  appDir = "app",
  appName = Sys.getenv("SHINYAPPS_APP_NAME", "taiwan-house-map"),
  appFiles = app_files,
  forceUpdate = TRUE,          # 不要跳出「確定要覆蓋嗎？」的詢問 (自動化時沒有人可以按)
  launch.browser = FALSE
)
