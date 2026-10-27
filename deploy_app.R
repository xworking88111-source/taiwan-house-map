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

rsconnect::deployApp(
  appDir = "app",
  appName = Sys.getenv("SHINYAPPS_APP_NAME", "taiwan-house-map"),
  appFiles = c("app.R", "shared_features.R",
               list.files("app/data", pattern = "^trades_.*\\.rds$", full.names = FALSE) |> (\(f) file.path("data", f))(),
               "data/meta.rds", "data/condition_adjust.rds", "data/towns.rds", "data/road_lookup.rds",
               "data/facilities.rds", "data/nbr_table.rds",            # v2：空間特徵
               "models/price_model.rds", "models/xgb_point.ubj", "models/xgb_quant.ubj",
               if (file.exists(renv_file)) ".Renviron"),
  forceUpdate = TRUE,          # 不要跳出「確定要覆蓋嗎？」的詢問 (自動化時沒有人可以按)
  launch.browser = FALSE
)
