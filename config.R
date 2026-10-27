# ==============================================================================
# 共用設定：專案路徑、資料庫連線
#
# 為什麼要獨立成一個檔案？
#   1. 不重複 (DRY = Don't Repeat Yourself)：四支程式都要連資料庫，改一個地方就好。
#   2. 同一份程式碼要在「你的 Mac」和「GitHub Actions 的雲端主機」兩個地方跑，
#      兩邊的路徑、資料庫帳號都不一樣。
#      做法：優先讀「環境變數」，沒有設定時才用你 Mac 的預設值。
#      這就是業界常說的「設定與程式碼分離」(12-Factor App 原則之一)。
#   3. 密碼永遠不寫在程式碼裡：要密碼時放在環境變數 PGPASSWORD (本機可寫在 ~/.Renviron)。
# ==============================================================================
project_path <- Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data")

db_connect <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = Sys.getenv("PGDATABASE", "postgres"),
    host     = Sys.getenv("PGHOST", "localhost"),
    port     = as.integer(Sys.getenv("PGPORT", "5432")),
    user     = Sys.getenv("PGUSER", "dzzz"),
    password = Sys.getenv("PGPASSWORD", "")      # 本機沒有密碼時是空字串
  )
}
