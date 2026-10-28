# 測試開始前執行一次：把「特徵函式庫」載入成函數 (不執行它的主程式)
# features.as_library = TRUE → new_features.R 最後的主程式區塊不會跑 (不會連資料庫、不會讀 OSM)
options(features.as_library = TRUE)
suppressMessages(source(file.path(Sys.getenv("RE_PROJECT_PATH", "../.."), "new_features.R")))
