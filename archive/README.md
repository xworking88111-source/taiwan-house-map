# archive/：專案初期的原始版本 (保留不修改)

這裡是改寫成 `new_*.R` 之前的原始程式，保留下來做對照 (依專案規則：原檔不覆蓋、修改版另存為 `new_原檔名.R`)。
這些檔案**不在**自動化流程裡執行。

| 檔案 | 用途 | 取代它的新版 |
|---|---|---|
| `All_data_ETL.R` | 第一版 ETL (寫入 public schema) | `new_All_data_ETL.R` (re_v2 schema、增量、冪等) |
| `房屋資訊地圖.R` | 第一版 Shiny 地圖 | `app/app.R` |
| `Adjusted_app.R` | 第一版 App 的調整版 | `app/app.R` |
| `test_SQL.R` | 測試資料庫連線的小程式 | `config.R` 的 `db_connect()` |

> 如果需要重建舊的 public schema 資料表：`All_data_ETL.R` 第 188–352 行的 SQL (見學習筆記第二輪)。
