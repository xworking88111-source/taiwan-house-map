# 台灣房價地圖：實價登錄互動地圖與房價預測

> 全台 5 年、169 萬筆不動產交易（成屋、預售屋、預售交屋、土地、車位）：自動化 ETL、免費離線地理編碼、互動式行情地圖、市場探勘圖表，以及附「價格區間」與「AI 複檢」的房價預測。

## 功能

| 功能 | 說明 |
|---|---|
| 🗺️ 行情地圖 | 5 種交易類別；全台 → 縣市 → 行政區面量圖，選了行政區後顯示「每棟建物 / 每個建案」，點擊可以看交易明細 |
| 📊 市場探勘 | 單價趨勢、成交量、建物型態占比、單價分布、行政區排行、屋齡與單價 |
| 🤖 AI 估價 | XGBoost 預測單價與 P10–P90 行情區間；38 個特徵含交通（捷運、輕軌、台鐵、高鐵、機場、公車）、嫌惡設施、生活機能、附近行情；可選屋況與產權條件（影響幅度由實價登錄備註量化）；列出附近相似成交，並產生 AI 複檢提示詞 |
| 🔄 自動更新 | GitHub Actions 每週檢查新資料 → ETL → 重新訓練 → 部署 |

## 架構

```
內政部實價登錄 (季資料 ZIP)
      │  new_All_data_ETL.R：下載、標準化、以季為單位增量寫入、保留最近 20 季
      ▼
PostgreSQL (schema re_v2)
  raw_trade ──SQL 清洗──▶ real_estate_clean ──▶ fact_residential_house / fact_presale_delivered / fact_land_only / fact_parking_only
  raw_presale ──SQL 清洗──▶ fact_presale_contract
  geo_cache ◀── new_geocode.R：OSM 道路 + 鄉鎮界線離線比對
      │
      ├─▶ new_model_training.R：基準 / 線性迴歸 / 隨機森林 / XGBoost + 分位數迴歸 + 保形校正
      └─▶ new_export_app_data.R：匯出精簡資料 (約 28MB)
                │
                ▼
          app/app.R (Shiny + bslib + Leaflet + Plotly) ──deploy_app.R──▶ shinyapps.io
```

## 資料管線成果

| 項目 | 數字 |
|---|---|
| 資料期間 | 110S4 ～ 115S3（2021-10 ～ 2026-09，共 20 季） |
| 原始交易 | a 檔（買賣）1,798,230 筆 + b 檔（預售屋契約）430,744 筆 |
| App 資料 | 1,686,026 筆：成屋 73 萬、預售屋 39 萬、土地 34 萬、預售交屋 21 萬、車位 1.9 萬 |
| 地理編碼 | 56.5 萬個地址，93% 精準到路段或巷弄；增量更新只處理新地址 |

## 模型成績

測試集是最近 6 個月的交易，模型訓練時完全沒看過：

| 模型 | MAPE | 誤差 ±10% 內 | 誤差 ±20% 內 |
|---|---|---|---|
| 基準：同區同型態中位數 | 24.5% | 25.9% | 49.9% |
| 線性迴歸（特徵價格模型） | 23.9% | 33.1% | 58.4% |
| 隨機森林 | 14.1% | 52.0% | 79.0% |
| **XGBoost** | **13.0%** | **55.1%** | **82.1%** |

- 價格區間：XGBoost 分位數迴歸搭配保形校正（Conformalized Quantile Regression），P10–P90 區間的實際涵蓋率為 **80.2%**。
- 滾動回測（4 個不同上線時間點 + 12 個月測試）：MAPE 穩定在 12.9%–14.3%，預測 12 個月的誤差和 6 個月相同（`new_model_backtest.R`）。
- 依建物型態的 MAPE：電梯大樓 9.1%、華廈 11.8%、公寓 16.7%、透天厝 19.5%。

## 如何執行

```bash
# macOS 啟動 PostgreSQL（需指定語系，否則會啟動失敗）
LC_ALL=en_US.UTF-8 pg_ctl -D /Volumes/Dzzz_ssd/postgres_data -l /Volumes/Dzzz_ssd/postgres_data/logfile start
```

```r
source("new_All_data_ETL.R")      # 約 3 分鐘（第一次含下載）
source("new_geocode.R")           # 約 30 秒（第一次會下載約 260MB 的 OSM 圖資）
source("new_model_training.R")    # 約 20 分鐘
source("new_export_app_data.R")   # 約 1 分鐘
shiny::runApp("app")              # 本機預覽
source("deploy_app.R")            # 部署到 shinyapps.io（需先設定帳號，見檔案開頭說明）
```

路徑與資料庫連線設定集中在 `config.R`，可以用環境變數覆寫（GitHub Actions 就是這樣用的）。

## 資料來源與授權

- 不動產成交資料：[內政部不動產交易實價查詢服務網](https://plvr.land.moi.gov.tw/)（政府資料開放授權條款）
- 道路圖資：© [OpenStreetMap](https://www.openstreetmap.org/copyright) contributors（ODbL），經 Geofabrik 下載
- 鄉鎮市區界線：內政部國土測繪中心，經 [taiwan-atlas](https://github.com/dkaoster/taiwan-atlas) 整理
- 底圖：Esri World Gray Canvas

估價結果僅供參考，不構成任何買賣或投資建議。

## 技術

R · PostgreSQL · SQL · sf (GIS) · XGBoost · ranger · Shiny · bslib · Leaflet · Plotly · Claude API · GitHub Actions
