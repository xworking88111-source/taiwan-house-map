# ==============================================================================
# 資料品質檢查 (Data Quality Checks)：資料進模型之前，先確認它「長得像該有的樣子」
#
# 為什麼需要？
#   每週自動更新沒有人盯著。政府某季檔案缺一半、欄位改名、地理編碼壞掉，
#   程式「不會報錯」，只會默默訓練出一個爛模型。資料品質檢查就是自動化的「驗收」。
#
# 業界的做法：
#   dbt tests (not_null、unique、accepted_values…)、Great Expectations、R 的 pointblank。
#   它們的核心概念都一樣：每條規則 = 一個「找出違規資料」的查詢，違規筆數 > 門檻就失敗。
#   這裡用最直接的方式自己實作 (純 SQL)，概念完全相同，之後換成 dbt 也只是換寫法。
#
# 規則分兩級：
#   error：資料壞了 → stop()，GitHub Actions 停在這一步，不訓練、不部署 (網站繼續用上週的版本)
#   warn ：值得注意但不致命 → 印出來、記錄下來，流程繼續
#
# 每次的結果寫進 re_v2.dq_results (歷史紀錄)，可以追蹤「品質隨時間的變化」
#
# 執行：Rscript new_data_checks.R  (在 ETL、地理編碼、空間特徵之後；訓練之前)
# ==============================================================================
library(DBI)
library(RPostgres)
library(tibble)
source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "config.R"))

# --- 規則清單 ------------------------------------------------------------------
# 每條規則的 sql 回傳「違規的筆數」(一個數字)；max_bad = 允許的最大違規數
rules <- tribble(
  ~rule,                       ~level,  ~max_bad, ~sql,
  # 完整性 (completeness)：該有的資料都要在
  "季數 = 20 季",               "error", 0, "SELECT abs(count(DISTINCT season) - 20) FROM real_estate_clean",
  "22 個縣市都有資料",           "error", 0, "SELECT 22 - count(DISTINCT city) FROM real_estate_clean",
  "最新一季筆數 >= 前幾季中位數的 50%", "error", 0,
    "WITH s AS (SELECT season, count(*) n FROM real_estate_clean GROUP BY 1)
     SELECT (SELECT n FROM s ORDER BY season DESC LIMIT 1) < 0.5 * percentile_cont(0.5) WITHIN GROUP (ORDER BY n)
     FROM s",
  # 唯一性 (uniqueness)：同一筆交易不能出現兩次 (重跑 ETL 沒清乾淨的典型症狀)
  "交易編號不重複",             "error", 0,
    "SELECT count(*) - count(DISTINCT (season, serial_number)) FROM real_estate_clean",
  # 有效性 (validity)：數值要在合理範圍
  "成屋單價 > 0",               "error", 0,
    "SELECT count(*) FROM fact_residential_house WHERE pure_house_unit_price_ping IS NULL OR pure_house_unit_price_ping <= 0",
  "成屋都有鄉鎮市區",           "error", 0,
    "SELECT count(*) FROM fact_residential_house WHERE district IS NULL OR district = ''",
  "屋齡不是負數",               "error", 0,
    "SELECT count(*) FROM fact_residential_house WHERE house_age_at_trans < 0",
  "交易日期不在未來",           "warn",  10,
    "SELECT count(*) FROM real_estate_clean WHERE trans_ym > date_trunc('month', now())",
  "建物型態在已知清單內",       "warn",  0,
    "SELECT count(*) FROM fact_residential_house WHERE b_category NOT IN
       ('電梯大樓','透天厝','華廈','公寓','套房','店面','廠辦工廠','其他','辦公商業','農舍')",
  # 關聯完整性 (referential integrity)：每個門牌都要有座標和空間特徵，否則訓練時 JOIN 會默默丟掉資料
  "成屋都有地理編碼",           "error", 0,
    "SELECT count(*) FROM fact_residential_house f LEFT JOIN geo_cache g USING (addr_key) WHERE g.addr_key IS NULL",
  "成屋都有空間特徵",           "error", 0,
    "SELECT count(*) FROM fact_residential_house f LEFT JOIN geo_features g USING (addr_key) WHERE g.addr_key IS NULL",
  "定位成功率 >= 95%",          "error", 0,
    "SELECT (avg((g.geo_level <> 'none')::int) < 0.95)::int FROM fact_residential_house f JOIN geo_cache g USING (addr_key)"
)

# --- 執行 -----------------------------------------------------------------------
con <- db_connect()
on.exit(dbDisconnect(con))
# DQ_SCHEMA：預設檢查正式的 re_v2；測試時可以指向一個「故意弄壞」的 schema，確認檢查真的會擋 (見學習筆記)
schema <- Sys.getenv("DQ_SCHEMA", "re_v2")
invisible(dbExecute(con, paste0("SET search_path TO ", schema, ";")))
rules$bad <- vapply(rules$sql, function(q) as.numeric(dbGetQuery(con, q)[[1]]), numeric(1))
rules$pass <- rules$bad <= rules$max_bad

for (i in seq_len(nrow(rules))) {
  icon <- if (rules$pass[i]) "✅" else if (rules$level[i] == "error") "❌" else "⚠️"
  message(sprintf("%s [%s] %s：違規 %s (允許 %s)", icon, rules$level[i], rules$rule[i],
                  format(rules$bad[i], big.mark = ","), rules$max_bad[i]))
}

# 結果存成歷史紀錄 (每次檢查附加幾列)
invisible(dbExecute(con, "CREATE TABLE IF NOT EXISTS dq_results (
  checked_at TIMESTAMPTZ, rule TEXT, level TEXT, bad NUMERIC, max_bad NUMERIC, pass BOOLEAN)"))
invisible(dbAppendTable(con, "dq_results", data.frame(checked_at = Sys.time(), rules[c("rule", "level", "bad", "max_bad", "pass")])))

failed <- rules[!rules$pass & rules$level == "error", ]
if (nrow(failed) > 0) stop("資料品質檢查失敗 (", nrow(failed), " 條 error 規則)：", paste(failed$rule, collapse = "、"),
                           " → 停止流程，不訓練、不部署")
message("✅ 資料品質檢查通過 (", sum(rules$pass), "/", nrow(rules), " 條規則通過)")
