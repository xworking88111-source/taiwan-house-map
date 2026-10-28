# ==============================================================================
# 訓練與網站一致性測試 (training-serving skew test)
#
# 問題：模型訓練時用「訓練程式」算特徵，網站估價時用「網站程式」算特徵。
#       兩邊只要有一個欄位算法不同，模型就會看錯資料，而且「不會報錯」，只會估得不準。
#       這個專案踩過兩次：土地持分 (網站給 -1 → 低估 25%)、inp$road 名稱衝突。
# 做法：
#   1. 訓練時挑 300 筆最新月份的真實成交，用訓練算法算出模型輸入，存成標準答案
#      (R/model_lib.R 的 make_serving_fixture → app/models/serving_check.rds)
#   2. 這支程式把同樣的條件 (縣市、地址、坪數、屋齡…) 餵給網站的 make_estimate_input()
#      再經過網站的 build_model_input()，得到網站版的模型輸入
#   3. 逐欄比較：任何一欄有超過 2% 的房子不一致 → 測試失敗 → 不部署
#
# 執行：Rscript tests/serving_consistency.R  (需要 app/data、app/models；GitHub Actions 在部署前執行)
# ==============================================================================
root <- Sys.getenv("RE_PROJECT_PATH", normalizePath("."))
setwd(file.path(root, "app"))                     # app.R 用相對路徑讀 data/、models/
suppressMessages(suppressWarnings(source("app.R", local = TRUE)))   # 只載入函數和資料；最後的 shinyApp() 不會啟動伺服器
fx <- readRDS("models/serving_check.rds")
stopifnot(identical(fx$features, model$features))   # 標準答案和網站的模型必須是同一版

r <- fx$rows
# 使用者會怎麼輸入地址：完整門牌 (含樓層)，去掉縣市、區 → 和網站的輸入框一樣
addr <- mapply(function(a, c, d) sub(d, "", sub(c, "", a, fixed = TRUE), fixed = TRUE), r$address_norm, r$city, r$district)
x_app <- t(vapply(seq_len(nrow(r)), function(i) {
  e <- make_estimate_input(city = r$city[i], district = r$district[i], road = addr[i], type = r$b_category[i],
                           age = r$house_age_at_trans[i], ping = r$pure_house_ping[i], land = r$land_ping[i],
                           floor = r$floor_num[i], total_floors = r$total_floors_num[i],
                           rooms = r$rooms[i], baths = r$baths[i], parking = isTRUE(r$parking[i] == 1),
                           elevator = r$elevator[i] == 1, management = r$management[i] == 1,
                           # 分區空白 = 非都市土地 (網站的「非都市土地」選項)，不是「不知道」
                           zoning = if (is.na(r$zoning[i]) || r$zoning[i] == "") "非都市" else r$zoning[i],
                           main_ping = r$main_ping[i])
  build_model_input(e$inp)[1, ]
}, numeric(length(fx$features))))
colnames(x_app) <- fx$features

# 逐欄比較。距離類特徵允許 1 公尺誤差 (座標轉換的四捨五入)，其他允許 0.1% 相對誤差 (浮點數)
tol <- ifelse(grepl("^d_", fx$features), 1, 1e-3)
bad <- abs(x_app - fx$x) > pmax(tol[col(x_app)], 1e-3 * abs(fx$x))
report <- data.frame(feature = fx$features, mismatch = colMeans(bad))
report <- report[order(-report$mismatch), ]
print(head(report[report$mismatch > 0, ], 15), row.names = FALSE)

# 不一致的例子：方便除錯 (看是哪一筆、兩邊各算出多少)
worst <- report$feature[report$mismatch > 0.02]
for (f in worst) {
  i <- which(bad[, f])[1]
  message(sprintf("  ✗ %s 例：%s%s%s  訓練 = %s，網站 = %s", f, r$city[i], r$district[i], addr[i],
                  format(fx$x[i, f]), format(x_app[i, f])))
}
if (length(worst) > 0) stop("訓練與網站的特徵不一致 (", length(worst), " 個欄位超過 2%)：", paste(worst, collapse = "、"))
message("✅ 訓練與網站一致：", nrow(r), " 筆真實成交 × ", length(fx$features), " 個特徵，全部欄位不一致比例 ≤ 2%")
