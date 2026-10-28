# ==============================================================================
# 同棟歷史成交 building_history() 的測試
# 測試的寫法：準備「答案已知」的小資料 → 呼叫函數 → expect_*() 檢查結果
#   每個 test_that() 是一個獨立的情境，名稱寫「應該怎樣」，失敗時一看就知道哪個規則被破壞
# ==============================================================================

test_that("只看更早的月份，同月份的成交不能算進去 (防資料洩漏)", {
  # A 棟：第 5 個月有兩筆 → 這兩筆互相看不到對方 (同月)，只看得到第 1 個月
  r <- building_history(c("A", "A", "A"), c(1, 5, 5), log(c(10, 20, 30)))
  expect_equal(r$bld_n, c(0, 1, 1))
  expect_equal(r$bld_lp[2:3], rep(log(10), 2))
  expect_true(is.na(r$bld_lp[1]))                 # 沒有更早的紀錄 → NA (不是 0，0 會被當成「很便宜」)
})

test_that("不同建物互不影響", {
  r <- building_history(c("A", "B"), c(1, 5), log(c(10, 20)))
  expect_equal(r$bld_n, c(0, 0))
})

test_that("超過時間窗 (36 個月) 的舊成交不算", {
  r <- building_history(c("A", "A"), c(1, 40), log(c(10, 20)))
  expect_equal(r$bld_n[2], 0)
  r <- building_history(c("A", "A"), c(4, 40), log(c(10, 20)))   # 40 - 36 = 4 → 剛好在窗內
  expect_equal(r$bld_n[2], 1)
})

test_that("cap_month 之後的成交看不到 (回測時模擬「上線當下」)", {
  # 切點 = 10：第 12 個月的成交在上線時還不存在，第 15 個月的房子只能看到第 5 個月
  r <- building_history(c("A", "A", "A"), c(5, 12, 15), log(c(10, 20, 30)), cap_month = 10)
  expect_equal(r$bld_n[3], 1)
  expect_equal(r$bld_lp[3], log(10))
  expect_equal(r$bld_dt[3], 11 - 5)               # 站在 cap_month + 1 = 11 往回看，距今 6 個月
})

test_that("輸出筆數和順序跟輸入一致", {
  set.seed(1)
  k <- sample(c("A", "B", "C"), 200, replace = TRUE); m <- sample(1:60, 200, replace = TRUE)
  r <- building_history(k, m, rnorm(200))
  expect_equal(nrow(r), 200)
  # 逐筆用最笨的方法算一次，和函數結果比對 (「參考實作」測試法：慢但一定對)
  slow_n <- sapply(seq_along(k), function(i) sum(k == k[i] & m < m[i] & m >= m[i] - 36))
  expect_equal(r$bld_n, slow_n)
})
