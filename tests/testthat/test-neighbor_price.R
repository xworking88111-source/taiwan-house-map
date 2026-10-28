# ==============================================================================
# 附近行情 neighbor_price() / lookup_nbr() 的測試
# ==============================================================================
test_that("附近行情只看之前 12 個月、不看自己這個月", {
  # 同一格 3 筆透天：第 1、2 月各一筆，第 3 月的房子應該看到前兩筆 (min_n 調成 1 方便測)
  r <- neighbor_price(x = c(10, 20, 30), y = c(10, 20, 30), type = rep("透天厝", 3),
                      month = c(1, 2, 3), logp = c(1, 2, 3), min_n = 1)
  expect_equal(r$nbr_n, c(0, 1, 2))
  expect_equal(r$nbr_lp[3], mean(c(1, 2)))
})

test_that("超過 9 格範圍 (約 375 公尺以外) 的成交不算", {
  r <- neighbor_price(x = c(0, 2000), y = c(0, 0), type = rep("透天厝", 2), month = c(1, 2), logp = c(1, 2), min_n = 1)
  expect_equal(r$nbr_n[2], 0)
})

test_that("lookup_nbr 用 data.table 傳進來也不會選錯 (變數和欄位同名的 bug)", {
  tbl <- data.table::data.table(cx = c(0, 100), cy = c(0, 100), type = "透天厝", s = c(3, 30), n = c(3, 3))
  r <- lookup_nbr(tbl, x = 10, y = 10, type = "透天厝")
  expect_equal(r$nbr_n, 3)               # 只算到 (0,0) 這格，不能把遠方的 (100,100) 也算進來
  expect_equal(r$nbr_lp, 1)
})
