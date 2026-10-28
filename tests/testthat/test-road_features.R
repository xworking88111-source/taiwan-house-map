# ==============================================================================
# 臨路等級 road_features() 的測試：地址解析的規則 + 道路等級查表
# ==============================================================================
# 假的道路表：一條「中正路」在 (0,0) 方格是等級 5、一條「民生街」等級 2；主要道路在原點
rt <- list(cls = data.frame(key = c("中正路 0 0", "民生街 0 0"), rank = c(5, 2)),
           major = matrix(c(0, 0), ncol = 2))
xy <- function(n) matrix(rep(c(300, 400), each = n), ncol = 2)   # 距離原點 500 公尺

test_that("巷弄深度：臨路 0、巷 1、弄 2", {
  r <- road_features(c("中正路10號", "中正路10巷3號", "中正路10巷5弄3號"), xy(3), rt)
  expect_equal(r$lane_depth, c(0, 1, 2))
})

test_that("路名尾字：大道 3、路 2、街 1、沒有路名 0", {
  r <- road_features(c("台灣大道一段1號", "中正路1號", "民生街1號", "後壁厝1號"), xy(4), rt)
  expect_equal(r$road_type, c(3, 2, 1, 0))
})

test_that("村里名稱要先去掉，否則路名會變成「安招里安林路」查不到", {
  r <- road_features(c("安招里中正路1號", "安招里5鄰中正路1號"), xy(2), rt)
  expect_equal(r$road_class, c(5, 5))
})

test_that("道路等級：查得到給等級，查不到給 NA", {
  r <- road_features(c("中正路1號", "民生街1號", "不存在路1號"), xy(3), rt)
  expect_equal(r$road_class, c(5, 2, NA))
})

test_that("到主要道路距離 = 直線距離 (公尺)", {
  expect_equal(road_features("中正路1號", xy(1), rt)$d_major_road, 500)
})

test_that("空字串地址 (使用者只選行政區) 不會出錯", {
  r <- road_features("", xy(1), rt)
  expect_equal(r$lane_depth, 0); expect_true(is.na(r$road_class))
})
