# ==============================================================================
# 共用函式庫 R/model_lib.R 的測試：清理、特徵工程、模型輸入格式
# ==============================================================================
suppressMessages(source(file.path(Sys.getenv("RE_PROJECT_PATH", "../.."), "R", "model_lib.R")))

toy <- tibble(
  city = c("高雄市", "高雄市", "臺北市"), district = c("鳳山區", "三民區", "大安區"),
  b_category = c("透天厝", "電梯大樓", "電梯大樓"), trans_ym = as.Date(c("2020-01-01", "2021-03-01", "2026-09-01")),
  floor_num = c(NA, 5, 10), total_floors_num = c(3, 10, 10), pure_house_ping = c(40, 30, 20),
  land_ping = c(20, 3, 2), has_elevator = c(NA, "有", "有"), has_management = c("無", "有", NA),
  include_parking = c("否", "是", "否"), parking_price_unknown = c(FALSE, TRUE, FALSE),
  pure_house_unit_price_ping = c(2e5, 3e5, 1e6),
  zoning = c("住", "都市：其他:商業區", NA), main_ping = c(38, 18, NA)
)

test_that("engineer_features：月份編號、樓層比、頂樓、缺值處理", {
  f <- engineer_features(toy)
  expect_equal(f$t_month_idx, c(1, 15, 81))                   # 2020-01 = 1、2021-03 = 15、2026-09 = 81
  expect_equal(f$y, log(toy$pure_house_unit_price_ping))
  expect_equal(f$floor_ratio, c(NA, 0.5, 1))
  expect_equal(f$is_top_floor, c(0L, 0L, 1L))
  expect_equal(f$elevator, c(0L, 1L, 1L))                     # NA → 0 (沒寫就當沒有)，不能是 NA
  expect_equal(f$management, c(0L, 1L, 0L))
  expect_equal(f$zoning_code, c(1L, 2L, 0L))                 # 住 → 1、「都市：其他:商業區」→ 2、空白 → 0
  expect_equal(f$main_ratio, c(38 / 40, 18 / 30, NA))
})

test_that("使用分區代碼：簡寫、完整寫法、非都市、其他都對得上 (網站和訓練共用)", {
  expect_equal(zoning_code_of(c("住", "商", "工", "農", "", NA, "非都市", "都市：其他:住宅區", "都市：其他:保護區")),
               c(1L, 2L, 3L, 4L, 0L, 0L, 0L, 1L, 5L))
  expect_equal(main_ratio_of(c(30, 0, NA, 90), c(50, 50, 50, 50)), c(0.6, NA, NA, 1.5))   # 超過 1.5 截斷
})

test_that("clean_trades：同縣市同型態內砍掉最高、最低 1%，坪數範圍外的移除", {
  set.seed(1)
  d <- tibble(city = "高雄市", b_category = "透天厝", trans_ym = as.Date("2024-01-01"),
              pure_house_ping = c(runif(198, 10, 100), 2, 400), house_age_at_trans = 10,
              pure_house_unit_price_ping = c(rnorm(198, 2e5, 2e4), 2e5, 2e5))
  d$pure_house_unit_price_ping[1:2] <- c(1, 1e9)              # 兩筆極端值
  out <- clean_trades(d)
  expect_false(any(out$pure_house_unit_price_ping %in% c(1, 1e9)))
  expect_false(any(out$pure_house_ping %in% c(2, 400)))       # 5 坪以下、300 坪以上
})

test_that("to_matrix：欄位順序固定、類別編號不受資料子集影響 (as.factor 舊 bug 的回歸測試)", {
  f <- engineer_features(toy) %>% mutate(house_age_at_trans = 5, rooms = 3, baths = 2, parking = 0L, lng = 120, lat = 23)
  lv <- make_fac_levels(f)
  feats <- features_v1
  full <- to_matrix(f, feats, lv)
  sub  <- to_matrix(f[2, ], feats, lv)                        # 只有「三民區」一筆的子集
  expect_equal(colnames(full), feats)
  expect_equal(sub[1, "district"], full[2, "district"])        # 同一個區，編號一定一樣
  expect_equal(full[1, "floor_num"], c(floor_num = -1))        # 缺值 → -1
})

test_that("特徵清單：v4 = v3 + 使用分區 + 主建物比例", {
  expect_equal(setdiff(features_v4, features_v3), c("zoning_code", "main_ratio"))
  expect_identical(features, features_v4)
})

test_that("特徵清單：v3 = v2 + 7 個新特徵、沒有重複", {
  expect_equal(setdiff(features_v3, features_v2),
               c("bld_lp", "bld_n", "bld_dt", "lane_depth", "road_type", "road_class", "d_major_road"))
  expect_false(anyDuplicated(features_v3) > 0)
})
