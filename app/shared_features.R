# ==============================================================================
# 估價時要用的空間特徵函數 (App 和訓練程式共用同一份)
#
# 為什麼放在 app/ 資料夾？
#   部署到 shinyapps.io 時只會上傳 app/ 資料夾。App 估價時要算「到捷運站的距離」「附近行情」，
#   必須和訓練時用一模一樣的算法 (否則就是 training-serving skew，上一輪的土地持分 bug)。
#   所以這些函數只寫在這裡一份：App 用 source("shared_features.R")，
#   根目錄的 new_features.R 也 source 這個檔案。
#   (只依賴 base R + RANN + sf，不依賴 data.table / dplyr)
# ==============================================================================

# --- 2. 計算一批位置的設施特徵 --------------------------------------------------
# xy：n × 2 的矩陣 (TWD97 公尺)。回傳 n 列的 data.frame
dist_cols  <- c("mrt", "lrt", "tra", "hsr", "airport", "bus", "cemetery", "landfill", "wastewater",
                "industrial", "military", "fuel", "school", "park", "hospital", "market")
count_cols <- c("bus", "convenience")                     # 500 公尺內有幾個

facility_features <- function(xy, fac) {
  out <- list()
  for (k in dist_cols) {
    # nn2：k-d tree 最近鄰搜尋。k = 1 → 最近的那一個；回傳距離 (公尺)
    # 設施不存在 (例如澎湖沒有捷運) 時給一個很大的距離 (50 公里) = 「附近沒有」
    out[[paste0("d_", k)]] <- if (nrow(fac[[k]]) == 0) rep(50000, nrow(xy)) else
      pmin(RANN::nn2(fac[[k]], xy, k = 1)$nn.dists[, 1], 50000)
  }
  for (k in count_cols) {
    # searchtype = "radius"：只找 500 公尺內的點，最多找 200 個；回傳 0 的代表「沒找到」
    nn <- RANN::nn2(fac[[k]], xy, k = min(200, nrow(fac[[k]])), searchtype = "radius", radius = 500)
    out[[paste0("n_", k, "_500")]] <- rowSums(nn$nn.idx > 0)
  }
  as.data.frame(lapply(out, round))
}

# 經緯度 → TWD97 公尺座標
lnglat_to_xy <- function(lng, lat) {
  st_coordinates(st_transform(st_as_sf(data.frame(lng = lng, lat = lat), coords = c("lng", "lat"), crs = 4326), 3826))
}

# --- 附近行情：格子大小 + 查詢單一位置 ---------------------------------------------
# 定義與說明見 new_features.R 的 neighbor_price()
cell_size <- 250

lookup_nbr <- function(tbl, x, y, type, min_n = 3) {
  # ⚠️ 變數名稱刻意叫 qx、qy，不叫 cx、cy：如果 tbl 是 data.table，tbl[...] 裡的 cx 會被當成「欄位」
  #    而不是這裡算好的值 (data.table 的作用域規則)，結果整張表都被選到。as.data.frame() 也是同樣的保險
  tbl <- as.data.frame(tbl)
  qx <- floor(x / cell_size); qy <- floor(y / cell_size)
  near <- tbl[tbl$cx %in% (qx - 1):(qx + 1) & tbl$cy %in% (qy - 1):(qy + 1), ]
  same <- near[near$type == type, ]; all <- near[near$type == "ALL", ]
  data.frame(nbr_lp = if (sum(same$n) >= min_n) sum(same$s) / sum(same$n) else NA,
             nbr_n = sum(same$n),
             nbr_lp_all = if (sum(all$n) >= min_n) sum(all$s) / sum(all$n) else NA)
}


# --- 臨路等級 (實驗 03 驗證有效：MAPE 12.2% → 12.0%，透天 18.2% → 17.6%) -------------------
# 訓練 (new_model_training.R) 和 App 估價都呼叫 road_features()，保證算法一模一樣 (避免 training-serving skew)
# rt = build_road_tables() 產生的兩張表 (見 new_features.R)：
#   rt$cls   ：「路名 + 2 公里方格」→ 這條路在附近 3 公里內的最高 OSM 等級
#   rt$major ：主要道路 (OSM trunk/primary/secondary) 每 50 公尺一個點的 TWD97 座標
road_rank <- c(trunk = 5, primary = 5, secondary = 4, tertiary = 3, residential = 2,
               unclassified = 2, living_street = 2, service = 1, motorway = 0)
road_grid <- 2000

# 路名：抓第一個「…路 / …街 / …大道」。base R 的 regmatches：找不到時回傳 NA
road_name <- function(x) {
  out <- rep(NA_character_, length(x))
  m <- regexpr("^.+?(大道|路|街)", x, perl = TRUE)
  out[m > 0] <- regmatches(x, m)
  out
}

# rest：地址去掉「縣市 + 鄉鎮市區」之後的部分 (例：康樂街136巷15弄32號十一樓之4)
# xy  ：n × 2 的 TWD97 公尺座標
road_features <- function(rest, xy, rt) {
  rest <- sub("^[^0-9路街道巷弄段]{1,4}[村里](?=\\d+鄰|[^0-9]+(路|街|大道|巷))", "", rest, perl = TRUE)  # 去掉村里
  rest <- sub("^\\d+鄰", "", rest)
  road <- road_name(rest)
  key  <- paste(road, floor(xy[, 1] / road_grid), floor(xy[, 2] / road_grid))
  data.frame(
    lane_depth   = ifelse(grepl("\\d+弄", rest), 2L, ifelse(grepl("\\d+巷", rest), 1L, 0L)),   # 0 臨路 / 1 巷 / 2 弄
    road_type    = ifelse(is.na(road), 0L, ifelse(grepl("大道$", road), 3L,
                          ifelse(grepl("路$", road), 2L, ifelse(grepl("街$", road), 1L, 0L)))),
    road_class   = ifelse(is.na(road), NA_real_, rt$cls$rank[match(key, rt$cls$key)]),
    d_major_road = round(RANN::nn2(rt$major, xy, k = 1)$nn.dists[, 1])
  )
}
