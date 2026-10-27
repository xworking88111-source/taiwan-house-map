# ==============================================================================
# 空間特徵：交通、嫌惡設施、生活機能、附近行情
#
# 為什麼要獨立成一支檔案？
#   訓練 (new_model_training.R)、回測 (new_model_backtest.R)、App (app/app.R)
#   都要算「同一套」特徵。上一輪才因為 App 和訓練時的土地持分算法不同，估價低估了 25%
#   (training-serving skew)。所以特徵的定義只寫在這裡一份，大家都呼叫同一個函數。
#
# 產出：
#   geo_data/facilities.rds      各類設施的座標 (TWD97 公尺座標)，App 也會用
#   PostgreSQL re_v2.geo_features 每個地址 (addr_key) 的設施距離 → 「特徵庫 (feature store)」
#                                 一個地址只算一次，之後每週更新只算新地址 (和 geo_cache 一樣是增量)
#
# 執行：source("new_features.R")  (約 1~2 分鐘)
# ==============================================================================
library(sf)
library(tidyverse)
library(DBI)
library(RPostgres)
library(RANN)          # k-d tree 最近鄰搜尋：幾十萬個點找「最近的捷運站」只要幾秒
library(data.table)    # 附近行情的「滾動加總」需要大量 join，data.table 比 dplyr 快很多

source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "config.R"))
geo_dir <- file.path(project_path, "geo_data")

# --- 1. 設施清單：每一類 = 一組點 (TWD97 公尺座標) ---------------------------------
# 座標系統：EPSG:3826 (TWD97 二度分帶)，單位是公尺 → 算距離直接用畢氏定理就好
# 面狀設施 (工業區、墓地、公園…)：取「多邊形的頂點」當作點 → 算出來的是「到邊界」的距離，
# 比「到中心點」合理 (一個大工業區的中心可能離你 2 公里，但邊界就在對面)
read_layer <- function(layer, fclass = NULL, name_pattern = NULL, name_exclude = NULL) {
  f <- file.path(geo_dir, paste0(layer, ".shp"))
  d <- st_read(f, quiet = TRUE)
  if (!is.null(fclass))       d <- d[d$fclass %in% fclass, ]
  if (!is.null(name_pattern)) d <- d[!is.na(d$name) & grepl(name_pattern, d$name), ]
  if (!is.null(name_exclude)) d <- d[is.na(d$name) | !grepl(name_exclude, d$name), ]
  st_transform(d, 3826)
}
to_points <- function(d) {
  if (nrow(d) == 0) return(matrix(numeric(0), ncol = 2))
  g <- st_geometry(d)
  if (all(st_geometry_type(g) %in% c("POINT"))) return(st_coordinates(g)[, 1:2, drop = FALSE])
  # st_coordinates() 會列出多邊形的「每一個頂點」。頂點太多會很慢 → 四捨五入到 20 公尺的格子再去重複。
  # (⚠️ 一開始用 st_simplify + st_cast(..., "POINT")：st_cast 只會取每個多邊形的「第一個點」，
  #    而 st_simplify 會把很小的多邊形簡化成「空的」→ 座標變 NA，最近鄰搜尋就當掉了)
  xy <- st_coordinates(g[!st_is_empty(g)])[, 1:2, drop = FALSE]
  unique(round(xy / 20) * 20)
}

build_facilities <- function() {
  message(">> 讀取 OSM 設施圖層…")
  # 車站分類：車站本身沒有標記是捷運還是台鐵，所以看「100 公尺內是哪一種鐵路路線」
  stations <- rbind(read_layer("gis_osm_transport_free_1", c("railway_station", "railway_halt", "tram_stop"))[, "name"],
                    st_centroid(read_layer("gis_osm_transport_a_free_1", c("railway_station", "railway_halt"))[, "name"]))
  rail <- read_layer("gis_osm_railways_free_1")
  near_line <- function(lines) lengths(st_is_within_distance(stations, lines, dist = 100)) > 0
  # 高鐵：台北段的高鐵在地下，和台鐵、捷運路線重疊 → 光看「靠近高鐵路線」會把萬華、松山、西門也算進來。
  # 所以再加一個條件：站名必須是 12 個高鐵站之一
  hsr_names <- c("南港", "臺北", "台北", "台北車站", "板橋", "桃園", "高鐵桃園站", "新竹", "苗栗",
                 "臺中", "台中", "彰化", "雲林", "嘉義", "臺南", "台南", "左營", "新左營")
  is_hsr <- near_line(rail[rail$fclass == "rail" & grepl("高速鐵路|高鐵", rail$name), ]) & stations$name %in% hsr_names
  is_mrt <- near_line(rail[rail$fclass %in% c("subway", "monorail"), ])
  is_lrt <- near_line(rail[rail$fclass %in% c("light_rail", "tram"), ])
  is_tra <- near_line(rail[rail$fclass == "rail" & !grepl("高速鐵路|高鐵", rail$name), ])

  # 機場：只留民用機場 (排除空軍基地、營區、輕航機場、直升機場)
  airports <- rbind(read_layer("gis_osm_transport_a_free_1", "airport")[, "name"] %>% st_centroid(),
                    read_layer("gis_osm_transport_free_1", "airport")[, "name"])
  airports <- airports[grepl("機場", airports$name) &
                         !grepl("空軍|海軍|營區|基地|輕航|直昇|無人機|訓練|俱樂部|舊|龍岡|屏北|屏南|南保|飛行場", airports$name), ]

  fac <- list(
    # ---- 交通 ----
    mrt      = st_coordinates(stations[is_mrt, ])[, 1:2],
    lrt      = st_coordinates(stations[is_lrt, ])[, 1:2],
    tra      = st_coordinates(stations[is_tra, ])[, 1:2],
    hsr      = st_coordinates(stations[is_hsr, ])[, 1:2],
    airport  = st_coordinates(airports)[, 1:2],
    bus      = to_points(read_layer("gis_osm_transport_free_1", c("bus_stop", "bus_station"))),
    # ---- 嫌惡設施 ----
    cemetery   = rbind(to_points(read_layer("gis_osm_landuse_a_free_1", "cemetery")),
                       to_points(read_layer("gis_osm_pois_a_free_1", "graveyard"))),
    landfill   = rbind(to_points(read_layer("gis_osm_landuse_a_free_1", "landfill")),
                       to_points(read_layer("gis_osm_pois_a_free_1", "recycling"))),
    wastewater = to_points(read_layer("gis_osm_pois_a_free_1", "wastewater_plant")),
    industrial = to_points(read_layer("gis_osm_landuse_a_free_1", "industrial")),
    military   = to_points(read_layer("gis_osm_landuse_a_free_1", "military")),
    fuel       = to_points(read_layer("gis_osm_traffic_free_1", "fuel")),
    # ---- 生活機能 ----
    school     = to_points(read_layer("gis_osm_pois_a_free_1", c("school", "kindergarten"))),
    park       = rbind(to_points(read_layer("gis_osm_pois_a_free_1", "park")),
                       to_points(read_layer("gis_osm_landuse_a_free_1", "park"))),
    hospital   = rbind(to_points(read_layer("gis_osm_pois_a_free_1", "hospital")),
                       to_points(read_layer("gis_osm_pois_free_1", "hospital"))),
    market     = rbind(to_points(read_layer("gis_osm_pois_a_free_1", c("marketplace", "supermarket", "mall"))),
                       to_points(read_layer("gis_osm_pois_free_1", c("marketplace", "supermarket", "mall")))),
    convenience = to_points(read_layer("gis_osm_pois_free_1", "convenience"))
  )
  fac <- lapply(fac, function(m) { m <- unique(round(as.matrix(m))); colnames(m) <- c("x", "y"); m })
  message(">> 設施點數：", paste(names(fac), sapply(fac, nrow), sep = "=", collapse = "，"))
  fac
}

# --- 2. 共用函數 (設施距離、座標轉換、單一位置附近行情) ------------------------
# 放在 app/shared_features.R：App 部署時只會上傳 app/ 資料夾，所以共用函數放那裡，這裡載入
source(file.path(project_path, "app", "shared_features.R"))

# --- 3. 附近行情：附近約 500 公尺、同型態、「交易月份之前」12 個月的成交行情 -------------
# 定義：把台灣切成 250 × 250 公尺的格子，「附近」= 自己的格子 + 周圍 8 格 (750 公尺見方，
#       面積約等於半徑 420 公尺的圓)。取這 9 格裡、過去 12 個月同型態成交的 log 單價平均。
#
# 為什麼是 log 平均，不是中位數？
#   中位數沒辦法「加總」：每筆交易都要把附近幾百筆撈出來排序，76 萬筆 × 幾百筆 = 好幾億次運算。
#   log 平均 (= 幾何平均) 可以先按「格子 × 月份」加總好，再用加減法算任意時間窗，幾秒就算完；
#   而且取 log 之後，一兩筆豪宅不會把平均拉很高，對極端值的敏感度和中位數接近。
#
# 為什麼只能用「之前」的月份？(資料洩漏 data leakage)
#   如果用到同月份或之後的成交，等於讓模型偷看答案。上線時我們不會知道未來的成交。
#   cap_month：回測時「切點之後」的資料在真實世界還不存在，所以時間窗最多只能到切點。
neighbor_price <- function(x, y, type, month, logp, cap_month = Inf, window = 12, min_n = 3) {
  dt <- data.table(cx = floor(x / cell_size), cy = floor(y / cell_size), type = type, m = month, lp = logp)
  dt[, id := .I]
  calc <- function(type_key) {
    # (1) 先按「格子 × 型態 × 月份」加總，再算每個格子的累計值 (cumsum)
    src <- if (type_key == "same") dt[, .(cx, cy, type, m, lp)] else dt[, .(cx, cy, type = "ALL", m, lp)]
    agg <- src[, .(s = sum(lp), n = .N), by = .(cx, cy, type, m)][order(cx, cy, type, m)]
    agg[, `:=`(cs = cumsum(s), cn = cumsum(n)), by = .(cx, cy, type)]
    # (2) 每筆交易 × 9 個鄰近格子 → 查詢「時間窗結尾」和「開頭前一個月」的累計值，相減 = 窗內加總
    q <- dt[, .(id, cx, cy, type = if (type_key == "same") type else "ALL",
                hi = pmin(m - 1, cap_month), lo = m - window - 1)]
    q <- q[rep(seq_len(.N), each = 9)][, `:=`(cx = cx + rep(-1:1, each = 3), cy = cy + rep(-1:1, times = 3))]
    # roll = TRUE：找「月份 <= 查詢月份」的最後一筆累計值 (rolling join，data.table 的招牌功能)
    hi <- agg[q, on = .(cx, cy, type, m = hi), roll = TRUE, .(id, cs, cn)]
    lo <- agg[q, on = .(cx, cy, type, m = lo), roll = TRUE, .(cs, cn)]
    w <- data.table(id = hi$id, s = fcoalesce(hi$cs, 0) - fcoalesce(lo$cs, 0),
                    n = fcoalesce(hi$cn, 0L) - fcoalesce(lo$cn, 0L))
    w[, .(s = sum(s), n = sum(n)), by = id][order(id)]
  }
  same <- calc("same"); all <- calc("all")
  data.frame(
    nbr_lp   = ifelse(same$n >= min_n, same$s / same$n, NA),   # 附近同型態行情 (log 元/坪)
    nbr_n    = same$n,                                          # 附近同型態成交筆數 (= 熱門程度)
    nbr_lp_all = ifelse(all$n >= min_n, all$s / all$n, NA)      # 附近所有型態行情 (同型態太少時的備援)
  )
}

# --- 3b. 單一位置的附近行情 (訓練程式的示範估價、App 估價都用這個) ----------------
# 上面的 neighbor_price() 是替「整批歷史交易」各自算 (每筆有自己的時間窗)。
# 估價時只需要「現在」的行情：先把最近 12 個月的成交按「格子 × 型態」加總成一張小表存起來，
# 估價時查詢周圍 9 格相加即可。時間窗 = [最新月份 - 12, 最新月份 - 1]，
# 和訓練時「預測月份 = 最新月份」的設定完全一致。
build_nbr_table <- function(x, y, type, month, logp, ref_month) {
  dt <- data.table(cx = floor(x / cell_size), cy = floor(y / cell_size), type = type, m = month, lp = logp)
  dt <- dt[m >= ref_month - 12 & m <= ref_month - 1]
  rbind(dt[, .(s = sum(lp), n = .N), by = .(cx, cy, type)],
        dt[, .(s = sum(lp), n = .N, type = "ALL"), by = .(cx, cy)])
}
# --- 4. 主程式：建立設施檔 + 增量計算每個地址的設施特徵 ----------------------------
# --- 同棟歷史成交 (實驗 02 驗證有效：MAPE 12.5% → 12.2%) -------------------------
# 每筆交易：同門牌 (addr_key，不含樓層) 在它之前 window 個月內的成交 → 平均 log 單價、筆數、平均距今月數
# 防洩漏：只看更早的月份；cap_month 以後的成交一律看不到 (測試期 = 站在 cap_month+1 往回看)
building_history <- function(addr_key, month, logp, cap_month = Inf, window = 36) {
  hist <- data.table(k = addr_key, hm = month, lp = logp)[hm <= cap_month]
  q <- data.table(k = addr_key, m = pmin(month, cap_month + 1))   # 測試期：等同站在 cut_month+1 往回看
  q[, `:=`(lo = m - window, hi = m - 1)]
  res <- hist[q, on = .(k, hm >= lo, hm <= hi),
              .(bld_lp = mean(lp), bld_n = sum(!is.na(lp)), bld_dt = mean(i.m - hm)), by = .EACHI]
  res[bld_n == 0, `:=`(bld_lp = NA_real_, bld_dt = NA_real_)]
  as_tibble(res[, .(bld_lp, bld_n, bld_dt)])
}

# --- 臨路等級用的道路表 (App 估價也要用 → 匯出到 app/data/road_class.rds) ---------------------
# 1. 主要道路：路線每 50 公尺切一個點 → 估價時用 k-d tree 找最近點 = 到主要道路的距離 (誤差 < 25 公尺)
# 2. 道路等級：OSM 路段名稱 (忠孝東路四段) 只留到「路/街/大道」→ 忠孝東路，
#    按「路名 + 2 公里方格」取最高等級，再複製到周圍 9 格 → 查詢時一次 match() 就等於查 3 公里內
#    為什麼要限定附近？全台有幾百條「中山路」，等級各不相同
build_road_tables <- function() {
  roads <- st_read(file.path(project_path, "geo_data", "gis_osm_roads_free_1.shp"), quiet = TRUE,
                   query = paste0("SELECT name, fclass FROM gis_osm_roads_free_1 WHERE fclass IN (",
                                  paste0("'", names(road_rank), "'", collapse = ","), ")")) %>%
    st_transform(3826)
  major <- roads %>% filter(fclass %in% c("trunk", "primary", "secondary")) %>%
    st_segmentize(dfMaxLength = 50) %>% st_coordinates()
  xy  <- st_coordinates(suppressWarnings(st_point_on_surface(roads)))
  osm <- data.table(road = road_name(coalesce(roads$name, "")), rank = road_rank[roads$fclass],
                    gx = floor(xy[, 1] / road_grid), gy = floor(xy[, 2] / road_grid))[!is.na(road)]
  osm <- osm[, .(rank = max(rank)), by = .(road, gx, gy)]
  cls <- rbindlist(lapply(0:8, function(i) osm[, .(road, gx = gx + i %/% 3 - 1, gy = gy + i %% 3 - 1, rank)]))
  cls <- cls[, .(rank = max(rank)), by = .(road, gx, gy)]
  list(cls = data.frame(key = paste(cls$road, cls$gx, cls$gy), rank = cls$rank),
       major = unname(round(major[, 1:2])))           # 座標取整數公尺，檔案小一半
}

if (!isTRUE(getOption("features.as_library"))) {   # 被其他程式當成函式庫載入時不執行主程式
  fac_file <- file.path(geo_dir, "facilities.rds")
  fac <- build_facilities()
  saveRDS(fac, fac_file)

  con <- db_connect()
  dbExecute(con, "SET search_path TO re_v2;")
  # 設施資料 (OSM) 更新後要全部重算 → 用 fac_version 判斷；同一版只算新地址
  fac_version <- format(file.mtime(file.path(geo_dir, "gis_osm_transport_free_1.shp")), "%Y%m%d")
  if (dbExistsTable(con, "geo_features") &&
      !identical(dbGetQuery(con, "SELECT max(fac_version) v FROM geo_features")$v, fac_version)) {
    dbExecute(con, "DROP TABLE geo_features;")
  }
  todo <- dbGetQuery(con, if (dbExistsTable(con, "geo_features")) "
      SELECT g.addr_key, g.lng, g.lat FROM geo_cache g
      LEFT JOIN geo_features f USING (addr_key) WHERE f.addr_key IS NULL" else
      "SELECT addr_key, lng, lat FROM geo_cache")
  message(">> 待計算地址：", nrow(todo))
  if (nrow(todo) > 0) {
    xy <- lnglat_to_xy(todo$lng, todo$lat)
    feats <- cbind(addr_key = todo$addr_key, x = round(xy[, 1]), y = round(xy[, 2]),
                   facility_features(xy, fac), fac_version = fac_version)
    dbWriteTable(con, "geo_features", feats, append = TRUE)
    dbExecute(con, "CREATE UNIQUE INDEX IF NOT EXISTS idx_geofeat_key ON geo_features(addr_key);")
    print(summary(feats[, c("d_mrt", "d_tra", "d_hsr", "d_bus", "d_cemetery", "d_industrial", "n_convenience_500")]))
  }
  dbExecute(con, "ANALYZE geo_features;")
  dbDisconnect(con)
}
