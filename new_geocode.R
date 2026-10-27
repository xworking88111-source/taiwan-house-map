# ==============================================================================
# 地址 → 經緯度（免費、離線、可重複執行）
#
# 為什麼不用 Google / TGOS API？
#   - 去重複後約 76 萬個建物地址。Google 免費額度每月約 1 萬筆、TGOS 線上批次一次 500 筆，
#     Nominatim 公共伺服器限速 1 秒 1 筆且禁止大量批次。全部都不符合「免費 + 一週內」。
#
# 做法：「OSM 道路資料 + 鄉鎮界線」在自己電腦上比對（幾分鐘跑完全台）
#   1. Geofabrik 提供每日更新的台灣 OSM 道路 shapefile，路名格式很完整，
#      例如「忠孝東路四段181巷35弄」，連 段/巷/弄 都有。
#   2. 把每一條道路切片丟進鄉鎮界線，得到 (縣市, 鄉鎮區, 路名) → 代表點 的對照表。
#   3. 實價登錄地址從最精細開始往上比對：弄 → 巷 → 路+段 → 路 → 鄉鎮區中心點。
#   4. 每個地址記錄 geo_level，App 可以誠實地告訴使用者「這個點精準到哪一層」。
#
# 精準度：巷弄層級通常在 50~200 公尺內，路段層級可能差到 1 公里 (長路)。
#   對「集群地圖」「區域行情」完全夠用；之後想升級成門牌級，見檔尾 Phase 2。
#
# 實價登錄本身也有「門牌區間化」(例如 1~30 號)，原始資料就不是精確門牌，
# 所以路段/巷弄級在資料品質上是合理的。
# ==============================================================================
library(sf)
library(tidyverse)
library(DBI)
library(RPostgres)

# 共用設定 (路徑、資料庫連線)：本機和 GitHub Actions 都用同一份程式
source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "config.R"))
base_path <- project_path
geo_dir   <- file.path(base_path, "geo_data")       # 圖資放這 (很大，不要上傳 GitHub)
dir.create(geo_dir, showWarnings = FALSE)

# --- 1. 下載圖資 (只有第一次會下載) -------------------------------------------
towns_file <- file.path(geo_dir, "towns.json")
roads_shp  <- file.path(geo_dir, "gis_osm_roads_free_1.shp")

if (!file.exists(towns_file)) {
  # 鄉鎮市區界線 (TopoJSON，來源為內政部國土測繪中心，由 taiwan-atlas 專案整理)
  download.file("https://cdn.jsdelivr.net/npm/taiwan-atlas/towns-10t.json", towns_file, mode = "wb")
}
osm_layers <- c("gis_osm_roads_free_1", "gis_osm_transport_free_1", "gis_osm_transport_a_free_1",
                "gis_osm_railways_free_1", "gis_osm_pois_free_1", "gis_osm_pois_a_free_1",
                "gis_osm_landuse_a_free_1", "gis_osm_traffic_free_1")
# 任何一個需要的圖層不存在就重新下載 (舊版只解壓縮道路，雲端快取裡可能只有道路)
if (!all(file.exists(file.path(geo_dir, paste0(osm_layers, ".shp"))))) {
  # 約 260MB；OSM 資料授權為 ODbL，作品中要標示「© OpenStreetMap contributors」
  zip <- file.path(geo_dir, "osm.zip")
  options(timeout = 1800)
  download.file("https://download.geofabrik.de/asia/taiwan-latest-free.shp.zip", zip, mode = "wb")
  # 道路 (地理編碼用) + 交通、設施、土地使用 (new_features.R 的空間特徵用)
  unzip(zip, files = as.vector(outer(osm_layers, c("shp", "shx", "dbf", "prj", "cpg"), paste, sep = ".")), exdir = geo_dir)
  file.remove(zip)
}

# --- 2. 建立「道路代表點」對照表 -----------------------------------------------
# 座標系統小教室：
#   EPSG:4326 = 經緯度 (度)，地圖顯示用。
#   EPSG:3826 = TWD97 二度分帶 (公尺)，台灣算距離、算面積用。計算時都轉成 3826。
build_road_lookup <- function() {
  towns <- st_read(towns_file, layer = "towns", quiet = TRUE) %>%
    st_set_crs(4326) %>%                             # TopoJSON 沒有記錄座標系統，手動指定為 WGS84 經緯度
    transmute(city = gsub("台", "臺", COUNTYNAME), district = gsub("台", "臺", TOWNNAME)) %>%
    st_make_valid() %>%
    st_transform(3826)

  roads <- st_read(roads_shp, quiet = TRUE,
                   query = "SELECT name FROM gis_osm_roads_free_1 WHERE name IS NOT NULL") %>%
    mutate(name = gsub("台", "臺", chartr("０１２３４５６７８９", "0123456789", name))) %>%
    st_transform(3826)

  # 每段道路取中心點，判斷它落在哪個鄉鎮區 (點在多邊形內 = spatial join)
  pts <- suppressWarnings(st_centroid(roads))
  pts <- st_join(pts, towns, join = st_within) %>% filter(!is.na(district))
  xy  <- st_coordinates(pts)

  district_pts <- suppressWarnings(st_point_on_surface(towns))   # 鄉鎮區的「保證在區內」代表點
  dxy <- st_coordinates(st_transform(district_pts, 4326))

  road_tbl <- tibble(city = pts$city, district = pts$district, name = pts$name,
                     x = xy[, 1], y = xy[, 2]) %>%
    group_by(city, district, name) %>%
    # 同一條路由很多小段組成：先算平均位置，再挑「最靠近平均位置的那一段」當代表點，
    # 這樣點一定落在路上 (彎曲的路直接取平均可能會落在路外)
    mutate(d = (x - mean(x))^2 + (y - mean(y))^2) %>%
    slice_min(d, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(city, district, name, x, y)

  ll <- st_coordinates(st_transform(st_as_sf(road_tbl, coords = c("x", "y"), crs = 3826), 4326))
  list(
    roads = road_tbl %>% mutate(lng = ll[, 1], lat = ll[, 2]) %>% select(-x, -y),
    districts = tibble(city = towns$city, district = towns$district, lng = dxy[, 1], lat = dxy[, 2])
  )
}

# --- 3. 地址拆解成多層候選路名 -------------------------------------------------
# 例：臺北市松山區南京東路五段389巷5弄9號
#   lane2 = 南京東路五段389巷5弄 / lane = 南京東路五段389巷
#   road_sec = 南京東路五段 / road = 南京東路
make_candidates <- function(keys) {
  # keys: tibble(addr_key, city, district)
  rest <- str_remove(keys$addr_key, fixed(paste0(keys$city, keys$district)))
  rest <- str_remove(rest, fixed(keys$city))                                          # 行政區欄寫成縣市名的情況
  # 去掉「XX里16鄰」「XX村」這種村里前綴 (OSM 路名不含村里)。
  # 條件：後面必須接「鄰」或接一段含 路/街/巷 的文字，避免把「萬里路」的「里」誤刪
  rest <- str_remove(rest, "^[^0-9路街道巷弄段]{1,4}[村里](?=\\d+鄰|[^0-9]+(路|街|大道|巷))")
  rest <- str_remove(rest, "^\\d+鄰")                                                  # 去掉「16鄰」

  tibble(addr_key = keys$addr_key, city = keys$city, district = keys$district,
         lane2    = str_match(rest, "^(.+?(路|街|大道).*?巷.*?弄)")[, 2],
         lane     = coalesce(str_match(rest, "^(.+?(路|街|大道).*?巷)")[, 2],
                             str_match(rest, "^([^0-9路街]+?巷)")[, 2]),   # 鄉下常見「新厝巷18號」：沒有路名的巷
         road_sec = str_match(rest, "^(.+?(路|街|大道)[一二三四五六七八九十]{1,2}段)")[, 2],
         road     = str_match(rest, "^(.+?(路|街|大道))")[, 2]) %>%
    pivot_longer(c(lane2, lane, road_sec, road), names_to = "geo_level", values_to = "name") %>%
    filter(!is.na(name))
}

geocode_keys <- function(keys, lookup) {
  level_rank <- c(lane2 = 1, lane = 2, road_sec = 3, road = 4)

  # 少數資料的「鄉鎮市區」欄直接寫縣市名 (例：嘉義市 的 district 寫「嘉義市」而不是 東區/西區)。
  # 這種地址找不到正確的區，只好改用「同縣市內同名道路」比對：
  # 同一縣市有多個區都有同名路時，取第一個 (一個縣市內同名路很少見)
  valid <- paste(lookup$districts$city, lookup$districts$district)
  bad_district <- !paste(keys$city, keys$district) %in% valid
  roads_citywide <- lookup$roads %>% distinct(city, name, .keep_all = TRUE) %>% select(-district)

  cand <- make_candidates(keys)
  hits <- bind_rows(
    cand %>% filter(!addr_key %in% keys$addr_key[bad_district]) %>%
      inner_join(lookup$roads, by = c("city", "district", "name")),
    cand %>% filter(addr_key %in% keys$addr_key[bad_district]) %>%
      inner_join(roads_citywide, by = c("city", "name"))
  ) %>%
    mutate(rank = level_rank[geo_level]) %>%
    group_by(addr_key) %>%
    slice_min(rank, n = 1, with_ties = FALSE) %>%   # 每個地址只留最精細的那一層
    ungroup() %>%
    select(addr_key, lng, lat, geo_level)

  keys %>%
    left_join(hits, by = "addr_key") %>%
    left_join(lookup$districts %>% rename(d_lng = lng, d_lat = lat), by = c("city", "district")) %>%
    # 最後一道防線：連區都對不到時，用縣市內各區代表點的平均位置
    left_join(lookup$districts %>% group_by(city) %>% summarise(c_lng = mean(lng), c_lat = mean(lat)),
              by = "city") %>%
    mutate(geo_level = coalesce(geo_level, if_else(!is.na(d_lng), "district",
                                                   if_else(!is.na(c_lng), "city", "none"))),
           lng = coalesce(lng, d_lng, c_lng), lat = coalesce(lat, d_lat, c_lat),
           geo_source = "osm_road_v1", geocoded_at = Sys.time()) %>%
    select(addr_key, city, district, lng, lat, geo_level, geo_source, geocoded_at)
}

# --- 4. 主程式：只處理「還沒有座標」的地址 (增量) ------------------------------
if (sys.nframe() == 0 || interactive()) {
  lookup_file <- file.path(geo_dir, "road_lookup.rds")
  lookup <- if (file.exists(lookup_file)) readRDS(lookup_file) else {
    l <- build_road_lookup(); saveRDS(l, lookup_file); l
  }
  message(">> 道路對照表：", nrow(lookup$roads), " 條 (縣市+鄉鎮+路名)")

  con <- db_connect()
  dbExecute(con, "SET search_path TO re_v2;   -- 只放 re_v2，避免誤動 public 的表")

  # geo_cache：一個地址只算一次，之後每週更新只會算新出現的地址
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS geo_cache (
      addr_key    TEXT PRIMARY KEY,      -- PRIMARY KEY = 不可重複 + 自動建索引
      city TEXT, district TEXT,
      lng DOUBLE PRECISION, lat DOUBLE PRECISION,
      geo_level   TEXT,                  -- lane2 / lane / road_sec / road / district / none
      geo_source  TEXT,                  -- 之後升級成門牌級時可以知道是哪一版算的
      geocoded_at TIMESTAMPTZ
    );")

  # LEFT JOIN ... WHERE g.addr_key IS NULL = 「在 raw_trade 有、在 geo_cache 沒有」的地址
  # 注意：同一個地址在不同交易裡，偶爾會被登記成不同的行政區 (例如邊界上的地址、登錄錯誤)。
  # 如果寫 SELECT DISTINCT addr_key, city, district，同一個地址會出現兩次 → 違反 PRIMARY KEY。
  # 所以用 GROUP BY addr_key，行政區取「出現最多次的那個」(MODE() 是 PostgreSQL 的眾數函數)。
  todo <- dbGetQuery(con, "
    SELECT r.addr_key,
           MODE() WITHIN GROUP (ORDER BY r.city)     AS city,
           MODE() WITHIN GROUP (ORDER BY r.district) AS district
    -- 成屋 (raw_trade) 和預售屋 (raw_presale) 的地址一起處理；UNION ALL = 把兩張表上下接起來
    FROM (SELECT addr_key, city, district FROM raw_trade
          UNION ALL
          SELECT addr_key, city, district FROM raw_presale) r
    LEFT JOIN geo_cache g ON r.addr_key = g.addr_key
    WHERE r.addr_key IS NOT NULL AND g.addr_key IS NULL
    GROUP BY r.addr_key;") %>% as_tibble()
  message(">> 待處理地址：", nrow(todo))

  if (nrow(todo) > 0) {
    res <- geocode_keys(todo, lookup)
    dbAppendTable(con, "geo_cache", as.data.frame(res))
    print(count(res, geo_level) %>% mutate(pct = round(n / sum(n) * 100, 1)))
  }
  dbExecute(con, "ANALYZE geo_cache;")
  dbDisconnect(con)
}

# ==============================================================================
# Phase 2（選做）：升級成門牌級
#   A. 自架 Nominatim (Docker + 台灣 OSM)：完全免費、無次數限制，但台灣門牌在 OSM 覆蓋率有限。
#   B. 申請 TGOS「全國門牌地址定位服務」APIKey：門牌級最準，但有每日次數限制，
#      可以只拿來升級「交易量最大的那幾萬個地址」，其他維持巷弄級。
#   做法都一樣：挑出 geo_level 不是門牌級的 addr_key → 呼叫服務 → UPDATE geo_cache，
#   geo_source 改成新版本名稱即可。
# ==============================================================================
