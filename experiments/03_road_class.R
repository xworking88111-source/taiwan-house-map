# ==============================================================================
# 實驗 03：臨路等級 (房子面對的是大馬路、小路、還是巷子裡？)
#
# 直覺：同一區的透天，臨 30 米大馬路的 (可做店面) 和巷弄深處的，單價可以差 3 成以上。
#       目前模型只知道「座標」，不知道「門口是什麼路」。
#
# 特徵 (全部只用「地址文字 + 座標 + OSM 道路圖」就能算 → App 估價時一樣算得出來，不會有 training-serving skew)：
#   lane_depth   巷弄深度：0 = 直接面對路/街，1 = 在「巷」裡，2 = 在「弄」裡    ← 解析地址文字
#   road_type    路名尾字：3 = 大道，2 = 路，1 = 街，0 = 其他 (村里、無路名)     ← 解析地址文字
#   road_class   門牌所在道路在 OSM 的等級 (5 = 省道/主要幹道 … 1 = 服務道路)，同名道路取 3 公里內最高等級
#   d_major_road 到最近「主要道路」(OSM trunk/primary/secondary) 的距離 (公尺)
#
# OSM 道路等級 (fclass) 是全世界通用的分類，台灣大致對應：
#   motorway = 國道 (高速公路，住家不能直接臨接，另外處理)、trunk = 快速道路/省道、primary = 主要幹道、
#   secondary = 次要幹道、tertiary = 一般道路、residential = 社區道路、service = 巷道/停車場通道
#
# 比較對象：實驗 02 (01A 設定 + 同棟歷史)，同種子，唯一差別是多了上面 4 個特徵
# ==============================================================================
source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "new_model_experiments.R"))
library(data.table)
t0 <- Sys.time()

# --- 1. 地址文字 → 巷弄深度、路名 ---------------------------------------------------
# address_norm 例：臺北市內湖區康樂街136巷15弄32號十一樓之4
#   去掉「縣市+鄉鎮區」後，抓第一個「…路 / …街 / …大道」當路名 (和地理編碼 new_geocode.R 同一套規則)
parse_road <- function(addr, city, district) {
  rest <- str_remove(str_remove(addr, fixed(city)), fixed(district))
  tibble(road = str_match(rest, "^(.+?(大道|路|街))")[, 2],
         lane_depth = case_when(str_detect(rest, "\\d+弄") ~ 2L, str_detect(rest, "\\d+巷") ~ 1L, TRUE ~ 0L),
         road_type = case_when(str_detect(road, "大道$") ~ 3L, str_detect(road, "路$") ~ 2L,
                               str_detect(road, "街$") ~ 1L, TRUE ~ 0L))
}
df <- bind_cols(df, parse_road(df$address_norm, df$city, df$district))

# --- 2. OSM 道路圖：讀「有名字或是主要道路」的路段 ------------------------------------
fclass_rank <- c(trunk = 5, primary = 5, secondary = 4, tertiary = 3, residential = 2,
                 unclassified = 2, living_street = 2, service = 1, motorway = 0)
roads <- st_read(file.path(project_path, "geo_data", "gis_osm_roads_free_1.shp"), quiet = TRUE,
                 query = paste0("SELECT name, fclass FROM gis_osm_roads_free_1 WHERE fclass IN (",
                                paste0("'", names(fclass_rank), "'", collapse = ","), ")")) %>%
  st_transform(3826)                                               # TWD97 公尺座標，和 tx/ty 一致

# 2a. 到主要道路的距離：把路線每 50 公尺切一個點，再用 k-d tree 找最近點 (誤差最多約 25 公尺)
major <- roads %>% filter(fclass %in% c("trunk", "primary", "secondary")) %>%
  st_segmentize(dfMaxLength = 50) %>% st_coordinates()
df$d_major_road <- round(RANN::nn2(major[, 1:2], cbind(df$tx, df$ty), k = 1)$nn.dists[, 1])
message("主要道路取樣點 ", nrow(major), " 個")

# 2b. 門牌所在道路的等級：OSM 路段名稱 (忠孝東路四段) 去掉「段」→ 忠孝東路，
#     以 2 公里方格彙總「這個方格裡這條路的最高等級」，再讓每筆交易查周圍 3×3 格 (約 3 公里內)
#     為什麼不直接用路名查？全台有幾百條「中山路」，等級各不相同，必須限定在附近
seg_xy <- st_coordinates(suppressWarnings(st_point_on_surface(roads)))
osm <- data.table(road = str_match(roads$name, "^(.+?(大道|路|街))")[, 2],
                  rank = fclass_rank[roads$fclass],
                  gx = floor(seg_xy[, 1] / 2000), gy = floor(seg_xy[, 2] / 2000))[!is.na(road)]
osm <- osm[, .(rank = max(rank)), by = .(road, gx, gy)]
osm9 <- rbindlist(lapply(-1:1, function(dx) lapply(-1:1, function(dy)       # 每格複製到周圍 9 格
  osm[, .(road, gx = gx + dx, gy = gy + dy, rank)])) %>% unlist(recursive = FALSE))[, .(rank = max(rank)), by = .(road, gx, gy)]
q <- data.table(road = df$road, gx = floor(df$tx / 2000), gy = floor(df$ty / 2000))
df$road_class <- osm9[q, on = .(road, gx, gy), x.rank]
message("道路等級對到的比例 ", scales::percent(mean(!is.na(df$road_class)), .1),
        "；特徵計算 ", round(as.numeric(Sys.time() - t0, units = "secs")), " 秒")

# 檢查：特徵和單價的關係合不合直覺 (同區、同型態比較才公平 → 看透天在各組的「相對單價」)
print(df %>% filter(b_category == "透天厝") %>% group_by(city_district) %>%
        mutate(rel = y - median(y)) %>% ungroup() %>%
        mutate(major_band = cut(d_major_road, c(-1, 30, 100, 300, Inf), c("≤30m", "30-100m", "100-300m", ">300m"))) %>%
        group_by(lane_depth, major_band) %>% summarise(n = n(), 相對單價 = round(exp(mean(rel)) - 1, 3), .groups = "drop"))

# --- 3. 回測：實驗 02 的特徵 vs 再加臨路等級 -----------------------------------------
df <- bind_cols(df, building_history(df$addr_key, df$t_month_idx, df$y, cap_month = cut_month))
sp <- split_data(df)
feats02 <- c(features_base, "bld_lp", "bld_n", "bld_dt")
feats03 <- c(feats02, "lane_depth", "road_type", "road_class", "d_major_road")
p02 <- predict_xgb(fit_xgb(sp, feats = feats02, seed = 1), sp$test, feats = feats02)
m03 <- fit_xgb(sp, feats = feats03, seed = 1)
p03 <- predict_xgb(m03, sp$test, feats = feats03)
log_experiment("03 基準(=02) 1×1500", "01A 設定 + 同棟歷史，重跑當基準", score(p02, sp$test))
log_experiment("03 臨路等級 1×1500", "02 + lane_depth/road_type/road_class/d_major_road",
               bind_cols(score(p03, sp$test), tibble(模型大小MB = model_size_mb(m03))))
ape <- function(p) abs(exp(p) - exp(sp$test$y)) / exp(sp$test$y)
print(tibble(類別 = sp$test$b_category, 巷弄 = sp$test$lane_depth, a = ape(p02), b = ape(p03)) %>%
        group_by(類別, 巷弄) %>% summarise(n = n(), 原本MAPE = mean(a), 加臨路MAPE = mean(b), .groups = "drop"))
print(xgb.importance(model = m03) %>% filter(Feature %in% feats03[-(1:length(feats02))]))
