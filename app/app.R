# ==============================================================================
# 台灣房價地圖 Shiny App (第一版，2026-10-27 第二次修訂：加入五種交易類別)
#
# 三個主要功能：
#   1. 地圖：全台/縣市「行政區行情面量圖」 + 點進行政區後看「每一筆成交 (集群)」
#   2. 市場探勘：趨勢、成交量、型態占比、單價分布、行政區排行、屋齡與單價
#   3. AI 估價：XGBoost 模型預測單價與 P10~P90 行情區間 + 附近相似成交 + Claude 複檢說明
#
# 資料來源：app/data/*.rds (由 new_export_app_data.R 從 PostgreSQL 匯出)
# 這個 App 不連資料庫 → 可以直接部署到免費的 shinyapps.io
#
# Shiny 的基本觀念 (看不懂程式時先看這段)：
#   - ui     ：畫面長什麼樣子 (輸入元件、輸出位置)
#   - server ：收到輸入後要算什麼、畫什麼
#   - reactive({...})：「會自動重算的值」。它用到的輸入改變時才重算，沒改變就用快取
#   - observeEvent(input$x, {...})：「當 x 被觸發時做某件事」(例如按按鈕)
#   - render*() / *Output()：成對出現。server 端 renderPlotly 產生圖，ui 端 plotlyOutput 決定放哪
# ==============================================================================
library(shiny)
library(bslib)       # Bootstrap 5 主題與現代化版面 (卡片、側邊欄、數值框)
library(leaflet)     # 互動地圖
library(plotly)      # 互動圖表
library(dplyr)
library(sf)          # 地理資料 (行政區多邊形)
library(xgboost)     # 載入訓練好的模型
# 小提醒：R 後載入的套件會「蓋掉」同名函數 (masking)。例如 library(jsonlite) 會蓋掉 Shiny 的 validate()、
# library(httr) 會蓋掉 plotly 的 config()。需要用到這類套件時，請用 jsonlite::toJSON() 這種寫法，不要整個載入。

# --- 1. 載入資料 (App 啟動時只做一次，所有使用者共用) --------------------------
# 交易資料已經在 new_export_app_data.R 依類別切成 5 個檔 (省記憶體，見該檔說明)
meta <- readRDS("data/meta.rds")
trades_by_cat <- lapply(setNames(nm = names(meta$cat_counts)),
                        function(cc) readRDS(file.path("data", paste0("trades_", cc, ".rds"))))
towns  <- readRDS("data/towns.rds")
roads  <- readRDS("data/road_lookup.rds")
model  <- readRDS("models/price_model.rds")
# 空間特徵 (v2 模型新增)：設施座標、附近行情表，以及和訓練程式共用的計算函數
source("shared_features.R")
facilities <- readRDS("data/facilities.rds")
nbr_tbl    <- readRDS("data/nbr_table.rds")
# 屋況 / 產權條件的影響：由 new_condition_adjust.R 用實價登錄「備註」+ 交叉預測量出來的
cond_tbl   <- if (file.exists("data/condition_adjust.rds")) readRDS("data/condition_adjust.rds") else NULL
xgb_point <- xgb.load("models/xgb_point.ubj")   # 點估計模型
xgb_quant <- xgb.load("models/xgb_quant.ubj")   # 分位數模型 (P10/P50/P90)

data_months <- meta$data_months
city_order  <- c("臺北市", "新北市", "桃園市", "臺中市", "臺南市", "高雄市", "基隆市", "新竹市",
                 "嘉義市", "新竹縣", "苗栗縣", "彰化縣", "南投縣", "雲林縣", "嘉義縣", "屏東縣",
                 "宜蘭縣", "花蓮縣", "臺東縣", "澎湖縣", "金門縣", "連江縣")
cities      <- intersect(city_order, meta$city_levels)
# 建物型態：住宅 5 種 + 非住宅 4 種 (店面、辦公商業、農舍、廠辦工廠)
# 估價模型只用住宅 5 種訓練 (非住宅的定價邏輯差很多，樣本也少)
types       <- c("電梯大樓", "華廈", "公寓", "透天厝", "套房", "店面", "辦公商業", "農舍", "廠辦工廠")
home_types  <- types[1:5]
# 配色原則：每種型態的顏色「色相」要差很多，一眼就分得出來
# (第一版店面用粉紅、透天厝用紅色，兩條線幾乎分不出來)
type_colors <- c("電梯大樓" = "#0F766E", "華廈" = "#2563EB", "公寓" = "#F59E0B",
                 "透天厝" = "#DC2626", "套房" = "#7C3AED", "店面" = "#92400E", "辦公商業" = "#374151",
                 "農舍" = "#65A30D", "廠辦工廠" = "#9CA3AF", "土地" = "#4D7C0F", "車位" = "#475569")

# 五種交易類別 (見 new_export_app_data.R 的說明)
categories <- c("成屋" = "成屋", "預售屋 (契約)" = "預售屋", "預售交屋" = "預售交屋",
                "土地" = "土地", "車位" = "車位")
cat_colors <- c("成屋" = "#0F766E", "預售屋" = "#2563EB", "預售交屋" = "#7C3AED",
                "土地" = "#65A30D", "車位" = "#475569")
building_cats <- c("成屋", "預售屋", "預售交屋")    # 有建物、可以畫到「每棟建物」的類別

# 每種類別要看的「價格指標」：建物類 = 每坪單價；土地 = 每坪土地單價；車位 = 每個車位的總價
metric_of   <- function(d, cat) if (identical(cat, "車位")) d$total_wan else d$unit_wan
metric_unit <- function(cat) if (identical(cat, "車位")) "萬/個" else "萬/坪"
metric_name <- function(cat) switch(cat, "車位" = "車位價格", "土地" = "土地單價", "單價")
years       <- as.integer(format(data_months, "%Y"))

# 各縣市的行政區清單：啟動時先算好 (之後會把完整的 trades 刪掉省記憶體)
district_list <- meta$district_list
districts_of <- function(city) district_list[[city]] %||% character(0)
# 底圖：Esri 淺灰底圖 (免金鑰、低彩度，讓資料顏色更突出)，另外提供 OpenStreetMap 可切換。
# (原本用的 CARTO 底圖現在需要 API 金鑰，畫面會出現「API KEY REQUIRED」浮水印)
add_basemap <- function(map) {
  map %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas, group = "淺灰底圖") %>%
    addProviderTiles(providers$OpenStreetMap, group = "街道地圖") %>%
    addLayersControl(baseGroups = c("淺灰底圖", "街道地圖"), position = "topright",
                     options = layersControlOptions(collapsed = TRUE))
}
# 配色：優先用「分位數」切 7 級 (房價差距很大，等距切會幾乎全部同色)。
# ⚠️ 只選一個行政區、或資料很少時，分位數的切點會重複，colorQuantile 會直接報錯
#    → 整個 Shiny 連線斷掉、畫面變灰 (第一版的 bug)。所以切點重複時改用連續色階。
make_pal <- function(values) {
  values <- values[!is.na(values)]
  br <- unique(quantile(values, seq(0, 1, length.out = 8), names = FALSE))
  if (length(br) >= 3) colorBin("YlOrRd", domain = values, bins = br)
  else colorNumeric("YlOrRd", domain = range(values) + c(-0.5, 0.5))
}
fmt_wan <- function(x, digits = 1) ifelse(is.na(x), "—", formatC(x, format = "f", digits = digits, big.mark = ","))

# --- 2. 共用的篩選函數 --------------------------------------------------------
# 三個分頁都要「依縣市/區/型態/年份篩選」，寫成一個函數避免重複
# category：要看哪幾種交易類別 (可以多選，例如探勘頁的「類別占比」要看全部)
# 土地、車位沒有「建物型態」和「屋齡」，所以這兩個條件只套用在建物類別上
filter_trades <- function(category, city, district, type_sel, year_rng, age_rng = c(0, 70)) {
  d <- if (length(category) == 1) trades_by_cat[[category]] else do.call(rbind, unname(trades_by_cat[category]))
  if (!is.null(city) && city != "全台")        d <- d[d$city == city, ]
  if (!is.null(district) && district != "全部") d <- d[d$district == district, ]
  d <- d[d$b_category %in% c(type_sel, "土地", "車位"), ]
  d <- d[d$year >= year_rng[1] & d$year <= year_rng[2], ]
  # 屋齡：滑桿在預設的「全部範圍」時，連屋齡不明的交易也保留 (第一版會把它們濾掉)
  # (⚠️ 之前預設值寫 c(0, 100)、判斷卻寫 c(0, 70) → 探勘頁永遠被當成「有篩屋齡」，土地、車位全被濾掉)
  if (age_rng[1] > 0 || age_rng[2] < 70) d <- d[!is.na(d$age) & d$age >= age_rng[1] & d$age <= age_rng[2], ]
  d
}

# --- 3. 估價：把使用者輸入轉成模型看得懂的格式 ---------------------------------
# 這段必須和 new_model_training.R 的 prep_x() / to_matrix() 完全一致：
# 欄位順序、類別編號 (factor levels)、缺值填 -1。不一致時模型會「看錯欄位」而不會報錯，非常危險。
build_model_input <- function(inp) {
  d <- tibble(
    city = inp$city, district = inp$district, b_category = inp$type,
    house_age_at_trans = inp$age, pure_house_ping = inp$ping,
    land_ratio = if (is.na(inp$land) || inp$ping <= 0) NA_real_ else inp$land / inp$ping,
    floor_num = inp$floor, total_floors_num = inp$total_floors,
    floor_ratio = if (is.na(inp$floor) || is.na(inp$total_floors) || inp$total_floors <= 0) NA_real_
                  else inp$floor / inp$total_floors,
    is_top_floor = as.integer(!is.na(inp$floor) && identical(inp$floor, inp$total_floors)),
    rooms = inp$rooms, baths = inp$baths,
    elevator = as.integer(inp$elevator), management = as.integer(inp$management),
    parking = as.integer(inp$parking), parking_unknown = 0L,
    t_month_idx = model$t_month_idx_max,          # 用「資料中最新的月份」= 預測現在的行情
    lng = inp$lng, lat = inp$lat
  )
  # 空間特徵：到各類設施的距離、附近行情 (和訓練時用同一個函數算，見 shared_features.R)
  d <- cbind(d, inp$geo, inp$nbr)
  for (col in names(model$factor_levels)) {
    d[[col]] <- as.integer(factor(d[[col]], levels = model$factor_levels[[col]]))
  }
  d <- d[model$features]                          # 欄位順序對齊訓練時
  d[] <- lapply(d, function(x) { x <- as.numeric(x); x[is.na(x)] <- -1; x })
  as.matrix(d)
}

predict_price <- function(inp) {
  x <- build_model_input(inp)
  q  <- predict(xgb_quant, x)                     # 模型預測的是 log(元/坪)
  cq <- model$conformal_q %||% 0                  # 保形校正加寬量 (訓練時算好的，見 new_model_training.R)
  list(point = exp(predict(xgb_point, x)),        # exp() 把 log 轉回 元/坪
       p10 = exp(q[1] - cq), p50 = exp(q[2]), p90 = exp(q[3] + cq))
}

# 找「相似成交」：同區、同型態、屋齡 ±8 年、坪數 ±40%，依距離排序取最近 10 筆
# 距離用簡化公式：經緯度差換算公尺 (台灣緯度約 23 度，1 度經度 ≈ 102 公里、1 度緯度 ≈ 111 公里)
find_comparables <- function(inp, n = 10) {
  h <- trades_by_cat[["成屋"]]
  d <- h[h$city == inp$city & h$district == inp$district & h$b_category == inp$type, ]
  d <- d[!is.na(d$age) & abs(d$age - inp$age) <= 8 & d$ping >= inp$ping * 0.6 & d$ping <= inp$ping * 1.4, ]
  if (nrow(d) == 0) return(d)
  d$dist_m <- sqrt(((d$lng - inp$lng) * 102000)^2 + ((d$lat - inp$lat) * 111000)^2)
  d %>% arrange(dist_m, desc(trans_ym)) %>% head(n)
}

# --- 3b. 土地持分推估 --------------------------------------------------------
# ⚠️ 重要 bug 修正 (2026-10-27)：訓練資料裡「每一筆」華廈、大樓、公寓都有土地持分坪數，
#    但第一版 App 只有透天厝才問土地坪數，其他型態一律填 -1 (缺值)。
#    -1 是模型訓練時從沒看過的值 → 預測整個跑掉：三民區鼎祥街的華廈估 14.0 萬/坪，
#    附近實際成交卻是 19.2 萬/坪 (低估約 27%)。這叫「訓練與上線不一致」(training-serving skew)。
#    修法：使用者不知道土地持分時，用「同縣市、同區、同型態」的土地比例中位數推估。
land_ratio_tbl <- meta$land_ratio_tbl     # 在 new_export_app_data.R 先算好
land_ratio_city <- land_ratio_tbl %>% group_by(city, b_category) %>% summarise(r = median(r), .groups = "drop")
cat_counts <- meta$cat_counts
impute_land_ratio <- function(city, district, type) {
  r <- land_ratio_tbl$r[land_ratio_tbl$city == city & land_ratio_tbl$district == district &
                          land_ratio_tbl$b_category == type & land_ratio_tbl$n >= 10]
  if (length(r) == 0) r <- land_ratio_city$r[land_ratio_city$city == city & land_ratio_city$b_category == type]
  if (length(r) == 0) r <- median(land_ratio_tbl$r[land_ratio_tbl$b_category == type])
  r[1]
}

# --- 3c. 地址定位：使用者可以輸入完整門牌，也可以只填路名 -----------------------
# 第一版的輸入框只接受「清單裡的路名」，打完整地址 (例如 中山一路100號三樓) 會被直接清掉。
# 現在的流程 (和 new_geocode.R 的離線地理編碼同一套邏輯)：
#   1. 標準化：全形數字→半形、台→臺、去掉縣市區前綴、去掉樓層 (「號」之後的字)
#   2. 這棟建物在資料庫裡有成交紀錄 → 直接用這棟的位置 (最準)，並列出它的歷史成交
#   3. 沒有 → 依序找 弄 → 巷 → 路+段 → 路 的代表點
#   4. 都找不到 → 行政區中心
cn_digit <- c("1" = "一", "2" = "二", "3" = "三", "4" = "四", "5" = "五", "6" = "六", "7" = "七", "8" = "八", "9" = "九")
first_match <- function(pattern, x) { m <- regmatches(x, regexpr(pattern, x, perl = TRUE)); if (length(m)) m else NA }
locate_address <- function(city, district, text) {
  dd <- roads$districts[roads$districts$city == city & roads$districts$district == district, ]
  out <- list(lng = dd$lng[1], lat = dd$lat[1], level = "行政區中心", label = paste0(city, district), history = NULL)
  if (is.null(text) || !nzchar(trimws(text))) return(out)
  a <- chartr("０１２３４５６７８９－", "0123456789-", text)
  a <- gsub("台", "臺", gsub("巿", "市", gsub("\\s", "", a)))
  a <- sub(paste0("^", city), "", a); a <- sub(paste0("^", district), "", a)
  a <- sub("^[^0-9路街道巷弄段]{1,4}[村里](?=\\d+鄰|[^0-9]+(路|街|大道|巷))", "", a, perl = TRUE)
  a <- sub("^\\d+鄰", "", a)
  # 「4段」→「四段」(OSM 路名和實價登錄都用國字)
  for (k in names(cn_digit)) a <- gsub(paste0(k, "段"), paste0(cn_digit[k], "段"), a, fixed = TRUE)
  bld <- sub("(號).*$", "\\1", a)                         # 去掉樓層：「100號三樓」→「100號」
  # (2) 同一棟建物的歷史成交
  if (grepl("號$", bld)) {
    key <- paste0(city, district, bld)
    # address 欄位就是建物門牌 (匯出時已經把樓層拿掉，見 new_export_app_data.R)
    h <- rbind(trades_by_cat[["成屋"]][trades_by_cat[["成屋"]]$address %in% key, ],
               trades_by_cat[["預售交屋"]][trades_by_cat[["預售交屋"]]$address %in% key, ])
    if (nrow(h) > 0) {
      h <- h[order(h$trans_ym, decreasing = TRUE), ]
      return(list(lng = h$lng[1], lat = h$lat[1], level = "這棟建物 (資料庫有成交紀錄)", label = bld, history = h))
    }
  }
  # (3) 巷弄 / 路段
  cands <- c(first_match("^.+?(路|街|大道).*?巷.*?弄", a), first_match("^.+?(路|街|大道).*?巷", a),
             first_match("^.+?(路|街|大道)[一二三四五六七八九十]{1,2}段", a), first_match("^.+?(路|街|大道)", a),
             first_match("^[^0-9路街]+?巷", a))
  lv <- c("巷弄", "巷弄", "路段", "路名", "巷弄")
  rr <- roads$roads[roads$roads$city == city & roads$roads$district == district, ]
  for (j in seq_along(cands)) {
    if (is.na(cands[j])) next
    hit <- rr[rr$name == cands[j], ]
    if (nrow(hit) > 0) return(list(lng = hit$lng[1], lat = hit$lat[1], level = lv[j], label = cands[j], history = NULL))
  }
  out$level <- "行政區中心 (找不到這個地址，請確認行政區是否正確)"
  out
}

# --- 3d. 周邊環境的白話說明 (估價依據、AI 提示詞共用) ---------------------------
fmt_dist <- function(m) ifelse(m >= 50000, "50 公里以上", ifelse(m >= 1000, sprintf("%.1f 公里", m / 1000), sprintf("%d 公尺", as.integer(round(m, -1)))))
describe_surroundings <- function(g, nbr, type) {
  transit <- c(mrt = "捷運", lrt = "輕軌", tra = "台鐵", hsr = "高鐵", airport = "機場")
  tr <- sapply(names(transit), function(k) g[[paste0("d_", k)]])
  tr_txt <- paste0("最近的", transit[tr < 15000], ifelse(names(transit)[tr < 15000] == "airport", " ", "站 "),
                   fmt_dist(tr[tr < 15000]), collapse = "、")
  life <- sprintf("500 公尺內公車站 %d 個、便利商店 %d 間；最近的學校 %s、公園 %s、市場/超市 %s、醫院 %s",
                  g$n_bus_500, g$n_convenience_500, fmt_dist(g$d_school), fmt_dist(g$d_park),
                  fmt_dist(g$d_market), fmt_dist(g$d_hospital))
  # 嫌惡設施：300 公尺內才提醒 (再遠影響通常不大；距離是到設施邊界)
  nimby <- c(cemetery = "墓地", landfill = "垃圾場/資源回收場", wastewater = "污水處理廠",
             industrial = "工業區", military = "軍事用地", fuel = "加油站")
  nd <- sapply(names(nimby), function(k) g[[paste0("d_", k)]])
  near <- nd < 300
  list(transit = if (nzchar(tr_txt)) tr_txt else "15 公里內沒有軌道運輸站",
       life = life,
       nimby = if (any(near)) paste0(nimby[near], " ", fmt_dist(nd[near]), collapse = "、") else "300 公尺內沒有",
       nbr = if (is.na(nbr$nbr_lp)) sprintf("附近同型態成交太少 (%d 筆)，改看其他特徵", nbr$nbr_n)
             else sprintf("附近約 500 公尺、近 12 個月 %d 筆%s成交，平均約 %.1f 萬/坪", nbr$nbr_n, type, exp(nbr$nbr_lp) / 1e4))
}

# --- 3e. 屋況 / 產權調整 -----------------------------------------------------
# 模型的估價 = 「一般屋況」的行情 (模型看不到裝潢、屋況，所以學到的是平均)。
# 使用者選了屋況、特殊情況後，用實際成交量出來的影響幅度調整 (多個條件相乘)。
# (「毛胚」原本也是選項，但成屋裡備註寫毛胚的只有 2 筆 (毛胚屋大多是預售屋交屋)，量不出可信的影響 → 拿掉)
cond_levels <- c("需大整修 (漏水、壁癌、老舊)" = "屋況不佳", "一般屋況" = "一般", "有裝潢 / 附家具家電" = "含裝潢")
special_levels <- c("帶租約" = "帶租約", "凶宅" = "凶宅", "海砂屋 / 輻射屋" = "海砂輻射")
get_adj <- function(cond, type) {
  if (is.null(cond_tbl) || cond == "一般") return(list(adj = 0, n = NA, pooled = FALSE))
  tg <- if (type == "套房") "電梯大樓" else type
  r <- cond_tbl[cond_tbl$condition == cond & cond_tbl$type_grp == tg, ]
  if (nrow(r) == 0) r <- cond_tbl[cond_tbl$condition == cond, ][1, ]   # 這個型態沒有樣本 → 用合併結果
  # 樣本不足 (連各型態合併都不到 30 筆)：不給百分比，避免用十幾筆資料硬算出一個看似精確的數字
  if (isTRUE(r$insufficient[1])) return(list(adj = 0, n = r$n[1], pooled = TRUE, insufficient = TRUE))
  list(adj = r$adj[1], n = r$n[1], pooled = r$use_pooled[1], insufficient = FALSE)
}
total_adj <- function(cond, specials, type) {
  parts <- c(cond, specials)
  a <- lapply(parts, get_adj, type = type); names(a) <- parts
  list(factor = prod(sapply(a, function(x) 1 + x$adj)), parts = a)
}

# --- 4. AI 複檢：產生「提示詞」，讓使用者貼到自己的 AI 工具 ----------------------
# 為什麼不在 App 裡直接呼叫 AI？
#   呼叫 Claude / GPT 的 API 要付費，用的是「開發者自己的金鑰」：使用者越多，帳單越高。
#   免費作品集沒辦法負擔，所以改成：App 把「物件條件 + 模型估價 + 10 筆相似成交」整理成提示詞，
#   使用者複製後貼到自己的 Gemini / ChatGPT / Claude → 開發者成本 0，使用者也能自由追問。
# 設計原則不變：AI 不自己猜價格，而是根據我們提供的實際資料「解釋與檢查」模型結果。
# (之後如果要做成內建功能，可以用 Claude Haiku 這類便宜的模型 + 每人每天次數上限，見學習筆記)
build_ai_prompt <- function(inp, pred, comps) {
  comp_txt <- if (nrow(comps) == 0) "（附近沒有找到條件相近的成交）" else paste(sprintf(
    "- %s｜%s｜%.1f 坪｜屋齡 %d 年｜單價 %.1f 萬/坪｜總價 %s 萬｜距離約 %d 公尺",
    format(comps$trans_ym, "%Y-%m"), comps$address, comps$ping, as.integer(comps$age),
    comps$unit_wan, fmt_wan(comps$total_wan, 0), as.integer(comps$dist_m)), collapse = "\n")

  sprintf(paste(
    "你是台灣不動產估價的審查員。以下是一個機器學習模型（XGBoost，以內政部實價登錄 5 年資料訓練）",
    "對某物件的估價，以及實價登錄中附近條件相近的實際成交。",
    "請用繁體中文、台灣用語，寫一段給一般買方看的複檢說明（400 字以內）：",
    "1) 模型估價和附近成交是否一致；2) 哪些條件可能讓實際價格偏高或偏低；",
    "3) 這個估價有哪些限制、看屋時需要實地確認什麼。只根據提供的資料推論，不要編造資料以外的成交數字。",
    "", "【物件】%s%s，%s，屋齡 %d 年，建物 %.1f 坪%s，%s%s",
    "【模型估價】單價 %.1f 萬/坪（行情區間 P10~P90：%.1f ~ %.1f 萬/坪），總價約 %s 萬",
    "【屋況與產權】%s",
    "【周邊環境】%s",
    "【附近相似成交】", "%s", sep = "\n"),
    inp$city, inp$district, inp$type, as.integer(inp$age), inp$ping,
    if (!is.na(inp$land)) sprintf("，土地 %.1f 坪", inp$land) else "",
    if (inp$parking) "含車位" else "無車位", if (nzchar(inp$road)) paste0("，位於", inp$road, "附近") else "",
    pred$point / 1e4, pred$p10 / 1e4, pred$p90 / 1e4, fmt_wan(pred$point * inp$ping / 1e4, 0),
    inp$cond_txt %||% "一般屋況",
    with(describe_surroundings(inp$geo, inp$nbr, inp$type),
         paste0("交通：", transit, "。生活機能：", life, "。300 公尺內的嫌惡設施：", nimby, "。", nbr)),
    comp_txt)
}

# --- 5. UI --------------------------------------------------------------------
theme <- bs_theme(
  version = 5,
  primary = "#0F766E", secondary = "#64748B", success = "#15803D", danger = "#B91C1C",
  bg = "#F8FAFC", fg = "#0F172A",
  base_font = font_google("Noto Sans TC"), heading_font = font_google("Noto Sans TC"),
  "navbar-bg" = "#0F172A", "card-border-radius" = "0.75rem"
)

year_slider <- function(id) {
  sliderInput(id, "交易年份", min = years[1], max = years[2], value = years, step = 1, sep = "")
}

ui <- page_navbar(
  title = tags$span(tags$b("台灣房價地圖"), tags$small(" 實價登錄 5 年", style = "opacity:.7")),
  theme = theme,
  fillable = TRUE,
  header = tags$head(
    # 手機上按下「開始估價」：自動收起輸入表單、捲到估價結果 (否則表單會蓋住結果，使用者以為沒反應)
    tags$script(HTML("
      $(document).on('click', '#p_go', function() {
        if (window.innerWidth >= 768) return;
        const lay = $(this).closest('.bslib-sidebar-layout')[0];
        if (lay && !lay.classList.contains('sidebar-collapsed')) lay.querySelector(':scope > .collapse-toggle').click();
        setTimeout(() => document.getElementById('p_unit').scrollIntoView({behavior: 'smooth', block: 'start'}), 450);
      });")),
    tags$style(HTML("
    .bslib-value-box .value-box-title { font-size: .85rem; opacity: .85; }
    .leaflet-container { border-radius: .75rem; }
    .legend { font-family: 'Noto Sans TC', sans-serif; }
    .ai-box { white-space: pre-wrap; line-height: 1.8; }
    .small-note { font-size: .8rem; color: #64748B; }
    /* 相似成交明細：字大一點；除了地址以外的欄位不換行 (第一版日期、距離被拆成兩行) */
    .comps-table table { font-size: .95rem; width: 100%; }
    .comps-table td, .comps-table th { padding: .45rem .6rem !important; vertical-align: middle; }
    .comps-table td:not(:nth-child(2)), .comps-table th { white-space: nowrap; }
    /* 手機寬度不夠時：表格可以左右滑動，地址欄至少 11 個字寬 (否則會被擠成一個字一行) */
    .comps-table { overflow-x: auto; }
    .comps-table td:nth-child(2) { min-width: 11em; }
    .basis-list { margin: 0; padding-left: 1.1rem; line-height: 1.9; }
    /* 手機 (寬度 < 576px)：數值框改成 2×2、隱藏大圖示，把空間留給地圖 */
    @media (max-width: 575.98px) {
      .vb-row { grid-template-columns: repeat(2, 1fr) !important; gap: .5rem !important; }
      .vb-row > * { grid-column: auto !important; }
      .vb-row .value-box-showcase { display: none !important; }
      .vb-row .value-box-value { font-size: 1.2rem; }
    }
  "))),

  # 【分頁 1：地圖】
  nav_panel(
    "地圖", icon = icon("map-location-dot"),
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        radioButtons("m_cat", "交易類別", categories, selected = "成屋"),
        selectInput("m_city", "縣市", c("全台", cities)),
        selectInput("m_district", "行政區", "全部"),
        # conditionalPanel：條件成立才顯示 (條件是 JavaScript 語法)。土地、車位沒有門牌，只能畫面量圖
        conditionalPanel(
          "['成屋', '預售屋', '預售交屋'].includes(input.m_cat)",
          radioButtons("m_mode", "顯示方式",
                       c("行政區行情 (面量圖)" = "choropleth", "每棟建物 (集群)" = "points")),
          checkboxGroupInput("m_type", "建物型態", types, selected = home_types)
        ),
        year_slider("m_year"),
        conditionalPanel("['成屋', '預售交屋'].includes(input.m_cat)",
                         sliderInput("m_age", "屋齡", 0, 70, c(0, 70), step = 1, post = " 年")),
        div(class = "small-note",
            "選擇行政區後會自動顯示該區的每一棟建物；點地圖上的行政區也可以直接放大。",
            "建物位置精準到巷弄/路段 (非實際門牌)；土地的地址是地號，只能顯示到行政區。")
      ),
      layout_columns(
        fill = FALSE,
        class = "vb-row",          # 手機版的 2×2 排法寫在最上面的 CSS (.vb-row)
        value_box("成交筆數", textOutput("m_n"), showcase = icon("house"), theme = "primary", max_height = "110px"),
        value_box(textOutput("m_unit_title"), textOutput("m_unit"), showcase = icon("tag"), theme = "secondary", max_height = "110px"),
        value_box("總價中位數", textOutput("m_total"), showcase = icon("sack-dollar"), theme = "secondary", max_height = "110px"),
        value_box(textOutput("m_extra_title"), textOutput("m_extra"), showcase = icon("hourglass-half"), theme = "secondary", max_height = "110px")
      ),
      # min_height：手機上 bslib 會關閉「填滿高度」，沒有最小高度的話地圖會變成 0 高
      card(full_screen = TRUE, min_height = "480px", leafletOutput("map", height = "100%"))
    )
  ),

  # 【分頁 2：市場探勘】
  nav_panel(
    "市場探勘", icon = icon("chart-column"),
    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        radioButtons("e_cat", "交易類別", categories, selected = "成屋"),
        selectInput("e_city", "縣市", c("全台", cities), selected = "高雄市"),
        selectInput("e_district", "行政區", "全部"),
        conditionalPanel("['成屋', '預售屋', '預售交屋'].includes(input.e_cat)",
                         checkboxGroupInput("e_type", "建物型態", types, selected = home_types)),
        year_slider("e_year")
      ),
      # fill = FALSE：每一列的高度跟著卡片走 (卡片固定 380px)，視窗變窄時卡片才不會互相重疊
      layout_columns(
        fill = FALSE,
        col_widths = c(8, 4, 6, 6, 6, 6),
        card(full_screen = TRUE, height = "400px", card_header(textOutput("e_trend_title")), plotlyOutput("e_trend"),
             card_footer(class = "small-note", textOutput("e_trend_note"))),
        card(full_screen = TRUE, height = "380px", card_header("各類交易筆數占比"), plotlyOutput("e_share")),
        card(full_screen = TRUE, height = "380px", card_header("每月成交筆數"), plotlyOutput("e_volume")),
        card(full_screen = TRUE, height = "380px", card_header(textOutput("e_hist_title")), plotlyOutput("e_hist")),
        card(full_screen = TRUE, height = "380px", card_header(textOutput("e_rank_title")), plotlyOutput("e_rank")),
        card(full_screen = TRUE, height = "400px", card_header(textOutput("e_age_title")), plotlyOutput("e_age"),
             card_footer(class = "small-note", textOutput("e_age_note")))
      )
    )
  ),

  # 【分頁 3：AI 估價】
  nav_panel(
    "AI 估價", icon = icon("wand-magic-sparkles"),
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        selectInput("p_city", "縣市", cities, selected = "高雄市"),
        selectInput("p_district", "行政區", districts_of("高雄市"), selected = "鳳山區"),
        # create = TRUE：允許輸入「清單裡沒有的文字」(完整門牌)，不會被清掉
        selectizeInput("p_road", "地址或路名 (選填，越精確越準)", choices = NULL,
                       options = list(create = TRUE, createOnBlur = TRUE, persist = FALSE,
                                      placeholder = "例如 中山一路100號3樓，或只填 瑞隆東路")),
        selectInput("p_type", "建物型態", home_types, selected = "透天厝"),
        layout_columns(
          numericInput("p_age", "屋齡 (年)", 35, min = 0, max = 80),
          numericInput("p_ping", "建物坪數", 82, min = 3, max = 500)
        ),
        numericInput("p_land", "土地持分坪數 (選填)", 30, min = 0, max = 500),
        div(class = "small-note mb-2", "不知道可以留空，系統會用同區、同型態的中位數推估。"),
        conditionalPanel("input.p_type != '透天厝'",
                         layout_columns(numericInput("p_floor", "所在樓層", 8, min = 1, max = 90),
                                        numericInput("p_tfloor", "總樓層", 15, min = 1, max = 90))),
        layout_columns(numericInput("p_rooms", "房", 3, min = 0, max = 20),
                       numericInput("p_baths", "衛", 2, min = 0, max = 20)),
        checkboxInput("p_parking", "含車位", FALSE),
        checkboxInput("p_elevator", "有電梯", FALSE),
        checkboxInput("p_mgmt", "有管理組織", FALSE),
        # 屋況、特殊情況：改變時估價會「立即」更新，不用重按開始估價
        radioButtons("p_cond", "屋況", cond_levels, selected = "一般"),
        checkboxGroupInput("p_special", "特殊情況 (可複選)", special_levels),
        actionButton("p_go", "開始估價", class = "btn-primary btn-lg w-100", icon = icon("calculator"))
      ),
      # 估價頁內容比較多 → 不要「塞進一個畫面」(fillable)，改成整頁往下捲動，卡片才不會被擠扁
      fillable = FALSE,
      layout_columns(
        fill = FALSE,
        # showcase_layout = "top right"：圖示縮小放右上角，不會把數字擠掉 (第一版「萬/坪」被切掉)
        value_box("預估單價", uiOutput("p_unit"), showcase = icon("tag"), showcase_layout = "top right", theme = "primary"),
        value_box("預估總價", uiOutput("p_total"), showcase = icon("sack-dollar"), showcase_layout = "top right", theme = "primary"),
        value_box("行情區間 (80% 的同條件交易落在這裡)", uiOutput("p_range"), showcase = icon("arrows-left-right"),
                  showcase_layout = "top right", theme = "secondary")
      ),
      layout_columns(
        fill = FALSE,
        col_widths = c(7, 5),
        card(full_screen = TRUE, height = "380px", card_header("附近相似成交 vs 預估"), plotlyOutput("p_plot")),
        card(full_screen = TRUE, height = "380px", card_header("物件位置與相似成交"), leafletOutput("p_map"))
      ),
      card(card_header(tags$span(icon("screwdriver-wrench"), " 屋況與產權對價格的影響")),
           plotlyOutput("p_cond_plot", height = "260px"),
           div(class = "small-note", uiOutput("p_cond_note"))),
      card(card_header(tags$span(icon("location-dot"), " 定位與估價依據")), uiOutput("p_basis")),
      uiOutput("p_history_card"),
      card(card_header("附近相似成交 (同區、同型態、屋齡 ±8 年、坪數 ±40%，由近到遠)"),
           div(class = "comps-table", tableOutput("p_comps"))),
      card(
        card_header(tags$span(icon("robot"), " AI 複檢")),
        p("把下面這段提示詞複製到你常用的 AI 工具，它會根據模型估價和附近的實際成交，",
          "用白話幫你檢查這個估價合不合理、看屋時要注意什麼。"),
        div(class = "d-flex flex-wrap gap-2",
            # 複製按鈕：用瀏覽器內建的 navigator.clipboard 把文字放進剪貼簿 (不經過伺服器)
            tags$button(id = "p_copy", class = "btn btn-primary", icon("copy"), " 複製提示詞",
                        onclick = "navigator.clipboard.writeText(document.getElementById('p_ai_prompt').innerText)
                                     .then(() => { const old = this.innerHTML; this.innerHTML = '✓ 已複製'; setTimeout(() => this.innerHTML = old, 2000); });"),
            tags$a(class = "btn btn-outline-secondary", href = "https://gemini.google.com/app", target = "_blank", "開啟 Gemini"),
            tags$a(class = "btn btn-outline-secondary", href = "https://chatgpt.com/", target = "_blank", "開啟 ChatGPT"),
            tags$a(class = "btn btn-outline-secondary", href = "https://claude.ai/new", target = "_blank", "開啟 Claude")),
        div(class = "small-note mt-2", "也可以用 Chrome 右上角的 Gemini 直接貼上。內建 AI 複檢：Coming soon。"),
        # .noWS = "inside"：不要在 <pre> 裡面加排版用的空白/換行 (否則複製出來的提示詞開頭會多一串空白)
        tags$pre(id = "p_ai_prompt", class = "ai-box mt-2 p-3 bg-light border rounded", .noWS = "inside",
                 textOutput("p_ai_prompt_text", inline = TRUE))
      )
    )
  ),

  # 【分頁 4：關於】
  nav_panel(
    "關於", icon = icon("circle-info"),
    card(card_body(
      h4("關於這個專案"),
      p(sprintf("資料期間：%s ~ %s，共 %s 筆交易 (已排除親友、持分等特殊交易)：%s。",
                format(data_months[1], "%Y-%m"), format(data_months[2], "%Y-%m"),
                format(sum(cat_counts), big.mark = ","),
                paste(sprintf("%s %s 筆", names(cat_counts), format(as.integer(cat_counts), big.mark = ",")),
                      collapse = "、"))),
      p("估價模型：XGBoost，以「最近 6 個月的交易」作為測試集 (模型訓練時沒看過)。",
        sprintf("測試集平均誤差 (MAPE) %s，誤差在 ±10%% 內的比例 %s。",
                scales::percent(model$metrics$MAPE[model$metrics$model == "3. XGBoost"], .1),
                scales::percent(model$metrics$hit_10[model$metrics$model == "3. XGBoost"], .1))),
      p("估價結果僅供參考，不構成任何買賣或投資建議。實際價格受屋況、裝潢、面寬、景觀、議價等因素影響。"),
      h5("資料來源"),
      tags$ul(
        tags$li(tags$a("內政部不動產交易實價查詢服務網", href = "https://plvr.land.moi.gov.tw/", target = "_blank")),
        tags$li("道路圖資 © OpenStreetMap contributors (ODbL)"),
        tags$li("鄉鎮市區界線：內政部國土測繪中心")
      )
    ))
  ),
  nav_spacer(),
  nav_item(tags$a(icon("github"), "GitHub", href = "https://github.com/xworking88111-source/taiwan-house-map", target = "_blank"))
)

# --- 6. Server ----------------------------------------------------------------
server <- function(input, output, session) {

  # 縣市改變 → 更新行政區選單 (三個分頁各自一組)
  # pending_district：點地圖跳到「別的縣市」的某區時，要等縣市選單更新完才能選區，先暫存在這裡
  pending_district <- reactiveVal(NULL)
  observeEvent(input$m_city, {
    ch <- if (input$m_city == "全台") "全部" else c("全部", districts_of(input$m_city))
    sel <- if (!is.null(pending_district()) && pending_district() %in% ch) pending_district() else "全部"
    updateSelectInput(session, "m_district", choices = ch, selected = sel)
    pending_district(NULL)
  })
  observeEvent(input$e_city, {
    ch <- if (input$e_city == "全台") "全部" else c("全部", districts_of(input$e_city))
    updateSelectInput(session, "e_district", choices = ch)
  })
  observeEvent(input$p_city, {
    ch <- districts_of(input$p_city)
    # 目前選的區如果還在新清單裡就保留 (App 剛啟動時就是預設的「鳳山區」)，否則選第一個
    sel <- if (isTRUE(input$p_district %in% ch)) input$p_district else ch[1]
    updateSelectInput(session, "p_district", choices = ch, selected = sel)
  })
  # 行政區改變 → 更新道路清單 (server = TRUE：選項很多時只在使用者打字時傳送符合的，網頁才不會卡)
  observeEvent(list(input$p_city, input$p_district), {
    r <- roads$roads$name[roads$roads$city == input$p_city & roads$roads$district == input$p_district]
    updateSelectizeInput(session, "p_road", choices = c("", sort(unique(r))), server = TRUE,
                         selected = if (input$p_district == "鳳山區") "瑞隆東路" else "",
                         options = list(create = TRUE, createOnBlur = TRUE, persist = FALSE,
                                        placeholder = "例如 中山一路100號3樓，或只填 瑞隆東路"))
  })

  # ===== 地圖 =====
  # 【多人同時使用的效能設計】
  # Shiny 是「單執行緒」：一個人在算的時候，其他人要排隊。所以要盡量「少算」：
  #   1. debounce(300)：使用者停止操作 300 毫秒後才計算 (連續點選不會每一下都算)
  #   2. bindCache()  ：把「計算結果」存起來，而且「所有使用者共用」。
  #      第一個人選三民區要算 1~4 秒，之後任何人選三民區都直接拿結果 (幾毫秒)。
  #      只快取「小的結果」(行政區中位數、每棟建物摘要、統計數字)，
  #      不快取篩選出來的原始資料 (全台成屋 73 萬筆，快取會吃光記憶體)。
  map_in <- reactive(list(
    cat = input$m_cat, city = input$m_city, district = input$m_district,
    type = input$m_type, year = input$m_year, age = input$m_age, mode = input$m_mode
  )) %>% debounce(300)

  map_filter <- function(i, district = i$district) filter_trades(i$cat, i$city, district, i$type, i$year, i$age)

  # 數值框 (4 個統計數字)
  map_kpi <- reactive({
    i <- map_in(); d <- map_filter(i)
    list(n = nrow(d),
         unit = median(metric_of(d, i$cat), na.rm = TRUE),
         total = median(d$total_wan, na.rm = TRUE),
         extra = if (i$cat %in% c("成屋", "預售交屋")) paste(fmt_wan(median(d$age, na.rm = TRUE), 0), "年")
                 else if (i$cat == "預售屋") format(n_distinct(d$build_case, na.rm = TRUE), big.mark = ",")
                 else paste(fmt_wan(median(d$ping, na.rm = TRUE)), "坪"))
  }) %>% bindCache(map_in()[c("cat", "city", "district", "type", "year", "age")])

  output$m_n           <- renderText(format(map_kpi()$n, big.mark = ","))
  output$m_unit_title  <- renderText(paste0(metric_name(input$m_cat), "中位數"))
  output$m_unit        <- renderText(paste(fmt_wan(map_kpi()$unit), metric_unit(input$m_cat)))
  output$m_total       <- renderText(paste(fmt_wan(map_kpi()$total, 0), "萬"))
  output$m_extra_title <- renderText({
    if (input$m_cat %in% c("成屋", "預售交屋")) "屋齡中位數"
    else if (input$m_cat == "預售屋") "建案數" else "面積中位數"
  })
  output$m_extra <- renderText(map_kpi()$extra)

  # 實際使用的顯示方式：
  #   - 土地、車位：沒有門牌 → 永遠是面量圖
  #   - 選了某個行政區：自動切到「每棟建物」(第一版要自己切，使用者會以為沒反應)
  map_mode <- reactive({
    i <- map_in()
    if (i$cat %in% c("土地", "車位")) "choropleth"
    else if (i$district != "全部") "points"
    else i$mode
  })

  # 面量圖用的資料：各行政區中位數 (一個縣市最多幾十列，很小 → 適合快取)
  # 選了某區時也畫整個縣市 (才有比較的基準)，所以快取的 key 不含 district
  map_choro <- reactive({
    i <- map_in()
    base <- map_filter(i, district = "全部")
    base$metric <- metric_of(base, i$cat)
    base %>% group_by(city, district) %>%
      summarise(n = n(), med = median(metric, na.rm = TRUE), .groups = "drop") %>%
      filter(n >= 3, !is.na(med)) %>%                    # 樣本太少的區中位數不可靠，不上色
      mutate(across(c(city, district), as.character))
  }) %>% bindCache(map_in()[c("cat", "city", "type", "year", "age")])

  # 每棟建物的摘要：一棟建物 = 一個標記 (同一個 addr_key = 同一個門牌，不含樓層)
  # 預售屋用「建案名稱」合併 → 一個建案一個標記
  building_key <- function(d, cat) if (cat == "預售屋") coalesce(d$build_case, d$address) else d$address
  map_bld <- reactive({
    i <- map_in(); d <- map_filter(i)
    d$metric <- metric_of(d, i$cat)
    d$bkey <- building_key(d, i$cat)
    d <- d[!is.na(d$bkey) & !is.na(d$lng), ]
    b <- d %>% group_by(bkey) %>%
      summarise(n = n(), med = median(metric, na.rm = TRUE),
                lng = first(lng), lat = first(lat), label = first(address), .groups = "drop")
    if (i$cat == "預售屋") b$label <- b$bkey
    b
  }) %>% bindCache(map_in()[c("cat", "city", "district", "type", "year", "age")])

  # 底圖只畫一次；之後用 leafletProxy 只更新圖層 → 切換篩選時地圖不會整個閃一下重畫
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      add_basemap() %>%
      setView(lng = 120.9, lat = 23.7, zoom = 7)
  })

  observe({
    i <- map_in(); mode <- map_mode()
    cat <- i$cat; city <- i$city; dist <- i$district
    unit <- metric_unit(cat)
    proxy <- leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>%
      clearMarkerClusters() %>% clearControls() %>% clearPopups()

    # tryCatch：畫圖時萬一出錯，只跳出提示，不讓整個 Shiny 連線斷掉 (第一版就是這樣斷掉的)
    tryCatch({
      if (mode == "choropleth") {
        agg <- map_choro()
        shp <- towns %>% inner_join(agg, by = c("city", "district"))
        if (city != "全台") shp <- shp[shp$city == city, ]
        if (nrow(shp) == 0) { showNotification("這個條件沒有足夠的資料", type = "warning"); return() }
        pal <- make_pal(shp$med)
        shp$sel <- shp$district == dist
        bb <- st_bbox(if (any(shp$sel)) shp[shp$sel, ] else shp)
        # 全台模式不用 bbox：金門、馬祖會把範圍撐大，台灣本島變得很小
        if (city == "全台") bb <- c(xmin = 119.9, ymin = 21.9, xmax = 122.1, ymax = 25.35)
        proxy %>%
          addPolygons(data = shp, layerId = ~paste(city, district, sep = "|"),
                      fillColor = ~pal(med), fillOpacity = 0.75,
                      color = ~ifelse(sel, "#0F172A", "#FFFFFF"), weight = ~ifelse(sel, 4, 1),
                      highlightOptions = highlightOptions(weight = 3, color = "#0F172A", bringToFront = TRUE),
                      label = ~lapply(sprintf("<b>%s%s</b><br>%s中位數 %.1f %s<br>成交 %s 筆",
                                              city, ifelse(district == city, "", district), metric_name(cat),
                                              med, unit, format(n, big.mark = ",")), HTML)) %>%
          addLegend("bottomright", pal = pal, values = shp$med, title = paste0(metric_name(cat), " (", unit, ")"),
                    labFormat = labelFormat(digits = 1, between = " ~ ")) %>%
          fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]], options = list(animate = FALSE))
      } else {
        if (city == "全台") { showNotification("「每棟建物」模式請先選擇縣市", type = "warning"); return() }
        b <- map_bld()
        if (nrow(b) == 0) { showNotification("這個條件沒有可以標在地圖上的建物", type = "warning"); return() }
        if (nrow(b) > 20000) {                             # 再多瀏覽器會卡
          b <- b %>% arrange(desc(n)) %>% head(20000)
          showNotification("建物超過 2 萬棟，只顯示交易最多的 2 萬棟。可以再選擇行政區縮小範圍。", type = "message")
        }
        pal <- make_pal(b$med)
        # 效能重點：標記上「不放」彈出視窗的 HTML。6000 棟建物 × 每棟一段 HTML = 好幾 MB 要傳給瀏覽器，
        # 會讓地圖卡好幾秒。改成 layerId = 建物代號，使用者點了哪一棟，伺服器才產生那一棟的明細。
        proxy %>%
          addCircleMarkers(
            data = b, lng = ~lng, lat = ~lat, layerId = ~bkey,
            radius = ~pmin(4 + sqrt(n) * 1.5, 16),           # 交易越多的建物，圓點越大
            stroke = TRUE, color = "#FFFFFF", weight = 1, fillColor = ~pal(med), fillOpacity = 0.9,
            clusterOptions = markerClusterOptions(disableClusteringAtZoom = 17, spiderfyOnMaxZoom = TRUE),
            label = ~sprintf("%s｜%d 筆｜%.1f %s", label, n, med, unit)
          ) %>%
          addLegend("bottomright", pal = pal, values = b$med, title = paste0(metric_name(cat), " (", unit, ")"),
                    labFormat = labelFormat(digits = 1, between = " ~ ")) %>%
          fitBounds(min(b$lng), min(b$lat), max(b$lng), max(b$lat), options = list(animate = FALSE))
      }
    }, error = function(e) showNotification(paste("地圖繪製失敗：", conditionMessage(e)), type = "error"))
  })

  # 點選某棟建物 → 這時才產生它的交易明細 (彈出視窗)
  geo_txt <- c(lane2 = "巷弄", lane = "巷弄", road_sec = "路段", road = "路段",
               district = "行政區中心", city = "縣市中心")
  observeEvent(input$map_marker_click, {
    clk <- input$map_marker_click
    if (is.null(clk$id)) return()
    i <- map_in(); cat <- i$cat; unit <- metric_unit(cat)
    d <- map_filter(i)
    x <- d[which(building_key(d, cat) == clk$id), ]
    if (nrow(x) == 0) return()
    x$metric <- metric_of(x, cat)
    x <- x[order(x$trans_ym, decreasing = TRUE), ]
    rows <- head(x, 8)
    html <- sprintf(paste0(
      "<div style='font-size:13px;line-height:1.7;min-width:260px'><b>%s</b><br>",
      "<span style='color:#64748B'>%s｜位置精準到%s</span><hr style='margin:4px 0'>",
      "共 <b>%d</b> 筆交易%s｜中位數 <b style='color:#B91C1C'>%.1f %s</b>｜總價中位數 %s 萬",
      "<hr style='margin:4px 0'><small>最近交易：<br>%s</small></div>"),
      if (cat == "預售屋") paste0(clk$id, "<br><small>", x$address[1], "</small>") else x$address[1],
      paste(unique(as.character(x$b_category)), collapse = "、"), unname(geo_txt[as.character(x$geo_level[1])]),
      nrow(x), if (sum(x$cancelled, na.rm = TRUE) > 0) sprintf("（解約 %d 筆）", sum(x$cancelled, na.rm = TRUE)) else "",
      median(x$metric, na.rm = TRUE), unit, fmt_wan(median(x$total_wan, na.rm = TRUE), 0),
      paste(sprintf("%s｜%s｜%s坪｜%s 萬｜%.1f %s%s", format(rows$trans_ym, "%Y-%m"), rows$b_category,
                    fmt_wan(rows$ping), fmt_wan(rows$total_wan, 0), rows$metric, unit,
                    ifelse(is.na(rows$age), "", paste0("｜屋齡 ", rows$age, " 年"))), collapse = "<br>"))
    leafletProxy("map") %>% clearPopups() %>% addPopups(clk$lng, clk$lat, html)
  })

  # 點選行政區 → 選單跳到該區 (選了區之後 map_mode 會自動切成「每棟建物」)
  observeEvent(input$map_shape_click, {
    parts <- strsplit(input$map_shape_click$id, "\\|")[[1]]
    if (parts[2] == parts[1]) return()                   # 新竹市/嘉義市「整個市」的多邊形，沒有區可以選
    if (input$m_city != parts[1]) {
      pending_district(parts[2])               # 縣市選單更新時會讀這個值 (見上面的 observeEvent)
      updateSelectInput(session, "m_city", selected = parts[1])
    } else {
      updateSelectInput(session, "m_district", selected = parts[2])
    }
  })

  # ===== 市場探勘 =====
  # ⚠️ debounce 和 bindCache 一起用時，「快取的鑰匙」和「計算用的資料」必須來自同一個 debounce 過的值。
  #    第一版鑰匙用即時的 input$e_cat、資料用 debounce 過的 ex_data()：切換類別的瞬間，
  #    鑰匙已經是「土地」、資料還是「成屋」→ 把成屋的圖存成土地的快取，之後所有人選土地都看到錯的圖。
  ex_in <- reactive(list(cat = input$e_cat, city = input$e_city, district = input$e_district,
                         type = input$e_type, year = input$e_year)) %>% debounce(300)
  ex_data <- reactive({ i <- ex_in(); filter_trades(i$cat, i$city, i$district, i$type, i$year) })
  plot_layout <- function(p, ...) {
    p %>% layout(font = list(family = "Noto Sans TC"), margin = list(t = 30), hovermode = "x unified",
                 legend = list(orientation = "h", x = 0, y = 1.12, yanchor = "bottom"), ...) %>%
      config(displaylogo = FALSE)
  }
  # 圖表依「建物型態」分色；土地、車位只有一種，就用自己的顏色
  ex_colors <- function(d) type_colors[intersect(names(type_colors), unique(as.character(d$b_category)))]

  output$e_trend_title <- renderText(paste0("每月", metric_name(input$e_cat), "中位數 (", metric_unit(input$e_cat), ")"))
  # 趨勢圖的資料：每月中位數。
  # 某型態某個月交易太少 (< 10 筆) 時，中位數會被一兩筆特殊交易拉著跑 → 那個月不畫 (線會斷開，不硬連)；
  # 整體有效月份太少 (< 6 個月) 的型態整條不畫，並在圖下方註明。
  trend_data <- function(i) {
    d <- filter_trades(i$cat, i$city, i$district, i$type, i$year)
    d$metric <- metric_of(d, i$cat)
    m <- d %>% group_by(b_category, trans_ym) %>%
      summarise(med = median(metric, na.rm = TRUE), n = n(), .groups = "drop") %>%
      mutate(med = ifelse(n >= 10, med, NA), b_category = as.character(b_category))
    ok <- m %>% group_by(b_category) %>% summarise(k = sum(!is.na(med)), .groups = "drop")
    keep <- ok$b_category[ok$k >= 6]
    months <- sort(unique(d$trans_ym))
    grid <- expand.grid(b_category = keep, trans_ym = months, stringsAsFactors = FALSE)
    list(data = grid %>% left_join(m, by = c("b_category", "trans_ym")) %>% arrange(b_category, trans_ym),
         dropped = setdiff(unique(as.character(d$b_category)), keep))
  }
  output$e_trend <- renderPlotly({
    t <- trend_data(ex_in()); d <- t$data
    validate(need(nrow(d) > 0, "資料不足：每月交易都少於 10 筆，請放寬縣市或年份"))
    d$b_category <- factor(d$b_category, levels = intersect(names(type_colors), unique(d$b_category)))
    plot_ly(d, x = ~trans_ym, y = ~med, color = ~b_category, colors = type_colors[levels(d$b_category)],
            type = "scatter", mode = "lines", line = list(width = 2.5), connectgaps = FALSE,
            text = ~n, hovertemplate = paste0("%{y:.1f} ", metric_unit(ex_in()$cat), "（%{text} 筆）")) %>%
      plot_layout(xaxis = list(title = ""), yaxis = list(title = metric_unit(ex_in()$cat)))
  }) %>%
    bindCache(ex_in())   # 同樣的條件，所有使用者共用同一張圖
  output$e_trend_note <- renderText({
    t <- trend_data(ex_in())
    paste0("中位數：一半的交易比它貴、一半比它便宜，比平均數不容易被極端值影響。當月少於 10 筆交易的月份不畫。",
           if (length(t$dropped)) paste0("「", paste(t$dropped, collapse = "、"), "」交易太少，未畫出。") else "")
  }) %>% bindCache(ex_in())

  output$e_volume <- renderPlotly({
    d <- ex_data() %>% count(b_category, trans_ym)
    validate(need(nrow(d) > 0, "資料不足"))
    plot_ly(d, x = ~trans_ym, y = ~n, color = ~b_category, colors = ex_colors(d), type = "bar") %>%
      plot_layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = "筆數"))
  }) %>%
    bindCache(ex_in())   # 同樣的條件，所有使用者共用同一張圖

  # 各類交易占比：不管上面選哪個類別，都看「同地區、同期間」所有類別的筆數
  output$e_share <- renderPlotly({
    # 每一類各自篩選再計數 (不把 5 類接成一張 168 萬筆的大表，避免瞬間多吃幾百 MB 記憶體)
    d <- tibble(category = unname(categories),
                n = sapply(unname(categories), function(cc)
                  nrow(filter_trades(cc, ex_in()$city, ex_in()$district, types, ex_in()$year)))) %>%
      filter(n > 0)
    validate(need(nrow(d) > 0, "資料不足"))
    plot_ly(d, labels = ~category, values = ~n, type = "pie", hole = 0.55,
            marker = list(colors = unname(cat_colors[as.character(d$category)])),
            texttemplate = "%{label}<br>%{percent:.1%}", sort = FALSE) %>%   # 百分比只顯示 1 位小數
      plot_layout(showlegend = FALSE, hovermode = FALSE)
  }) %>%
    bindCache(ex_in())   # 同樣的條件，所有使用者共用同一張圖

  output$e_hist_title <- renderText(paste0(metric_name(input$e_cat), "分布"))
  output$e_hist <- renderPlotly({
    d <- ex_data(); d$metric <- metric_of(d, ex_in()$cat)
    d <- d[!is.na(d$metric), ]
    validate(need(nrow(d) > 0, "資料不足"))
    cap <- quantile(d$metric, 0.99)               # 去掉最貴的 1%，不然 x 軸被極端值拉很長
    d <- d[d$metric <= cap, ]
    # 數量多的型態先畫、少的後畫 (疊在上面)，小型態才不會被大型態整個蓋住
    ord <- names(sort(table(as.character(d$b_category)), decreasing = TRUE))
    d$b_category <- factor(as.character(d$b_category), levels = ord)
    plot_ly(d, x = ~metric, color = ~b_category, colors = type_colors[ord],
            type = "histogram", nbinsx = 50, opacity = 0.6) %>%
      plot_layout(barmode = "overlay", xaxis = list(title = metric_unit(ex_in()$cat)),
                  yaxis = list(title = "筆數"), hovermode = "closest")
  }) %>%
    bindCache(ex_in())   # 同樣的條件，所有使用者共用同一張圖

  output$e_rank_title <- renderText({
    paste0(if (input$e_city == "全台") "各縣市" else paste0(input$e_city, "各行政區"),
           metric_name(input$e_cat), "中位數")
  })
  output$e_rank <- renderPlotly({
    d <- filter_trades(ex_in()$cat, ex_in()$city, "全部", ex_in()$type, ex_in()$year)
    d$metric <- metric_of(d, ex_in()$cat)
    grp <- if (ex_in()$city == "全台") "city" else "district"
    d <- d %>% group_by(name = as.character(.data[[grp]])) %>%
      summarise(med = median(metric, na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(n >= 10, !is.na(med)) %>% arrange(desc(med)) %>% head(15)   # 15 個以內，標籤才不會擠在一起
    validate(need(nrow(d) > 0, "資料不足"))
    hl <- if (ex_in()$district != "全部") ex_in()$district else ""
    plot_ly(d, y = ~reorder(name, med), x = ~med, type = "bar", orientation = "h",
            marker = list(color = ifelse(d$name == hl, "#B91C1C", "#0F766E")),
            text = ~sprintf("%.1f", med), textposition = "outside",
            hovertemplate = paste0("%{y}：%{x:.1f} ", metric_unit(ex_in()$cat), "<extra></extra>")) %>%
      plot_layout(xaxis = list(title = metric_unit(ex_in()$cat)), yaxis = list(title = ""), hovermode = "closest")
  }) %>%
    bindCache(ex_in())   # 同樣的條件，所有使用者共用同一張圖

  # 第六張圖：建物類看「屋齡 vs 單價」；預售屋看「建案排行」；土地、車位看「面積 vs 價格」
  output$e_age_title <- renderText(switch(input$e_cat,
    "預售屋" = "成交最多的建案 (單價中位數)", "土地" = "土地面積與單價", "車位" = "車位面積與價格", "不同屋齡的單價"))
  output$e_age <- renderPlotly({
    d <- ex_data(); d$metric <- metric_of(d, ex_in()$cat)
    validate(need(nrow(d) > 0, "資料不足"))
    if (ex_in()$cat == "預售屋") {
      b <- d %>% filter(!is.na(build_case)) %>% group_by(build_case) %>%
        summarise(n = n(), med = median(metric, na.rm = TRUE), cancel = sum(cancelled), .groups = "drop") %>%
        arrange(desc(n)) %>% head(15)
      validate(need(nrow(b) > 0, "資料不足"))
      plot_ly(b, y = ~reorder(build_case, n), x = ~n, type = "bar", orientation = "h",
              marker = list(color = "#2563EB"),
              text = ~sprintf("%.1f 萬/坪", med), textposition = "outside",
              hovertemplate = "%{y}<br>%{x} 筆｜解約 %{customdata} 筆<extra></extra>", customdata = ~cancel) %>%
        plot_layout(xaxis = list(title = "成交筆數"), yaxis = list(title = ""), hovermode = "closest")
    } else if (ex_in()$cat %in% c("土地", "車位")) {
      s <- d[!is.na(d$ping) & !is.na(d$metric), ]
      s <- s[s$metric <= quantile(s$metric, .99) & s$ping <= quantile(s$ping, .99), ]
      if (nrow(s) > 5000) s <- s[sample(nrow(s), 5000), ]   # 散布圖點太多會卡，隨機抽 5000 點
      plot_ly(s, x = ~ping, y = ~metric, type = "scattergl", mode = "markers",
              marker = list(size = 5, opacity = 0.4, color = unname(cat_colors[ex_in()$cat]))) %>%
        plot_layout(xaxis = list(title = "面積 (坪)"), yaxis = list(title = metric_unit(ex_in()$cat)),
                    hovermode = "closest")
    } else {
      # 第一版用箱型圖，一般人看不懂 q1、q3。改成「點 + 範圍線」：
      #   點 = 中位數；線 = 中間 50% 交易的範圍 (第 25 ~ 75 百分位)，滑鼠移上去用白話說明
      a <- d %>% filter(!is.na(age), !is.na(metric)) %>%
        mutate(age_grp = cut(age, c(-1, 5, 10, 20, 30, 40, 100),
                             labels = c("0-5 年", "6-10 年", "11-20 年", "21-30 年", "31-40 年", "40 年以上")),
               b_category = as.character(b_category)) %>%
        group_by(b_category, age_grp) %>%
        summarise(n = n(), med = median(metric), lo = quantile(metric, .25), hi = quantile(metric, .75),
                  .groups = "drop") %>%
        filter(n >= 10)                                   # 少於 10 筆的組合不畫，避免誤導
      validate(need(nrow(a) > 0, "資料不足"))
      a$b_category <- factor(a$b_category, levels = intersect(names(type_colors), unique(a$b_category)))
      a$tip <- sprintf("<b>屋齡 %s 的%s</b><br>單價中位數 <b>%.1f 萬/坪</b><br>一半的交易在 %.1f ~ %.1f 萬/坪 之間<br>共 %s 筆",
                       a$age_grp, a$b_category, a$med, a$lo, a$hi, format(a$n, big.mark = ","))
      plot_ly(a, x = ~age_grp, y = ~med, color = ~b_category, colors = type_colors[levels(a$b_category)],
              type = "scatter", mode = "markers", marker = list(size = 11),
              error_y = ~list(type = "data", symmetric = FALSE, array = hi - med, arrayminus = med - lo,
                              thickness = 2.5, width = 0),
              text = ~tip, hovertemplate = "%{text}<extra></extra>") %>%
        plot_layout(scattermode = "group", scattergap = 0.5,
                    xaxis = list(title = "屋齡"), yaxis = list(title = "單價 (萬/坪)"), hovermode = "closest")
    }
  }) %>%
    bindCache(ex_in())   # 同樣的條件，所有使用者共用同一張圖
  output$e_age_note <- renderText(switch(input$e_cat,
    "預售屋" = "長條長度 = 成交筆數，旁邊的數字是單價中位數；滑鼠移上去可以看解約筆數。",
    "土地" = , "車位" = "每個點是一筆交易 (最多隨機顯示 5,000 筆)。",
    "點 = 單價中位數；上下的線 = 中間一半交易的價格範圍。通常屋齡越高單價越低，但老屋若有都更題材可能反而變貴。"))

  # ===== AI 估價 =====
  # 切換建物型態：透天厝預設 30 坪土地；其他型態清空 (多數人不知道持分 → 用推估值)
  observeEvent(input$p_type, {
    updateNumericInput(session, "p_land", value = if (input$p_type == "透天厝") 30 else NA)
  }, ignoreInit = TRUE)
  # eventReactive：只有按下「開始估價」才計算 (不是每改一個輸入就重算)
  est <- eventReactive(input$p_go, {
    loc <- locate_address(input$p_city, input$p_district, input$p_road)
    is_house <- input$p_type == "透天厝"
    # 土地持分：有填就用；沒填就用同區同型態的中位數比例推估 (見 impute_land_ratio 的說明)
    land_ratio <- impute_land_ratio(input$p_city, input$p_district, input$p_type)
    land_given <- !is.na(input$p_land) && input$p_land > 0
    inp <- list(
      city = input$p_city, district = input$p_district, road = input$p_road %||% "",
      type = input$p_type, age = input$p_age, ping = input$p_ping,
      land = if (land_given) input$p_land else land_ratio * input$p_ping,
      land_imputed = !land_given,
      floor = if (is_house) NA_real_ else input$p_floor,
      total_floors = if (is_house) NA_real_ else input$p_tfloor,
      rooms = input$p_rooms, baths = input$p_baths,
      parking = input$p_parking, elevator = input$p_elevator, management = input$p_mgmt,
      lng = loc$lng, lat = loc$lat
    )
    validate(need(!is.na(inp$age) && !is.na(inp$ping) && inp$ping > 0, "請輸入屋齡與坪數"))
    xy <- lnglat_to_xy(inp$lng, inp$lat)
    inp$geo <- facility_features(xy, facilities)                    # 21 個設施距離 / 數量
    inp$nbr <- lookup_nbr(nbr_tbl, xy[1], xy[2], inp$type)           # 附近約 500 公尺、近 12 個月的同型態行情
    pred  <- predict_price(inp)
    comps <- find_comparables(inp)
    list(inp = inp, pred = pred, comps = comps, loc = loc)
  })

  # 依屋況、特殊情況調整後的價格：est() (模型) 只在按按鈕時算，屋況改變時只重算這裡 → 即時
  adj_pred <- reactive({
    e <- est()
    ta <- total_adj(input$p_cond %||% "一般", input$p_special, e$inp$type)
    list(point = e$pred$point * ta$factor, p10 = e$pred$p10 * ta$factor, p90 = e$pred$p90 * ta$factor,
         factor = ta$factor, parts = ta$parts, base = e$pred$point)
  })
  adj_label <- function(a) {
    if (abs(a$factor - 1) < 1e-9) return("")
    sprintf("<br><span style='font-size:.8rem'>一般屋況 %.1f，依你的條件 %+.1f%%</span>", a$base / 1e4, (a$factor - 1) * 100)
  }
  output$p_unit <- renderUI({
    a <- adj_pred(); HTML(sprintf("%.1f <small>萬/坪</small>%s", a$point / 1e4, adj_label(a)))
  })
  output$p_total <- renderUI({
    e <- est(); a <- adj_pred(); HTML(sprintf("%s <small>萬</small>", fmt_wan(a$point * e$inp$ping / 1e4, 0)))
  })
  output$p_range <- renderUI({
    e <- est(); a <- adj_pred()
    HTML(sprintf(paste0("<span style='font-size:1.5rem'>%.1f ~ %.1f <small>萬/坪</small></span><br>",
                        "<span style='font-size:.85rem'>總價 %s ~ %s 萬</span>"),
                 a$p10 / 1e4, a$p90 / 1e4,
                 fmt_wan(a$p10 * e$inp$ping / 1e4, 0), fmt_wan(a$p90 * e$inp$ping / 1e4, 0)))
  })

  # 不同屋況的估價並排比較 (= 「價值區間」的概念：同一間房子，屋況不同，價值不同)
  output$p_cond_plot <- renderPlotly({
    e <- est(); a <- adj_pred()
    sp <- input$p_special
    # 圖上用短名稱 (手機螢幕窄，長名稱會把長條擠扁)；完整說明在左側的選項
    short <- c("屋況不佳" = "需大整修", "一般" = "一般屋況", "含裝潢" = "有裝潢")
    d <- tibble(label = unname(short[cond_levels]), cond = unname(cond_levels)) %>%
      mutate(f = sapply(cond, function(cc) total_adj(cc, sp, e$inp$type)$factor),
             unit = e$pred$point * f / 1e4, total = unit * e$inp$ping,
             sel = cond == (input$p_cond %||% "一般"),
             label = factor(label, levels = rev(unname(short[cond_levels]))))
    plot_ly(d, y = ~label, x = ~total, type = "bar", orientation = "h",
            marker = list(color = ifelse(d$sel, "#0F766E", "#CBD5E1")),
            text = ~sprintf("%s 萬 (%.1f 萬/坪)", fmt_wan(total, 0), unit), textposition = "auto",
            hovertemplate = "%{y}<br>總價約 %{x:,.0f} 萬<extra></extra>") %>%
      plot_layout(xaxis = list(title = "預估總價 (萬)"), yaxis = list(title = ""), hovermode = "closest",
                  margin = list(l = 10, r = 10, t = 10, b = 40))
  })
  output$p_cond_note <- renderUI({
    e <- est(); a <- adj_pred()
    pieces <- lapply(names(a$parts), function(k) {
      x <- a$parts[[k]]
      if (k == "一般" || isTRUE(x$insufficient)) return(NULL)
      sprintf("%s %+.1f%%（依據 %s 筆備註有寫的實際成交%s）", k, x$adj * 100, format(x$n, big.mark = ","),
              if (isTRUE(x$pooled)) "，此型態樣本少，採各型態合併結果" else "")
    })
    pieces <- unlist(pieces)
    warn <- names(a$parts)[sapply(a$parts, function(x) isTRUE(x$insufficient))]
    tagList(
      if (length(pieces)) tags$p(tags$b("目前的調整："), paste(pieces, collapse = "；")),
      if (length(warn)) tags$p(style = "color:#B91C1C",
        sprintf("⚠️ %s：實價登錄中明確記載的這類交易太少 (不到 30 筆)，無法可靠估計影響，估價沒有調整。這類物件價格通常明顯較低，請務必另行請專業估價。",
                paste(warn, collapse = "、"))),
      tags$p("說明：模型的估價代表「一般屋況」。各條件的影響，是比較實價登錄備註寫了「含裝潢」「屋況需整理」",
             "「凶宅」等字樣的成交，實際價格平均比模型預測高或低多少。",
             "備註由賣方或地政士填寫，沒寫不代表沒有，所以影響幅度偏保守，僅供參考。")
    )
  })

  output$p_plot <- renderPlotly({
    e <- est(); c <- e$comps; a <- adj_pred()
    e$pred <- list(point = a$point, p10 = a$p10, p90 = a$p90)   # 圖上的星號 = 依屋況調整後的估價
    p <- plot_ly()
    if (nrow(c) > 0) {
      p <- p %>% add_markers(data = c, x = ~ping, y = ~unit_wan, name = "相似成交",
                             marker = list(size = 11, color = "#64748B", opacity = 0.8),
                             text = ~sprintf("%s<br>%s｜屋齡 %d 年", address, format(trans_ym, "%Y-%m"),
                                             as.integer(age)),
                             hovertemplate = "%{text}<br>%{x:.1f} 坪｜%{y:.1f} 萬/坪<extra></extra>")
    }
    p %>%
      add_markers(x = e$inp$ping, y = e$pred$point / 1e4, name = "本物件預估",
                  marker = list(size = 18, color = "#B91C1C", symbol = "star"),
                  error_y = list(type = "data", symmetric = FALSE, color = "#B91C1C",
                                 array = (e$pred$p90 - e$pred$point) / 1e4,
                                 arrayminus = (e$pred$point - e$pred$p10) / 1e4),
                  hovertemplate = "預估 %{y:.1f} 萬/坪<extra></extra>") %>%
      plot_layout(xaxis = list(title = "建物坪數"), yaxis = list(title = "單價 (萬/坪)"), hovermode = "closest")
  })

  output$p_map <- renderLeaflet({
    e <- est(); c <- e$comps
    m <- leaflet() %>% add_basemap() %>%
      addAwesomeMarkers(lng = e$inp$lng, lat = e$inp$lat, label = "本物件 (估計位置)",
                        icon = awesomeIcons(icon = "star", markerColor = "red", library = "fa"))
    if (nrow(c) > 0) {
      m <- m %>% addCircleMarkers(data = c, lng = ~lng, lat = ~lat, radius = 7, color = "#0F766E",
                                  label = ~sprintf("%.1f 萬/坪｜%s", unit_wan, format(trans_ym, "%Y-%m")))
    }
    m
  })

  output$p_basis <- renderUI({
    e <- est(); l <- e$loc
    tags$ul(class = "basis-list",
      tags$li(tags$b("定位："), l$label, "（精準到", l$level, "）"),
      tags$li(tags$b("土地持分："), sprintf("%.1f 坪", e$inp$land),
              if (isTRUE(e$inp$land_imputed)) sprintf("（沒有填寫，用%s%s%s的中位數比例推估）",
                                                      e$inp$city, e$inp$district, e$inp$type) else "（使用者填寫）"),
      with(describe_surroundings(e$inp$geo, e$inp$nbr, e$inp$type), tagList(
        tags$li(tags$b("交通："), transit),
        tags$li(tags$b("生活機能："), life),
        tags$li(tags$b("嫌惡設施 (300 公尺內)："),
                if (grepl("沒有", nimby)) nimby else tags$span(style = "color:#B91C1C", "⚠️ ", nimby)),
        tags$li(tags$b("附近行情："), nbr))),
      tags$li(tags$b("模型："), "XGBoost，以實價登錄 5 年成屋交易訓練；測試誤差約 ",
              scales::percent(model$metrics$MAPE[model$metrics$model == "3. XGBoost"], 1),
              "。預測的是「現在」的行情 (資料中最新月份)。")
    )
  })
  output$p_history_card <- renderUI({
    h <- est()$loc$history
    if (is.null(h) || nrow(h) == 0) return(NULL)
    card(card_header(tags$span(icon("building"), sprintf(" 這棟建物的歷史成交 (%d 筆，單價中位數 %.1f 萬/坪)",
                                                         nrow(h), median(h$unit_wan, na.rm = TRUE)))),
         div(class = "comps-table", renderTable(data.frame(
           交易年月 = format(h$trans_ym, "%Y-%m"), 類別 = as.character(h$category),
           樓層 = ifelse(is.na(h$floor_num), "—", paste0(h$floor_num, "/", h$total_floors_num, "F")),
           坪數 = sprintf("%.1f", h$ping), 屋齡 = ifelse(is.na(h$age), "—", h$age),
           `單價(萬/坪)` = sprintf("%.1f", h$unit_wan), `總價(萬)` = fmt_wan(h$total_wan, 0), check.names = FALSE
         ), striped = TRUE, hover = TRUE, width = "100%")))
  })

  output$p_comps <- renderTable({
    c <- est()$comps
    validate(need(nrow(c) > 0, "附近沒有找到條件相近的成交"))
    data.frame(
      交易年月 = format(c$trans_ym, "%Y-%m"), 地址 = c$address,
      坪數 = sprintf("%.1f", c$ping), 屋齡 = as.integer(c$age),
      `單價(萬/坪)` = sprintf("%.1f", c$unit_wan), `總價(萬)` = fmt_wan(c$total_wan, 0),
      距離 = paste0(round(c$dist_m / 10) * 10, " m"), check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, spacing = "s", width = "100%")

  # AI 複檢提示詞：每次估價後自動產生
  output$p_ai_prompt_text <- renderText({
    e <- est(); a <- adj_pred()
    inp <- e$inp
    inp$cond_txt <- paste0(names(cond_levels)[cond_levels == (input$p_cond %||% "一般")],
                           if (length(input$p_special)) paste0("；", paste(names(special_levels)[special_levels %in% input$p_special], collapse = "、")) else "",
                           sprintf("（模型估一般屋況 %.1f 萬/坪，依這些條件調整 %+.1f%%）", a$base / 1e4, (a$factor - 1) * 100))
    build_ai_prompt(inp, list(point = a$point, p10 = a$p10, p90 = a$p90), e$comps)
  })
}

shinyApp(ui, server)
