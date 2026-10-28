# ==============================================================================
# 實驗 04：附近地價 (這一帶的「土地」值多少錢？)
#
# 直覺：房價 = 土地 + 建物。建物會折舊，土地不會；尤其透天厝，價格大部分是土地。
#       目前模型的「附近行情」(nbr_lp) 是房屋單價，混著屋齡和建物好壞，不是純地價。
#
# 特徵 (全部只用「之前」的成交，測試期最多看到 cut_month，防資料洩漏)：
#   tland_lp / tland_n  附近 (1.5 公里見方) 過去 24 個月「透天厝總價 ÷ 土地坪數」的 log 平均、筆數
#                       → 透天的地坪單價最接近「這一帶的地價」；大樓、華廈也能參考
#   land_lp / land_n    同鄉鎮區過去 24 個月「純土地交易 (住宅區)」單價的 log 平均、筆數
#                       → 純土地交易沒有門牌 (只有地號)，沒辦法定位，只能用到「區」這一層
#
# 技巧：和 nbr_* 一樣用「累計加總 + rolling join」→ 任意時間窗的平均都只要加減法，幾秒算完
# 比較對象：實驗 03 (01A 設定 + 同棟歷史 + 臨路等級)，同種子，唯一差別是多了上面 4 個特徵
# ==============================================================================
source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "new_model_experiments.R"))
library(data.table)
t0 <- Sys.time()
# 臨路等級：用正式版的函數 (R/model_lib.R 的 add_road_features；實驗 03 驗證後整合進正式模型的版本)
# (舊版這裡用 eval(parse()) 執行實驗 03 程式的片段，重構時改掉：實驗 03 的標題一改，這裡就會壞)
df <- add_road_features(df, build_road_tables())

# --- 通用工具：「某個區域 key、某段時間窗」的 log 平均 ------------------------------
# src_*：用來算行情的成交 (key, 月份, log 值)；q_*：要查詢的列 (key, 查詢月份, 列編號 id)
# 時間窗 = [q_m - window, min(q_m - 1, cap_month)]；同一個 id 有多個 key (例如周圍 9 格) 時會自動加總
window_mean <- function(src_key, src_m, src_lp, q_id, q_key, q_m, cap_month, window, min_n = 3) {
  agg <- data.table(k = src_key, m = src_m, lp = src_lp)[m <= cap_month,
           .(s = sum(lp), n = .N), by = .(k, m)][order(k, m)]
  agg[, `:=`(cs = cumsum(s), cn = cumsum(n)), by = k]
  q  <- data.table(id = q_id, k = q_key, hi = pmin(q_m - 1, cap_month), lo = q_m - window - 1)
  hi <- agg[q, on = .(k, m = hi), roll = TRUE, .(id, cs, cn)]
  lo <- agg[q, on = .(k, m = lo), roll = TRUE, .(cs, cn)]
  w  <- data.table(id = hi$id, s = fcoalesce(hi$cs, 0) - fcoalesce(lo$cs, 0),
                   n = fcoalesce(hi$cn, 0L) - fcoalesce(lo$cn, 0L))[, .(s = sum(s), n = sum(n)), by = id][order(id)]
  data.table(lp = ifelse(w$n >= min_n, w$s / w$n, NA_real_), n = w$n)
}

# --- 1. 附近透天的地坪單價 (500 公尺格子 × 周圍 9 格 ≈ 1.5 公里見方) ------------------
cs <- 500
th <- df %>% filter(b_category == "透天厝", land_ping >= 5)
q9 <- data.table(id = seq_len(nrow(df)), cx = floor(df$tx / cs), cy = floor(df$ty / cs), m = df$t_month_idx)[
  rep(seq_len(nrow(df)), each = 9)][, `:=`(cx = cx + rep(-1:1, each = 3), cy = cy + rep(-1:1, times = 3))]
r <- window_mean(paste(floor(th$tx / cs), floor(th$ty / cs)), th$t_month_idx,
                 log(th$pure_house_total_price / th$land_ping),
                 q9$id, paste(q9$cx, q9$cy), q9$m, cut_month, window = 24)
df$tland_lp <- r$lp; df$tland_n <- r$n

# --- 2. 同區純土地交易 (住宅區) 單價 -----------------------------------------------
con <- db_connect()
land <- dbGetQuery(con, "
  SELECT city, district, trans_ym, unit_price_ping, land_ping FROM re_v2.fact_land_only
  WHERE zoning IN ('住', '都市：其他:住宅區') AND unit_price_ping > 0 AND land_ping BETWEEN 10 AND 3000
    AND trans_ym IS NOT NULL") %>% as_tibble()
dbDisconnect(con)
land <- land %>%
  mutate(m = (as.integer(format(trans_ym, "%Y")) - 2020) * 12 + as.integer(format(trans_ym, "%m")),
         lp = log(unit_price_ping)) %>%
  group_by(city) %>% filter(between(lp, quantile(lp, .02), quantile(lp, .98))) %>% ungroup()  # 去掉持分、畸零地等怪交易
r <- window_mean(paste0(land$city, land$district), land$m, land$lp,
                 seq_len(nrow(df)), paste0(df$city, df$district), df$t_month_idx, cut_month, window = 24)
df$land_lp <- r$lp; df$land_n <- r$n
message("純土地 (住宅區) ", nrow(land), " 筆；地價特徵計算 ", round(as.numeric(Sys.time() - t0, units = "secs")), " 秒；",
        "有值比例：tland ", scales::percent(mean(!is.na(df$tland_lp)), .1), "、land ", scales::percent(mean(!is.na(df$land_lp)), .1))
message("相關係數 (和自己的 log 單價)：tland ", round(cor(df$tland_lp, df$y, use = "complete.obs"), 3),
        "、land ", round(cor(df$land_lp, df$y, use = "complete.obs"), 3))

# --- 3. 回測 -----------------------------------------------------------------------
df <- bind_cols(df, building_history(df$addr_key, df$t_month_idx, df$y, cap_month = cut_month))
sp <- split_data(df)
feats03 <- c(features_base, "bld_lp", "bld_n", "bld_dt", "lane_depth", "road_type", "road_class", "d_major_road")
feats04 <- c(feats03, "tland_lp", "tland_n", "land_lp", "land_n")
p03 <- predict_xgb(fit_xgb(sp, feats = feats03, seed = 1), sp$test, feats = feats03)
m04 <- fit_xgb(sp, feats = feats04, seed = 1)
p04 <- predict_xgb(m04, sp$test, feats = feats04)
log_experiment("04 基準(=03) 1×1500", "01A + 同棟歷史 + 臨路等級，重跑當基準", score(p03, sp$test))
log_experiment("04 附近地價 1×1500", "03 + tland_lp/tland_n (附近透天地坪價) + land_lp/land_n (同區住宅區土地)",
               bind_cols(score(p04, sp$test), tibble(模型大小MB = model_size_mb(m04))))
print(xgb.importance(model = m04) %>% filter(Feature %in% setdiff(feats04, feats03)))
saveRDS(list(p03 = p03, p04 = p04), "models/exp04_preds.rds")
