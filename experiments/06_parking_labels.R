# ==============================================================================
# 實驗 06：「車位沒拆開」的成交要怎麼處理？
#
# 背景 (學習筆記第 18 章)：大樓 45%、華廈 34% 的成交含車位，但車位的價格或面積沒有完整拆開。
#   內政部規則：這種成交的單價 = 總價 ÷ 建物移轉面積 (車位的價錢和面積都混在裡面)
#   → 和「拆開的」同條件成交比，單價偏低 13~24%。訓練資料的「答案」本身就混了車位。
#
# 網站估價時使用者填的是「不含車位」的坪數，要的是「不含車位」的單價。所以評分只看
# 「乾淨」的測試資料 (沒車位、或車位價格面積都拆開)，這才是網站真正要預測的東西。
#
# 比較三種做法 (同一份資料、同種子、同測試期)：
#   A. 現狀：照樣訓練，靠 parking_unknown 這個特徵讓模型自己修正
#   B. 還原：用「同縣市、同車位類型」的車位行情 (只用訓練期的成交算)，把混在裡面的車位價格、面積扣掉，
#            換成乾淨的單價再訓練
#   C. 排除：訓練時直接不用這些成交 (資料變少，但答案都是乾淨的)
# ==============================================================================
source(file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "new_model_experiments.R"))
library(data.table)

df <- df %>% add_history_features(cap_month = cut_month) %>% add_road_features(build_road_tables())
df$park_status <- with(df, case_when(include_parking != "是" ~ "沒車位",
                                    coalesce(parking_price, 0) > 0 & coalesce(parking_ping, 0) > 0 ~ "拆開",
                                    TRUE ~ "沒拆開"))
print(count(df, b_category, park_status) %>% tidyr::pivot_wider(names_from = park_status, values_from = n))

# --- B. 車位行情：只用「訓練期、拆開的」成交 (防資料洩漏)，依 縣市 × 車位類型 取中位數 ----------------
pk <- df %>% filter(park_status == "拆開", t_month_idx <= cut_month) %>%
  group_by(city, parking_type) %>% summarise(p_price = median(parking_price), p_ping = median(parking_ping), n = n(), .groups = "drop") %>%
  filter(n >= 20)
pk_city <- df %>% filter(park_status == "拆開", t_month_idx <= cut_month) %>%
  group_by(city) %>% summarise(c_price = median(parking_price), c_ping = median(parking_ping), .groups = "drop")
restored <- df %>% left_join(pk, by = c("city", "parking_type")) %>% left_join(pk_city, by = "city") %>%
  mutate(est_price = coalesce(if_else(coalesce(parking_price, 0) > 0, parking_price, NA_real_), p_price, c_price),
         est_ping  = coalesce(if_else(coalesce(parking_ping, 0) > 0, parking_ping, NA_real_), p_ping, c_ping),
         clean_unit = (total_price - est_price) / (total_ping - est_ping),
         ok = park_status == "沒拆開" & !is.na(clean_unit) & clean_unit > 1e4 & total_ping - est_ping >= 5)
message("B 還原成功的比例：", scales::percent(mean(restored$ok[restored$park_status == "沒拆開"]), 0.1))
df_b <- df %>% mutate(y = if_else(restored$ok, log(restored$clean_unit), y),
                      parking_unknown = if_else(restored$ok, 0L, parking_unknown),
                      pure_house_ping = if_else(restored$ok, total_ping - restored$est_ping, pure_house_ping)) %>%
  add_history_features(cap_month = cut_month)        # 附近行情、同棟歷史也用還原後的單價重算

# --- 訓練、評分 ---------------------------------------------------------------------------
feats <- features_v3
eval_clean <- function(name, train_df) {
  sp <- split_data(train_df)
  sp$test <- split_data(df)$test %>% filter(park_status != "沒拆開")     # 評分：乾淨的測試資料 (三種做法都一樣)
  m <- fit_xgb(sp, feats = feats, seed = 1)
  p <- predict_xgb(m, sp$test, feats = feats)
  log_experiment(paste("06", name), "評分只看乾淨的測試資料 (沒車位、或車位價格面積都拆開)", score(p, sp$test))
}
eval_clean("A 現狀", df)
eval_clean("B 用車位行情還原", df_b)
eval_clean("C 排除沒拆開", df %>% filter(park_status != "沒拆開" | t_month_idx > cut_month))
