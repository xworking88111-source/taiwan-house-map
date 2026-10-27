# 如果還沒安裝請執行: 
#install.packages(c("DBI", "RPostgres"))
library(DBI)
library(RPostgres)

# 建立連線 (因為是剛初始化，預設資料庫名稱是 postgres，使用者也是你的電腦帳號)
con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'postgres',
                 host = 'localhost',
                 port = 5432,
                 user = 'dzzz') # 截圖顯示你的使用者名稱是 dzzz

# 測試：列出目前的資料表 (應該是空的，但不能報錯)
dbListTables(con)

# 讀取你硬碟裡的最終合併檔
all_data <- readRDS("/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data/national_trade_final_history.rds")
# 在寫入之前，確保它是最純粹的資料框格式
all_data_pure <- as.data.frame(all_data)

target_files <- list.files(temp_unzip_dir, 
                           pattern = "_lvr_land_a\\.csv$", 
                           full.names = TRUE, 
                           recursive = TRUE, 
                           ignore.case = TRUE)

# 讀取一個樣本檔的標題與英文名
sample_file <- target_files[1]
header_cn <- colnames(read_csv(sample_file, n_max = 0, locale = locale(encoding = "UTF-8")))
header_en <- read_csv(sample_file, skip = 1, n_max = 1, col_names = FALSE, locale = locale(encoding = "UTF-8")) %>% 
  unlist() %>% as.character()

# 建立對照表（方便你以後對照）
col_mapping <- data.frame(CN = header_cn, EN = header_en)
print(col_mapping)

# 換掉欄位名稱
all_data_eng <- my_data
colnames(all_data_eng) <- header_en

# 寫入資料庫（建立一張乾淨的英文版資料表）
dbWriteTable(con, "real_estate_eng", all_data_eng, overwrite = TRUE)

# 使用 SQL 語法查詢總筆數
dbGetQuery(con, "SELECT COUNT(*) FROM raw_real_estate")
# 查詢資料表的大小
dbGetQuery(con, "SELECT pg_size_pretty(pg_total_relation_size('raw_real_estate'))")

#檢查是否成功連接
#dbIsValid(con)

# 再次檢查資料表清單
dbListTables(con)

start_time <- Sys.time()

sanmin_data <- dbGetQuery(con, "
  SELECT * FROM raw_real_estate 
  WHERE \"鄉鎮市區\" = '三民區' AND \"縣市\" = '高雄市'
")

end_time <- Sys.time()
print(end_time - start_time)
print(nrow(sanmin_data))

# dbDisconnect(con)

