# --- 1. 環境設定 ---
library(showtext)
font_add_google("Noto Sans TC", "noto_sans")
showtext_auto()

library(shiny)
library(leaflet)
library(rio)
library(tidyverse)
library(shinythemes)
library(plotly)
library(RColorBrewer)

# --- 2. 資料匯入與預處理 ---
# 讀取資料
KAO <- import("房地產_清潔版_p.csv", fileEncoding = "BIG5")
data <- KAO

# 數值與型態修正 (這對預測功能至關重要)
data$鄉鎮市區 <- as.factor(data$鄉鎮市區)
data$建物型態 <- as.factor(data$建物型態)
data$有無車位 <- as.factor(data$有無車位)
data$總價元_萬元 <- round(data$總價元 / 10000, 1)
data$交易年月 <- paste0(data$交易年, "/", data$交易月)

# 選單內容準備
list_dist <- levels(data$鄉鎮市區)
list_type <- levels(data$建物型態)
屋齡級距_標籤 <- c("全部","新屋","5年以內","5-10年","10-20年","20-30年","30-40年","40年以上")

# --- 3. 使用者介面 (UI) ---
ui <- navbarPage(
  theme = shinytheme("flatly"),
  title = "高雄房地產數據金流地圖",
  
  # 加入全域 CSS 讓字體更精緻
  header = tags$head(
    tags$style(HTML("
      body { font-family: 'Noto Sans TC', sans-serif; }
      .navbar-brand { font-weight: bold; }
      .well { background-color: #ffffff; border-radius: 10px; border: 1px solid #eceeef; box-shadow: 0 2px 4px rgba(0,0,0,0.05); }
      .btn-primary { background-color: #2c3e50; border: none; }
      .btn-primary:hover { background-color: #1a252f; }
    "))
  ),
  
  # 【分頁 1：地圖探索】
  tabPanel("🔍 全域地圖探索", icon = icon("map-marked-alt"),
           fluidRow(
             column(3, selectInput("map_block", "行政區篩選", c("全部", list_dist))),
             column(3, selectInput("map_structure", "建物型態", c("全部", list_type))),
             column(3, selectInput("map_age", "精細屋齡篩選", 屋齡級距_標籤)),
             column(3, selectInput("map_car", "車位需求", c("全部", "y", "n")))
           ),
           hr(),
           leafletOutput("map", height = "750px")
  ),
  
  # 【分頁 2：產品佔比分析】
  tabPanel("📊 產品佔比分析", icon = icon("chart-pie"),
           sidebarLayout(
             sidebarPanel(
               selectInput("pie_dist", "選擇聚焦行政區", c("全部", list_dist)),
               helpText("分析特定區域內，各類型房產（如大樓、透天）的成交佔比。")
             ),
             mainPanel(plotlyOutput("rank_plot", height = "650px"))
           )
  ),
  
  # 【分頁 3：成交排行榜】
  tabPanel("📈 成交排行榜", icon = icon("trophy"),
           sidebarLayout(
             sidebarPanel(
               selectInput("sell_type", "篩選建物型態", c("全部", list_type)),
               sliderInput("sell_min", "過濾成交件數門檻 (筆)", 0, 300, 50),
               helpText("找出特定建物類型的熱點成交區域。")
             ),
             mainPanel(plotlyOutput("sell_plot", height = "650px"))
           )
  ),
  
  # 【分頁 4：單價行情榜】
  tabPanel("💰 單價行情榜", icon = icon("funnel-dollar"),
           sidebarLayout(
             sidebarPanel(
               sliderInput("cost_min", "過濾成交件數門檻 (筆)", 0, 300, 50),
               helpText("過濾交易量過少的行政區，獲取更具代表性的平均單價。")
             ),
             mainPanel(plotlyOutput("cost_plot", height = "650px"))
           )
  ),
  
  # 【分頁 5：AI 房價估算】(修復版)
  tabPanel("🤖 AI 房價估算", icon = icon("robot"),
           fluidRow(
             column(4, wellPanel(
               h4("📍 房屋基本條件"),
               selectInput("mod_pos", "行政區", list_dist),
               selectInput("mod_str", "建物型態", list_type),
               selectInput("mod_car", "有無車位", c("y", "n"))
             )),
             column(4, wellPanel(
               h4("📐 面積與屋齡"),
               numericInput("mod_size", "建物總坪數", 30, min = 1),
               numericInput("mod_land", "土地持分坪數", 10, min = 0),
               numericInput("mod_age", "屋齡 (年)", 15, min = 0)
             )),
             column(4, 
                    br(), br(),
                    actionButton("run_mod", "開始計算估價", class = "btn-primary btn-lg btn-block"),
                    hr(),
                    div(align="center", 
                        h3("系統預估總價", style="color:#7f8c8d;"),
                        h1(textOutput("model_res"), style = "color: #e74c3c; font-weight: bold;"))
             )
           )
  )
)

# --- 4. 伺服器邏輯 (Server) ---
server <- function(input, output, session) {
  
  # --- 1. 地圖邏輯 (採用你最喜歡的 Popup 呈現方式) ---
  map_filtered <- reactive({
    d <- data
    if(input$map_block != "全部") d <- d[d$鄉鎮市區 == input$map_block, ]
    if(input$map_structure != "全部") d <- d[d$建物型態 == input$map_structure, ]
    if(input$map_car != "全部") d <- d[d$有無車位 == input$map_car, ]
    
    age <- d$屋齡_年
    d <- switch(input$map_age,
                "全部" = d, "新屋" = d[age <= 1, ], "5年以內" = d[age > 1 & age <= 5, ],
                "5-10年" = d[age > 5 & age <= 10, ], "10-20年" = d[age > 10 & age <= 20, ],
                "20-30年" = d[age > 20 & age <= 30, ], "30-40年" = d[age > 30 & age <= 40, ],
                "40年以上" = d[age > 40, ], d)
    return(d)
  })
  
  output$map <- renderLeaflet({
    d <- map_filtered()
    validate(need(nrow(d) > 0, "查無符合篩選條件的資料"))
    
    leaflet(d) %>% addTiles() %>%
      addCircleMarkers(
        lng = ~經度, lat = ~緯度, clusterOptions = markerClusterOptions(),
        color = "#2c3e50", fillOpacity = 0.8, radius = 7,
        popup = ~paste0(
          "<div style='font-size:14px; font-family:Noto Sans TC;'>",
          "<b>", 土地區段位置建物區段門牌, "</b><br>",
          "<hr style='margin:5px 0;'>",
          "💰 總價：<b style='color:#e74c3c;'>", 總價元_萬元, " 萬</b><br>",
          "📏 總坪數：", 建物移轉總面積_坪, " 坪<br>",
          "🌱 土地坪數：", 土地移轉總面積_坪, " 坪<br>",
          "🏗️ 型態：", 建物型態, " | 屋齡：", 屋齡_年, "年<br>",
          "📅 交易年月：", 交易年月, "</div>"
        )
      )
  })
  
  # --- 2. 佔比分析 (獨立介面) ---
  output$rank_plot <- renderPlotly({
    d_stat <- if(input$pie_dist == "全部") data else data[data$鄉鎮市區 == input$pie_dist, ]
    統計 <- d_stat %>% group_by(建物型態) %>% summarise(數量 = n())
    
    plot_ly(統計, labels = ~建物型態, values = ~數量, type = 'pie', hole = 0.5,
            textinfo = 'label+percent',
            insidetextfont = list(size = 14),
            marker = list(colors = brewer.pal(8, "Pastel1"))) %>%
      layout(title = list(text = paste(input$pie_dist, "產品成交佔比"), font = list(size = 20)),
             margin = list(t = 60, b = 100, l = 50, r = 50),
             legend = list(orientation = "h", x = 0, y = -0.2))
  })
  
  # --- 3. 成交排行 (獨立介面) ---
  output$sell_plot <- renderPlotly({
    d_sell <- data
    if(input$sell_type != "全部") d_sell <- d_sell[d_sell$建物型態 == input$sell_type, ]
    
    統計 <- d_sell %>% group_by(鄉鎮市區) %>% summarise(件數 = n()) %>%
      filter(件數 >= input$sell_min) %>% arrange(desc(件數))
    
    plot_ly(統計, x = ~reorder(鄉鎮市區, -件數), y = ~件數, type = 'bar',
            marker = list(color = '#1abc9c')) %>%
      layout(title = paste("各區", input$sell_type, "成交件數排行"),
             xaxis = list(title = ""), yaxis = list(title = "成交筆數"))
  })
  
  # --- 4. 單價行情 (獨立介面) ---
  output$cost_plot <- renderPlotly({
    統計 <- data %>% group_by(鄉鎮市區) %>%
      summarise(平均單價 = round(mean(建物單坪價格_萬元, na.rm=T), 1), 件數 = n()) %>%
      filter(件數 >= input$cost_min) %>% arrange(desc(平均單價))
    
    plot_ly(統計, x = ~reorder(鄉鎮市區, -平均單價), y = ~平均單價, type = 'bar',
            marker = list(color = '#3498db')) %>%
      layout(title = "各行政區平均單價排行",
             xaxis = list(title = ""), yaxis = list(title = "萬元 / 坪"))
  })
  
  # --- 5. 預測模型 (修復與強化) ---
  observeEvent(input$run_mod, {
    # 使用當前資料訓練模型，確保包含所有必要的解釋變數
    lm_model <- lm(log10(總價元) ~ 鄉鎮市區 + 建物型態 + 建物移轉總面積_坪 + 土地移轉總面積_坪 + 屋齡_年 + 有無車位, data = data)
    
    # 建立預測輸入資料，並嚴格定義 Factor Levels
    new_data <- data.frame(
      鄉鎮市區 = factor(input$mod_pos, levels = levels(data$鄉鎮市區)),
      建物型態 = factor(input$mod_str, levels = levels(data$建物型態)),
      建物移轉總面積_坪 = input$mod_size,
      土地移轉總面積_坪 = input$mod_land,
      屋齡_年 = input$mod_age,
      有無車位 = factor(input$mod_car, levels = levels(data$有無車位))
    )
    
    # 執行預測並轉換回萬元單位
    res <- predict(lm_model, new_data)
    final_price <- round(10^res / 10000, 1)
    
    output$model_res <- renderText({ paste0(final_price, " 萬") })
  })
}

# 啟動應用
shinyApp(ui, server)