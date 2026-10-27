#install.packages("showtext")
library(showtext)
# 自動下載並載入 Google 字體（或是搜尋系統內建字體）
font_add_google("Noto Sans TC", "noto_sans") 
showtext_auto() # 全域開啟字體渲染
library(shiny)
library(leaflet)
library(rio)
library(tidyverse)
library(shinythemes)
library(plotly)
library(leaps)
#設定工作位置
setwd("/Volumes/Dzzz_ssd/R所有資料/R_code/Code")
#讀取檔案
KAO<-import("房地產_清潔版_p.csv",fileEncoding = "BIG5")
data<-KAO


data$鄉鎮市區 = as.factor(data$鄉鎮市區)
data$交易標的 = as.factor(data$交易標的)
data$土地使用分區 = as.factor(data$土地使用分區)
data$建物型態 = as.factor(data$建物型態)
data$建物現況格局.隔間 = as.factor(data$建物現況格局.隔間)
data$有無管理組織 = as.factor(data$有無管理組織)
data$交易筆棟數_建物數量 = as.numeric(data$交易筆棟數_建物數量)
data$有無車位 = as.factor(data$有無車位)
str(data)
#設定selectinput的值
鄉鎮市區 <- distinct(KAO, 鄉鎮市區)
建物型態 <- distinct(KAO, 建物型態)
鄉鎮市區 <-rbind("全部",鄉鎮市區)
建物型態 <-rbind("全部",建物型態)
房屋屋齡 <- data.frame(c("全部","新屋","5年以內","5年以上10年以內","10年以上20年以內","20年以上"))
房屋坪數 <- data.frame(c("全部","50坪以內","50坪到100坪","100坪到200坪","200坪以上"))
names(房屋屋齡)<-"屋齡"
names(房屋坪數)<-"坪數"
有無車位 <- distinct(KAO, 有無車位)
建物現況格局.隔間<-c("y","n")
有無管理組織<-distinct(data,有無管理組織)
有無車位 <-rbind("全部",有無車位)

a1<-鄉鎮市區[-1,]
b1<-建物型態[-1,]
c1<-有無車位[-1,]
#讀取畫地圖的檔案
house_da <-dplyr::select(data,土地區段位置建物區段門牌,建物型態,屋齡_年,總價元,
                         建物單坪價格_萬元,建物移轉總面積_坪,經度,緯度,鄉鎮市區,有無車位)
house_da$總價元_萬元<- round(house_da$總價元/10000,1)


#建構UI
ui <-fluidPage(
  
  titlePanel("房地產地圖資訊系統"),
  navlistPanel(widths=c(2,10),
    #房屋資訊地圖
    tabPanel("地圖",value="地圖",icon=icon("globe-asia"),
             #可選取區域,建物型態,屋齡,坪數,有無車位
             fluidRow(div(style = "height:0px"),
               column(width=1),
               column(width=2, 
                      selectInput("block","高雄各區",c(鄉鎮市區),selectize=F)),
               column(width=2, 
                      selectInput("structure","建物型態",c(建物型態),selectize=F)),
               column(width=2, 
                      selectInput("age","屋齡_年",c(房屋屋齡),selectize=F)),
               column(width=2, 
                      selectInput("size","房屋坪數",c(房屋坪數),selectize=F)),
               column(width=2, 
                      selectInput("car_place","有無車位",c(有無車位),selectize=F)),
            
             mainPanel(width = 12,div(style = "height:0px"),
                       #輸出map
                       column(width=12, 
                              leafletOutput("map",width = "100%",height = 600))
                       ) )
                     ),
    #分析統計輸出長方圖
    tabPanel("分析統計",icon = icon("bar-chart-o"),
             tabsetPanel(
               
               tabPanel("各區平均單坪價格",
                        mainPanel(width = 12,plotlyOutput("cost",width="100%",height = "600px"))),
               
               
               tabPanel("各區平均屋齡",
                        mainPanel(width = 12,plotlyOutput("old",width="100%",height = "600px"))),
               
               
               tabPanel("房屋類型排名",
                        mainPanel(width = 12,plotlyOutput("rank",width="100%",height = "600px"))),
               
               
               tabPanel("各區平均坪數",
                        mainPanel(width = 12,plotlyOutput("big",width="100%",height = "600px"))),
               
               
               tabPanel("各區平均成交量",
                        mainPanel(width = 12,plotlyOutput("sell",width="100%",height = "600px")))
             )),
    #房屋模型輸出text
    tabPanel("房屋分析模型",icon = icon("align-justify"),
             #模型的輸入
             fluidRow(
             column(width=2, 
             selectInput("position","高雄各區",c(a1),selectize=F)),
             
             column(width=2, 
             selectInput("str","建物型態",c(b1),selectize=F)),
             
             column(width=2, 
             selectInput("pattern","有無格局",c(建物現況格局.隔間),selectize=F)),
             
             column(width=2, 
             selectInput("management","有無管理組織",c(有無管理組織),selectize=F)),
             
             column(width=2, 
            selectInput("car_sit","有無車位",c(c1),selectize=F)),
             
             
            column(width=1,numericInput("years","屋齡_年",value=0,min=0)),
            column(width=1,numericInput("size_str","建物坪數",value=100,min=0))
             ),
            actionButton("run", "預測", class = "btn-success"),
             hr(),
             #印出金額
            mainPanel(textOutput("model"))
            
    )
  )
)

server <- function (input,output){# 伺服器端程式
  
  #讀取selectinput建物和鄉鎮的內容
  store1<-reactive({
   
    if(input$block=="全部"|input$block==""){
      if(input$structure=="全部"|input$structure==""){
        a<-house_da
      }
      else{
        a<-filter(house_da,建物型態==input$structure)
      }
    }
    else{
      if(input$structure=="全部"|input$structure==""){
        a<-filter(house_da,鄉鎮市區==input$block)
      }
      else{
        a<-filter(house_da,鄉鎮市區==input$block,建物型態==input$structure)
      }
    }

  })

  #讀取selectinput屋齡區間的內容
  store2<-reactive({
    
    if(input$age=="全部"){
        a<-house_da
    }
    else{
        if(input$age=="新屋"){
          a<-filter(house_da,屋齡_年==0|屋齡_年==1)
        }
        else if(input$age=="5年以內"){
          a<-filter(house_da,1<屋齡_年&屋齡_年<=5)
        }
        else if(input$age=="5年以上10年以內"){
          a<-filter(house_da,屋齡_年<=10&屋齡_年>5)
        }
        else if(input$age=="10年以上20年以內"){
          a<-filter(house_da,屋齡_年<=20&屋齡_年>10)
        }
        else if(input$age=="20年以上"){
          a<-filter(house_da,屋齡_年>20)
        }
       }
    })
  #讀取selectinput坪數區間的內容
  store3<-reactive({
    if(input$size=="全部"){
      a<-house_da
    }
    else{
      if(input$size=="50坪以內"){
        a<-filter(house_da,建物移轉總面積_坪<=50)
      }
      else if(input$size=="50坪到100坪"){
        a<-filter(house_da,建物移轉總面積_坪<=100&建物移轉總面積_坪>50)
      }
      else if(input$size=="100坪到200坪"){
        a<-filter(house_da,建物移轉總面積_坪<=200&建物移轉總面積_坪>100)
      }
      else if(input$size=="200坪以上"){
        a<-filter(house_da,建物移轉總面積_坪>200)
      }
    }
  })
  
  #讀取selectinput車位的內容
  store4<-reactive({
    if(input$car_place=="全部"){
      a<-house_da
    }
    else{
      if(input$car_place=="y"){
        a<-filter(house_da,有無車位=="y")
      }
      else if(input$car_place=="n"){
        a<-filter(house_da,有無車位=="n")
      }
    }
  })
    
  ##畫地圖
  output$map <-renderLeaflet({
    x1<-store1()
    x2<-store2()
    x3<-store3()
    x4<-store4()
    
    a<-Reduce(intersect, list(x1,x2,x3,x4))
    
    a$rank_money<- cut(
      a$總價元_萬元,
      breaks = c(0,500,1000,1500,2000,Inf),
      labels = c('500萬以內','500萬以上1000萬以下','1000萬以上1500萬以下','1500萬以上2000萬以下','2000萬以上'))
    a$color<- cut(a$總價元_萬元,
                  breaks = c(0,500,1000,1500,2000,Inf),
                  labels = c('black','purple','green','blue','red'))
    #延續上一個步驟，房價限分割完區間，共分了5組，所以用5種顏色來分別標示不同的房價
    pal <- colorFactor(palette = c('black','purple','green',
                                   'blue','red'),domain = a$rank_money)
    rank_list<- split(a,a$rank_money)
    # house_da<-filter(house_da,鄉鎮市區==input$block,建物型態==input$structure)
    map<- leaflet()%>%
      addTiles()%>%                 #addTiles()加上一個預設的地圖資料
      #fitBounds()用來設定地圖的位置，讓畫面可呈現指定的方形區域，這兩組經緯度是高雄市的經緯度位置
      fitBounds(120.1032,22.28,121.0115,23.28)
    names(rank_list) %>% purrr::walk( function(df) {
      map <<-map %>% addAwesomeMarkers(data=rank_list[[df]],icon=awesomeIcons(icon = 'home',markerColor = rank_list[[df]]$color),
                                       clusterOptions = markerClusterOptions(removeOutsideVisibleBounds=F),
                                       lng = rank_list[[df]]$經度,lat =rank_list[[df]]$緯度,popup =paste(sep='<br/>',
                                                                                                     '地址',rank_list[[df]]$土地區段位置建物區段門牌,'總價元_萬元',rank_list[[df]]$總價元_萬元,'屋齡_年',
                                                                                                     rank_list[[df]]$屋齡_年,'總坪數',rank_list[[df]]$建物移轉總面積_坪,'單坪價格_萬元',rank_list[[df]]$建物單坪價格_萬元,
                                                                                                     '建物型態',rank_list[[df]]$建物型態,'有無車位',rank_list[[df]]$有無車位) ,group= df)
    })
    map<- map%>% addLayersControl(overlayGroups =names(rank_list),
                                  options = layersControlOptions(collapsed = F))%>%
      addLegend(position = 'bottomright',pal=pal,values = a$rank_money,opacity = 1)
    map
  })
  #畫平均各區單坪房價
  output$cost <- renderPlotly({
    
     data1<-KAO
     統計<-group_by(data1,鄉鎮市區)%>%
     summarise(
        平均單坪價格_萬元=round(sum(建物單坪價格_萬元)/n(),1 ),
        每區購屋占比_百分比= round( n()/6957*100,2 ),
        每區購買件數=n(),
        每區平均屋齡=round(sum(屋齡_年)/n(),1))%>%arrange(desc(平均單坪價格_萬元))%>%filter(每區購買件數>=100)
     #windowsFonts(A=windowsFont("微軟正黑體"))  
     ggplotly(ggplot(data=統計[], aes(x=reorder(鄉鎮市區,-平均單坪價格_萬元),y=平均單坪價格_萬元,fill=鄉鎮市區)) +
      geom_bar(stat="identity",color="black")+
      ggtitle("單坪價格行政區排行")+ 
        labs(x = "高雄各鄉鎮市區", y = "平均單坪價格(萬元)")+
        theme_minimal(base_size = 20)+
      theme(
            axis.line = element_line(color = "orange", size = 2),
            axis.title = element_text(color = "red", face = "bold",family = "A"),
            axis.ticks = element_line(color = "purple", size = 3),
            axis.text = element_text(color = "blue",face = "bold",family = "A")
            ))
  })
  #畫平均各區販售數
  output$sell <- renderPlotly({
    
    data1<-KAO
    統計<-group_by(data1,鄉鎮市區)%>%
      summarise(
        平均單坪價格_萬元=round(sum(建物單坪價格_萬元)/n(),1 ),
        成交量_百分比= round( n()/6957*100,2 ),
        每區購買件數=n(),
        每區平均屋齡=round(sum(屋齡_年)/n(),1))%>%arrange(desc(每區平均屋齡))%>%filter(每區購買件數>=100)
    windowsFonts(A=windowsFont("微軟正黑體"))
    
    barplot_zone3<-ggplot(data=統計, aes(x=reorder(鄉鎮市區,-每區購買件數),y=每區購買件數,fill=鄉鎮市區)) +
      geom_bar(stat="identity",color="black")+
      ggtitle("各區成交量排名型態排名")+
      labs(x = "鄉鎮市區", y = "房屋銷售量")+
      theme_minimal(base_size = 20)+
      theme(
        axis.line = element_line(color = "orange", size = 2),
        axis.title = element_text(color = "red", face = "bold",family = "A"),
        axis.ticks = element_line(color = "purple", size = 3),
        axis.text = element_text(color = "blue",face = "bold",family = "A")
      )
    
    ggplotly(barplot_zone3)
    
    
  })
  
  #畫平均各區屋齡
  output$old <- renderPlotly({
    
    data1<-KAO
    統計<-group_by(data1,鄉鎮市區)%>%
      summarise(
        平均單坪價格_萬元=round(sum(建物單坪價格_萬元)/n(),1 ),
        每區購屋占比_百分比= round( n()/6957*100,2 ),
        每區購買件數=n(),
        每區平均屋齡=round(sum(屋齡_年)/n(),1))%>%arrange(desc(每區平均屋齡))%>%filter(每區購買件數>=100)
    windowsFonts(A=windowsFont("微軟正黑體"))
    
    ggplotly(ggplot(data=統計[], aes(x=reorder(鄉鎮市區,-每區平均屋齡),y=每區平均屋齡,fill=鄉鎮市區)) +
               geom_bar(stat="identity",color="black")+
               ggtitle("單坪價格行政區排行")+ 
               labs(x = "高雄各鄉鎮市區", y = "每區平均屋齡(年)")+
               theme_minimal(base_size = 20)+
               theme(
                 axis.line = element_line(color = "orange", size = 2),
                 axis.title = element_text(color = "red", face = "bold",family = "A"),
                 axis.ticks = element_line(color = "purple", size = 3),
                 axis.text = element_text(color = "blue",face = "bold",family = "A")
               ))
  })
  #畫各建物類型占比
  output$rank <- renderPlotly({
    
    data1<-KAO
    統計<-group_by(data1,建物型態)%>%
    summarise(
    平均單坪價格_萬元=round(sum(建物單坪價格_萬元)/n(),1 ),
    建物佔比_百分比= round( n()/6957*100,2 ),
    每區購買件數=n(),
    每區平均屋齡=round(sum(屋齡_年)/n(),1))%>%arrange(desc(每區平均屋齡))%>%filter(每區購買件數>=100)
    windowsFonts(A=windowsFont("微軟正黑體"))
    
    barplot_zone3<-ggplot(data=統計, aes(x=reorder(建物型態,-建物佔比_百分比),y=建物佔比_百分比,fill=建物型態)) +
      geom_bar(stat="identity",color="black")+
      ggtitle("交易建物型態排名")+
      labs(x = "建物型態", y = "成交房屋百分比(%)")+
      theme_minimal(base_size = 18)+
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
             axis.line = element_line(color = "orange", size = 2),
             axis.title = element_text(color = "red", face = "bold",family = "A"),
             axis.ticks = element_line(color = "purple", size = 3),
             axis.text = element_text(color = "blue",face = "bold",family = "A")
                                  )
    ggplotly(barplot_zone3)
    
  })

  #畫平均各區坪數
  output$big <- renderPlotly({
    data1<-KAO
    統計<-group_by(data1,鄉鎮市區)%>%
      summarise(
        平均單坪價格_萬元=round(sum(建物單坪價格_萬元)/n(),1 ),
        每區購屋占比_百分比= round( n()/6957*100,2 ),
        每區購買件數=n(),
        每區平均屋齡=round(sum(屋齡_年)/n(),1),
        每區平均坪數=round(sum(建物移轉總面積_坪)/n(),1)) %>%arrange(desc(平均單坪價格_萬元))%>%filter(每區購買件數>=100)
    
    barplot_zone<-ggplot(data=統計, aes(x=reorder(鄉鎮市區,-每區平均坪數),y=每區平均坪數,fill=鄉鎮市區)) +
      geom_bar(colour="black",stat="identity")+
      ggtitle("高雄各區平均坪數")+
      labs(x = "高雄各鄉鎮市區", y = "平均坪數")+
      theme_minimal(base_size = 20)+
      theme(
        axis.line = element_line(color = "orange", size = 2),
        axis.title = element_text(color = "red", face = "bold",family = "A"),
        axis.ticks = element_line(color = "purple", size = 3),
        axis.text = element_text(color = "blue",face = "bold",family = "A")
      )
    ggplotly(barplot_zone)  

  })
  #用按鈕紀錄選取的內容
  observeEvent(input$run,{
    
    str(data)
    lmall <- lm(log10(總價元)~ 鄉鎮市區+建物型態+建物移轉總面積_坪+屋齡_年+
                  建物現況格局.隔間+有無管理組織+有無車位, data=data)
    summary(lmall)

    
    summary(lmall)
    df <- as.data.frame(lmall$coefficients)
    if(input$pattern=="n"){
      x6<-0
    }
    else{
      x6<-df[paste0("建物現況格局.隔間",input$pattern),]
    }
    if(input$management=="n"){
      x7<-0
    }
    else{
      x7<-df[paste0("有無管理組織",input$management),]
    }
    if(input$car_sit=="n"){
      x8<-0
    }
    else{
      x8<-df[paste0("有無車位",input$car_sit),]
    }
    if(input$position=="三民區"){
      x2<-0
    }
    else{
      x2<-df[paste0("鄉鎮市區",input$position),]
    }
    if(input$str=="公寓(5樓含以下無電梯)"){
      x3<-0
    }
    else{
      x3<-df[paste0("建物型態",input$str),]
    }
    x<- df["(Intercept)",]+x2+df["屋齡_年",]*input$years+
      x3+df["建物移轉總面積_坪",]*input$size_str+
      x6+x7+x8
    
    y<- 10^x
    y<-round(y/10000,3)
    
    #印出價格
    output$model<-renderPrint({
   
    print(paste0("預測金額為 ",y," 萬元"))
    
  })
  })

}

shinyApp(ui = ui, server = server)