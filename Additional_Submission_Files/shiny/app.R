# Used packages
pacotes = c("shiny", "shinythemes", "tidyverse", "data.table", "lubridate")

# Run the following command to verify that the required packages are installed. If some package is missing, it will be installed automatically
package.check <- lapply(pacotes, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})

pacotes2 = c("AnomalyDetection")

package.check <- lapply(pacotes2, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    devtools::install_github("twitter/AnomalyDetection")
  }
})


library(data.table)
library(shiny)
library(tidyverse)
library(lubridate)
library(AnomalyDetection)
library(shinythemes)

fw = fread('dong_fw.csv')


ui = navbarPage("제주시 행정동별 음식물 배출량의 이상치 탐지",
                tabPanel("Graphic",fluidPage(theme = shinytheme("flatly")),
                         tags$head(
                           tags$style(HTML(".datepicker {z-index:99999 !important;}"))),
                         
                         pageWithSidebar(
                           headerPanel('
                                  일간 이상치 탐지기'),
                           sidebarPanel(width = 3,
                                        
                                        strong("도움말"),
                                        br(),
                                        p("지난 3개월의 추세, 계절성을 바탕으로
                                            오늘의 이상치 여부를 판단합니다"),
                                        br(),
                                        
                                        
                                        selectInput("dong",
                                                    label = "행정동을 선택하세요",
                                                    choices = fw$emd_nm %>% unique(),
                                                    selected = "건입동"),
                                        
                                        
                                        
                                        dateInput('Date',
                                                  label = "오늘의 날짜를 입력하세요",
                                                  value = "2021-06-30",
                                                  min = "2018-01-01", max = "2021-06-30",
                                                  startview = 'month', weekstart = 1
                                        ),
                                        
                                        
                                        br(),
                                        
                                        
                                        img(src = "logo3.png", height = 140, width = 360)
                           ),
                           mainPanel(
                             h1(textOutput("selected_dong")),
                             
                             plotOutput('plot1'),
                             
                             h3( strong(textOutput("anom_date"))))
                         )),
                tabPanel("About",p("저희는 ", a("빅콘테스트", href="https://www.bigcontest.or.kr/", target="_blank"),
                                   "에서 제공한  2018년 1월부터 2021년 6월까지의 제주도 음식물쓰레기 배출량의 데이터에대해 이상치를 탐지하는 서비스를 제공합니다. 2016년부터 제주 전역으로 확대된 RFID는 데이터를 기반한 음식물 쓰레기 관리를 가능하게 했지만, 고장과 기기교체 등의 이상 상황들은 실제 배출량 파악하기 어렵게 만듭니다. 이는 음식물 쓰레기 감축에 큰 걸림돌로 작용되고 있으므로 이상치탐지를 기반한 정확한 현황 관리를 통해  RFID의 유지 보수, 더 나아가 청정 제주를 보전하는 데에 기여하고자 합니다. ",style = "font-size:25px"),
                         
                         hr(), 
                         p("이상치 탐지 대상 행정동:",style = "font-size:25px"),
                         p("건입동", style = "font-size:15px;color: blue"),
                         p("구좌읍",style = "font-size:15px;color: blue"),
                         p("남원읍",style = "font-size:15px;color: blue"),
                         p("노형동",style = "font-size:15px;color: blue"),
                         p("대륜동",style = "font-size:15px;color: blue"),
                         p("대정읍",style = "font-size:15px;color: blue"),
                         p("대천동",style = "font-size:15px;color: blue"),
                         p("도두동",style = "font-size:15px;color: blue"),
                         p("동홍동",style = "font-size:15px;color: blue"),
                         p("봉개동",style = "font-size:15px;color: blue"),
                         p("삼도1동",style = "font-size:15px;color: blue"),
                         p("삼도2동",style = "font-size:15px;color: blue"),
                         p("삼양동",style = "font-size:15px;color: blue"),
                         
                         p("서홍동",style = "font-size:15px;color: blue"),
                         p("성산읍",style = "font-size:15px;color: blue"),
                         p("송산동",style = "font-size:15px;color: blue"),
                         p("아라동",style = "font-size:15px;color: blue"),
                         p("안덕면",style = "font-size:15px;color: blue"),
                         p("애월읍",style = "font-size:15px;color: blue"),
                         p("연동",style = "font-size:15px;color: blue"),
                         p("영천동",style = "font-size:15px;color: blue"),
                         p("예래동",style = "font-size:15px;color: blue"),
                         p("오라동",style = "font-size:15px;color: blue"),
                         p("외도동",style = "font-size:15px;color: blue"),
                         p("용담1동",style = "font-size:15px;color: blue"),
                         p("용담2동",style = "font-size:15px;color: blue"),
                         p("이도1동",style = "font-size:15px;color: blue"),
                         p("이도2동",style = "font-size:15px;color: blue"),
                         p("이호동",style = "font-size:15px;color: blue"),
                         p("일도1동",style = "font-size:15px;color: blue"),
                         p("일도2동",style = "font-size:15px;color: blue"),
                         p("정방동",style = "font-size:15px;color: blue"),
                         p("조천읍",style = "font-size:15px;color: blue"),
                         p("중문동",style = "font-size:15px;color: blue"),
                         p("중앙동",style = "font-size:15px;color: blue"),
                         p("천지동",style = "font-size:15px;color: blue"),
                         p("표선면",style = "font-size:15px;color: blue"),
                         p("한경면",style = "font-size:15px;color: blue"),
                         p("한림읍",style = "font-size:15px;color: blue"),
                         p("화북동",style = "font-size:15px;color: blue"),
                         p("효돈동",style = "font-size:15px;color: blue")),
                
                tabPanel("Developers",
                         strong("Team UmStatistics",style = "font-size:35px;color: darkblue" ),br(),
                         p("권남택",style = "font-size:25px;color: darkblue"),
                         p("e-mail: dracon5@g.skku.edu",style = "font-size:20px"),
                         p("오정민",style = "font-size:25px;color: darkblue"),
                         p("email: ojm123@g.skku.edu",style = "font-size:20px"),
                         p("유경민",style = "font-size:25px;color: darkblue"),
                         p("e-mail: ykm25031@gmail.com",style = "font-size:20px"),
                         p("이상현",style = "font-size:25px;color: darkblue"),
                         p("e-mail: sanghyun1154@gmail.com",style = "font-size:20px"),
                         
                         br(),
                         br(),
                         p(a("Thiago", href="https://github.com/ThiagoValentimMarques", target="_blank"), "를 참고하여 제작하였습니다.", style = "font-size:15px;color: black"),
                         p(a("AnomalyDetection", href="https://github.com/twitter/AnomalyDetection", target="_blank"), "패키지를 사용하여 제작하였습니다.", style = "font-size:15px;color: black"))
                
)

# Define server logic ----
server <- function(input, output) {
  
  output$selected_dong = renderText({
    paste0(input$dong , "의 ", input$Date, " 이상치 탐지 결과" )
  })
  
  res = reactive({
    fw %>% 
      as_tibble() %>% 
      filter(emd_nm==input$dong & (as.Date(input$Date) - 90) <= base_date & base_date <= input$Date) %>% 
      select(-emd_nm) %>%
      AnomalyDetectionTs(max_anoms=0.1, direction='neg', plot=TRUE)
    
  })
  
  output$plot1 = renderPlot({
    anoms <- res()$anoms
    anoms <- data.frame(anoms)
    
    if(is.null(anoms$timestamp) == T) {
      fw %>% 
        as_tibble() %>% 
        filter(emd_nm==input$dong & (as.Date(input$Date) - 90) <= base_date  & base_date <= input$Date) %>% 
        ggplot(aes(x=base_date, y=em_g))+
        geom_line(colour='RoyalBlue')+
        theme_minimal()+
        theme(axis.title.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.y=element_blank())
      
    } else{
      anoms$timestamp <- as.POSIXct(anoms$timestamp)
      fw %>% 
        as_tibble() %>% 
        filter(emd_nm==input$dong & (as.Date(input$Date) - 90) <= base_date  & base_date <= input$Date) %>% 
        ggplot(aes(x=base_date, y=em_g))+
        geom_line(colour='RoyalBlue') +
        geom_point(data=anoms, aes(x=timestamp, y=anoms), shape=21, size=3, colour="firebrick")+
        theme_minimal()+
        theme(axis.title.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.y=element_blank())}
    
  })
  
  output$anom_date = renderText({
    if(is.null(res()$anoms$timestamp)==T){"이상치가 아닙니다."}
    else{
      if(res()$anoms$timestamp[length(res()$anoms$timestamp)] !=input$Date)
        {"이상치가 아닙니다"}else{"이상치가 탐지되었습니다."}}
  })

}
# Run the app ----
shinyApp(ui = ui, server = server)