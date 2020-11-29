#install.packages("stringr")
#install.packages("dplyr")
#install.packages('tidyverse')
options(shiny.sanitize.errors = F) ## Error 내용 확인가능


library(shiny)
library(tidyverse) #ggplot2 사용
library(dplyr) # %>%  사용
library(showtext) #한글패치

#한글패치 나눔고딕체
font_add_google(name = "Nanum Gothic")
showtext_auto()
showtext_opts(dpi = 100)


#github에서 csv파일을 받아와서 샤이니 실행할때마다 연동하는 방법도 있음
#install github 해서 iris 불러오듯이 데이터파일 불러올수있게 만듬

data1 <- readRDS(file = 'long_1128_1.RDS')#매출액 데이터 파일 불러오기
data2 <- readRDS(file = 'long_1128_2.RDS')#부채 데이터 파일 불러오기

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel(h3(tags$b("1997Invest")), tags$div(img(src = "img1.jpg"))),
  actionButton("action1","HOME   "), actionButton("action2","NOTICE   "), actionButton("action3","
Investment IF   "),actionButton("action4","Debate"),
  hr(),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("text", label = h4(tags$b("회사명 정확하게 입력하세요")), placeholder = "EX)LG 화학"), #input data 입력받기

      tags$style('h4{color:#000080;}'),
      tags$div(h4(tags$b("국내증시")),h5("- 코스피"),h5("- 코스닥"),h5("- 선물"),h5("- 옵션")),

      tags$div(h4(tags$b("업종별 시세")),h5("- WICS"),h5("- KRX100")),

      tags$div(h4(tags$b("종목랭킹 검색")),h5("- 시가총액별"),h5("- 거래량별"),h5("- 상승률"),h5("- 하락률")),

      tags$div(h4(tags$b("거래별 종목랭킹")),h5("- 외국인/기관매매"),h5("- 외국인 보유율 상위"),h5("- 투자"),h5("- 증시")),

      tags$div(h4(tags$b("오늘의 날씨")),h5(" "),h5("- 날씨 정보"),h5("- 출처 : https://finance.naver.com/")),

      hr(),
      submitButton('submit'), # 제출버튼을 누르면 서버로 데이터가 전송
      downloadButton(outputId = "download", label = "Download the ggplot chart") #ggplot2 데이터 파일 저장


    ),


    # Show a plot of the generated distribution
    mainPanel(
      uiOutput("mainUI"), #plot 보여줄 UI
      img('img10.jpg')


    ),

  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$value <- renderPrint({ input$text })

  # 그래프 그리기
  output$barplot1 <- renderPlot({

    #ggplot2 pdf로 저장하기
    output$download <- downloadHandler(
      filename = function(){paste("barplot_", input$combobox,".pdf", sep="")},
      content = function(file){
        pdf(file)
        x <- data()[, input$combobox]
        hist(x, breaks = input$bins, col = input$color, border = 'white')
        dev.off()
      }
    )

    #plot 그리기
    data1 %>%
      filter(회사 == input$text) %>%
      ggplot(aes(x = year, y = sales , fill = year)) +
      geom_col() +
      ggtitle("[Salse volume]")+
      theme_bw()

  })


  output$barplot2 <- renderPlot({


    #ggplot2 pdf로 저장하기
    output$download <- downloadHandler(
      filename = function(){paste("barplot_", input$combobox,".pdf", sep="")},
      content = function(file){
        pdf(file)
        x <- data()[, input$combobox]
        hist(x, breaks = input$bins, col = input$color, border = 'white')
        dev.off()
      }
    )

    #plot 그리기
    data2 %>%
      filter(회사 == input$text) %>%
      ggplot(aes(x = year, y = debt , fill = year)) +
      geom_col() +
      ggtitle("[Debt volume]")+
      theme_bw()

  })

  #출력

  output$mainUI <- renderUI({

    tabsetPanel(tabPanel("Sales Volume",plotOutput("barplot1")),
                tabPanel("Debt Volume",plotOutput("barplot2")))
  })


}

# Run the application
shinyApp(ui = ui, server = server)
