## Mini Project - Cell number Calculator
# Have to give ui, server
library(shiny)

ui <- fluidPage(
  titlePanel("Cell count"),           # 앱 제목
  sidebarLayout(                        # 사이드바 + 메인 영역
    sidebarPanel(
      numericInput("C_num",                # 입력 위젯 (name tag)
                  "Number of cells (20-200) :",
                  min = 20,
                  max = 200,
                  value = NA),
      
      numericInput("C_dilute",                # 입력 위젯 (name tag)
                   "Dilution factor:",
                   min = 1,
                   max = 10,
                   value = NA),
      withMathJax(),
      uiOutput("C_total")          # 출력 영역
    ),
    mainPanel()
  )
)
server <- function(input, output) {
  
  C_num <- reactive({input$C_num})
  C_dilute <- reactive({input$C_dilute})
  
  output$C_total <- renderUI({
    C_total <- as.numeric(C_num()) * 10^4 * 10^(as.numeric(C_dilute()))
    
    C_ten <- nchar(as.character(format(C_total, scientific = F))) - 1
    C_final <- C_total/(10^(C_ten))
    
    withMathJax(
      helpText(paste("Total Cell number = $$", C_final, "x 10^", C_ten, "$$"))
      )
  })
}

# Running function (fix grammar)
runApp(shinyApp(ui, server))
