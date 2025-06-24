## Mini project2 - Molecular calculator_Morlarity
library(shiny)
library(bslib)            ## Good for designing
library(shinyWidgets)

ui <- fluidPage(
  titlePanel("Calculator"),           # 앱 제목
  sidebarLayout(                        # 사이드바 + 메인 영역
    sidebarPanel(
      selectizeInput("mass_unit", "Mass units:",
                     choices = list("kilogram" = 1,
                                    "gram" = 2,
                                   "miligram" = 3,
                                   "microgram" = 4,
                                   "nanogram" = 5),
                     multiple = FALSE),
      
      selectizeInput("volume_unit", "Volume units:",
                     choices = list("liter" = 1,
                                    "mililiter" = 2,
                                    "microliter" = 3),
                     multiple = FALSE),
      
      numericInput("M_mass",                # 입력 위젯 (name tag)
                   "Mass:",
                   min = 0,
                   max = 10000,
                   value = NA),
      
      numericInput("M_weight",                # 입력 위젯 (name tag)
                   "Formula weight(Dalton):",
                   min = 0,
                   max = 10000,
                   value = NA),
      
      numericInput("M_volume",                # 입력 위젯 (name tag)
                   "Volume: ",
                   min = 0,
                   max = 10000,
                   value = NA),
      
      withMathJax(),
      uiOutput("M_result")          # 출력 영역
    ),
    mainPanel()
  )
)
server <- function(input, output, session) {
  
  M_mass <- reactive({input$M_mass})
  M_weight <- reactive({input$M_weight})
  M_volume <- reactive({input$M_volume})
  
  output$selected_units <- renderPrint({
    list(
      mass_unit = input$mass_unit,
      volume_unit = input$volume_unit
    )
  })

  result_m <- c(10^3, 1, 10^-3, 10^-6, 10^-9)
  m <- reactive(input$mass_unit)
  M_factor1 <- reactive(result_m[as.numeric(m())])
  
  result_v <- c(1, 10^-3, 10^-6)
  v <- reactive(input$volume_unit)
  M_factor2 <- reactive(result_v[as.numeric(v())])
  
  output$M_result <- renderUI({
    M_temp <- as.numeric(M_mass()) * as.numeric(M_factor1()) / (as.numeric(M_weight()) * as.numeric(M_volume()) * as.numeric(M_factor2()))
    
    M_result <- if(is.na(M_temp)) {NA
    } else if (M_temp >= 1) {
      paste(M_temp, "M")
    } else if (M_temp >= 0.001) {
      paste(M_temp * 1000, "mM")
    } else if (M_temp >= 10^(-6)) {
      paste(M_temp * 10^6, "µM")
    } else if (M_temp >= 10^(-9)) {
      paste(M_temp * 10^9, "nM")
    } else {"too low"}
    
    
    withMathJax(
      helpText(paste("Molarity = $$", M_result, " M $$"))
    )
  })
}

# Running function (fix grammar)
runApp(shinyApp(ui, server))
