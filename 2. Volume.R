## Mini project2 - Molecular calculator_Volume

library(shiny)

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
      
      selectizeInput("concentration_unit", "Concentration units:",
                     choices = list("Molar" = 1,
                                    "milimolar" = 2,
                                    "micromolar" = 3,
                                    "nanomolar" = 4,
                                    "picomolar" = 5),
                     multiple = FALSE),
      
      numericInput("V_mass",                # 입력 위젯 (name tag)
                   "Mass:",
                   min = 0,
                   max = 10000,
                   value = NA),
      
      numericInput("V_weight",                # 입력 위젯 (name tag)
                   "Formula weight(Dalton):",
                   min = 0,
                   max = 10000,
                   value = NA),
      
      numericInput("V_concentration",                # 입력 위젯 (name tag)
                   "Concentration: ",
                   min = 0,
                   max = 10000,
                   value = NA),
      
      withMathJax(),
      uiOutput("V_result")          # 출력 영역
    ),
    mainPanel()
  )
)
server <- function(input, output, session) {
  
  V_mass <- reactive({input$V_mass})
  V_weight <- reactive({input$V_weight})
  V_concentration <- reactive({input$V_concentration})
  
  output$selected_units <- renderPrint({
    list(
      mass_unit = input$mass_unit,
      concentration_unit = input$concentration_unit
    )
  })
  
  result_m <- c(10^3, 1, 10^-3, 10^-6, 10^-9)
  m <- reactive(input$mass_unit)
  V_factor1 <- reactive(result_m[as.numeric(m())])
  
  result_c <- c(1, 10^-3, 10^-6, 10^-9, 10^-12)
  c <- reactive(input$concentration_unit)
  V_factor2 <- reactive(result_c[as.numeric(c())])
  
  output$V_result <- renderUI({
    temp <- as.numeric(V_mass()) * as.numeric(V_factor1()) / (as.numeric(V_weight()) * as.numeric(V_concentration()) * as.numeric(V_factor2()))
    
    V_result <- if(is.na(temp)) {NA
    } else if (temp >= 1) {
      paste(temp, "L")
    } else if (temp >= 0.001) {
        paste(temp * 1000, "mL")
    } else if (temp >= 10^(-6)) {
        paste(temp * 10^6, "µL")
    } else {"too low"}
    
    withMathJax(
      helpText(paste("Volume = $$", V_result, "$$"))
    )
  })
}

# Running function (fix grammar)
runApp(shinyApp(ui, server))
