## Mini Project - Cell number Calculator
# Have to give ui, server
library(shiny)
library(bslib)
library(shinyWidgets)

ui <- page_fillable(
  titlePanel("Calculator"),           # 앱 제목
  layout_columns(
    column(6,
           card(card_header("4. Dilute a stock solution"),
                div(style = "display: flex; gap: 20px; font-size: 14px;",
                    tags$label("Stock concentration:", `for` = "D_s_concen"),
                    column(3,
                           div(numericInput("D_s_concen",
                                            label = NULL,
                                            min = 0,
                                            max = 10000,
                                            value = NA))),
                    
                    column(3,
                           div(pickerInput("D_s_concen_unit",
                                           label = NULL,
                                           choices = list("pM" = 6,
                                                          "nM" = 5, 
                                                          "uM" = 4, 
                                                          "mM" = 3, 
                                                          "M" = 2),
                                           selected = 2,
                                           options = list(container = "body"))))
                ),
                
                div(style = "display: flex; gap: 20px; font-size: 14px;",
                    tags$label("Desired concentration:", `for` = "D_d_concen"),
                    column(3,
                           div(numericInput("D_d_concen",
                                            label = NULL,
                                            min = 0,
                                            max = 10000,
                                            value = NA))),
                    
                    column(3,
                           div(pickerInput("D_d_concen_unit",
                                           label = NULL,
                                           choices = list("pM" = 6,
                                                          "nM" = 5, 
                                                          "uM" = 4, 
                                                          "mM" = 3, 
                                                          "M" = 2),
                                           selected = 2,
                                           options = list(container = "body"))))
                ),
                
                div(style = "display: flex; gap: 20px; font-size: 14px;",
                    tags$label("Desired volume:", `for` = "D_volume"),
                    column(3,
                           div(numericInput("D_volume",
                                            label = NULL,
                                            min = 0,
                                            max = 10000,
                                            value = NA))),
                    
                    column(3,
                           div(pickerInput("D_vol_unit",
                                           label = NULL,
                                           choices = list("uL" = 4, 
                                                          "mL" = 3, 
                                                          "L" = 2),
                                           selected = 2,
                                           options = list(container = "body"))))
                ),
                
                uiOutput("D_result")
           )
    ))
)

server <- function(input, output) {
  
  #unit
  unit_vec <- reactiveVal(c(1000, 1, 10^(-3), 10^(-6), 10^(-9), 10^(-12)))
  
  #W
  D_s_concen <- reactive({input$D_s_concen})
  D_s_concen_unit <- reactive({input$ D_s_concen_unit})
  D_d_concen <- reactive({input$D_d_concen})
  D_d_concen_unit <- reactive({input$ D_d_concen_unit})
  D_volume <- reactive({input$D_volume})
  D_vol_unit <- reactive({input$D_vol_unit})
  
  output$D_result <- renderUI({
    temp_result <- D_d_concen() * D_volume() / D_s_concen()
    unit_vec <- unit_vec()
    
    SI_unit <- if(!is.na(temp_result)) {
      unit_vec[as.numeric(D_s_concen_unit())]*unit_vec[as.numeric(D_vol_unit())]
    } else {""}
    
    SI <- if(SI_unit >= 1) {"L"} 
    else if (SI_unit >= 0.001) {"mL"}
    else if (SI_unit >=10^(-6)) {"uL"}
    else if (SI_unit >=10^(-9)) ("nL")
    else if (SI_unit >= 10^(-12)) ("pL")
    else ("too low")
    
    
    D_result <- if(!is.na(temp_result)) {temp_result} else if (SI =="too low") {""} else ("")
    
    
    
    span("Required volume = ", style = "font-size: 18px; color:#FF8339; font-weight:bold;", 
         span(paste0(D_result, SI), style = "font-size: 18px; color:#000000; font-weight: normal"))
  })
}

# Running function (fix grammar)
runApp(shinyApp(ui, server))
