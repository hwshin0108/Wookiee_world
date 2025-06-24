## Mini Project - Cell number Calculator
# Have to give ui, server
library(shiny)
library(bslib)
library(shinyWidgets)

ui <- page_fillable(
    titlePanel("Calculator"),           # 앱 제목
    layout_columns(
      column(6,
          card(card_header("1. Mass from volume & concentration"),
            div(style = "display: flex; gap: 20px;",
            tags$label("Concentration:", `for` = "W_concen"),
            column(4,
            div(numericInput("W_concen",
                            label = NULL,
                            min = 0,
                            max = 10000,
                            value = NA))),
            
            column(3,
            div(pickerInput("W_concen_unit",
                       label = NULL,
                       choices = list("pM" = 6,
                                      "nM" = 5, 
                                      "uM" = 4, 
                                      "mM" = 3, 
                                      "M" = 2),
                       selected = 2,
                       options = list(container = "body"))))
            ),
            
            div(style = "display: flex; gap: 20px;",
                tags$label("Formula Weight (daltons):", `for` = "W_weight", style = "white-space: nowrap;"),
                column(10,
                       div(numericInput("W_weight",
                                        label = NULL,
                                        min = 0,
                                        max = 10000,
                                        value = NA,
                                        width = "50%")))
            ),
            
            div(style = "display: flex; gap: 20px;",
                tags$label("Volume:", `for` = "W_volume"),
                column(4,
                       div(numericInput("W_volume",
                                        label = NULL,
                                        min = 0,
                                        max = 10000,
                                        value = NA))),
                
                column(3,
                       div(pickerInput("W_vol_unit",
                                       label = NULL,
                                       choices = list("uL" = 4, 
                                                      "mL" = 3, 
                                                      "L" = 2),
                                       selected = 2,
                                       options = list(container = "body"))))
            ),
            
            uiOutput("W_result")
      )
  ))
)

server <- function(input, output) {
  
  #unit
  unit_vec <- reactiveVal(c(1000, 1, 10^(-3), 10^(-6), 10^(-9), 10^(-12)))
  
  #W
  W_concen <- reactive({input$W_concen})
  W_concen_unit <- reactive({input$W_concen_unit})
  W_weight <- reactive({input$W_weight})
  W_volume <- reactive({input$W_volume})
  W_vol_unit <- reactive({input$W_vol_unit})
  
  output$W_result <- renderUI({
    temp_result <- W_concen() * W_weight() * W_volume()
    unit_vec <- unit_vec()

    SI_unit <- if(!is.na(temp_result)) {
      unit_vec[as.numeric(W_concen_unit())]*unit_vec[as.numeric(W_vol_unit())]
    } else {""}
    
    SI <- if(SI_unit >= 1) {"g"} 
    else if (SI_unit >= 0.001) {"mg"}
    else if (SI_unit >=10^(-6)) {"ug"}
    else if (SI_unit >=10^(-9)) ("ng")
    else if (SI_unit >= 10^(-12)) ("pg")
    else ("too low")
    
    W_result <- if(!is.na(temp_result)) {temp_result} else if (SI =="too low") {""} else ("")
    
    
    
    span("Mass = ", style = "font-size: 18px; color:#FF8339; font-weight:bold;", 
         span(paste0(W_result, SI), style = "font-size: 18px; color:#000000; font-weight: normal"))
  })
}

# Running function (fix grammar)
runApp(shinyApp(ui, server))
