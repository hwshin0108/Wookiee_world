## Mini project3 - Decorating the app interface
# put in a "Card"
library(shiny)
library(bslib)
library(shinyWidgets)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$style(HTML("
  .selectize-input, .selectize-dropdown-content {
                  font-size: 12px;
                  }"
                  )
             ),
  
  layout_columns(col_widths = c(4, 4, 4, 6, 6),
                 row_heights = c(1, 1),
                 card(
                   style = "font-size: 14px;",
                   card_header("1. Molarity from mass & volume"),
                   card_body(
                     fluidRow(
                       column(6, numericInput("M_mass",                
                                              "Mass:",
                                              min = 0,
                                              max = 10000,
                                              value = NA)
                       ),
                       column(6, selectizeInput("mass_unit", "unit:",
                                                choices = list("kg" = 1,
                                                               "g" = 2,
                                                               "mg" = 3,
                                                               "µg" = 4,
                                                               "ng" = 5),
                                                multiple = FALSE)
                       )
                     ),
                     
                     numericInput("M_weight",                
                                  "Formula weight(Dalton):",
                                  min = 0,
                                  max = 10000,
                                  value = NA),
                     fluidRow(
                       column(6, numericInput("M_volume",                
                                              "Volume: ",
                                              min = 0,
                                              max = 10000,
                                              value = NA)
                       ),
                       column(6, selectizeInput("volume_unit", "unit:",
                                                choices = list("L" = 1,
                                                               "mliter" = 2,
                                                               "µliter" = 3),
                                                multiple = FALSE)
                       )
                     )
                   ),
                   card_footer(withMathJax(),
                               uiOutput("M_result")          
                   )
                   ),
#####################
                     card(
                       style = "font-size: 14px;",
                       card_header("2. Volume from mass & concentration"),
                       card_body(
                         fluidRow(
                           column(6, numericInput("V_mass",                
                                                  "Mass",
                                                  min = 0,
                                                  max = 10000,
                                                  value = NA)
                                  ),
                           column(6, selectizeInput("mass_unit", "unit:",
                                                    choices = list("kg" = 1,
                                                                   "g" = 2,
                                                                   "mg" = 3,
                                                                   "µg" = 4,
                                                                   "ng" = 5),
                                                    multiple = FALSE)
                                  )
                           ),
                           
                         numericInput("V_weight",                
                                      "Formula weight(Dalton):",
                                      min = 0,
                                      max = 10000,
                                      value = NA),
                           fluidRow(
                             column(6, numericInput("V_concentration",               
                                                    "Concentration",
                                                    min = 0,
                                                    max = 10000,
                                                    value = NA)
                                    ),
                             column(6, selectizeInput("concentration_unit", "unit:",
                                                      choices = list("M" = 1,
                                                                     "mM" = 2,
                                                                     "µM" = 3,
                                                                     "nM" = 4,
                                                                     "pM" = 5),
                                                      multiple = FALSE)
                                    )
                           )
                         ),
                       card_footer(withMathJax(),
                                   uiOutput("V_result")          
                                   )
                       ),
#####################
                     card(style = "font-size: 14px;",
                         card_header("3. Mass from volume & concentration"),
                         card_body(
                           fluidRow(
                             column(6, numericInput("W_concen", "Concentration",
                                                    min = 0,
                                                    max = 10000,
                                                    value = NA)
                             ),
                             
                             column(6, selectizeInput("W_concen_unit", "unit:",
                                                      choices = list("M" = 1,
                                                                     "mM" = 2, 
                                                                     "µM" = 3, 
                                                                     "nM" = 4, 
                                                                     "pM" = 5),
                                                      multiple = F)
                             )
                           ),
                           
                           numericInput("W_weight",                
                                        "Formula weight(Dalton):",
                                        min = 0,
                                        max = 10000,
                                        value = NA),
                           
                           fluidRow(
                             column(6, numericInput("W_volume", "Volume",
                                                    min = 0,
                                                    max = 10000,
                                                    value = NA)
                             ),
                             
                             column(6, selectizeInput("W_vol_unit", "unit:",
                                                      choices = list("µL" = 3, 
                                                                     "mL" = 2, 
                                                                     "L" = 1),
                                                      multiple = F)
                             )
                           )
                         ), card_footer(withMathJax(),
                                        uiOutput("W_result"))
                    ),

#####################
                     card(style = "font-size: 14px;",
                         card_header("4. Dilute a stock solution"),
                         card_body(
                           fluidRow(
                             column(6,numericInput("D_s_concen", "Stock concentration",
                                                   min = 0,
                                                   max = 10000,
                                                   value = NA)),
                             
                             column(6, selectizeInput("D_s_concen_unit", "unit",
                                                      choices = list("M" = 1,
                                                                     "mM" = 2, 
                                                                     "µM" = 3, 
                                                                     "nM" = 4, 
                                                                     "pM" = 5),
                                                      multiple = F)
                             )
                           ),
                           
                           fluidRow(
                             column(6, numericInput("D_d_concen", "Desired concentration",
                                                    min = 0,
                                                    max = 10000,
                                                    value = NA)
                             ),
                             
                             column(6, selectizeInput("D_d_concen_unit", "unit",
                                                      choices = list("M" = 1,
                                                                     "mM" = 2, 
                                                                     "µM" = 3, 
                                                                     "nM" = 4, 
                                                                     "pM" = 5),
                                                      multiple = F)
                             ),
                           ),
                           
                           fluidRow(
                             column(6, numericInput("D_volume", "Desired Volume",
                                                    min = 0,
                                                    max = 10000,
                                                    value = NA)),
                             
                             column(6, selectizeInput("D_vol_unit", "unit",
                                                      choices = list("µL" = 3, 
                                                                     "mL" = 2, 
                                                                     "L" = 1),
                                                      multiple = F)
                             )
                           )
                         ), card_footer(withMathJax(),
                                        uiOutput("D_result"))
                        ),
#####################
                     card(style = "font-size: 14px;",
                         card_header("5. Cell count"),
                         card_body(
                           fluidRow(
                             column(6, numericInput("C_num",                
                                                    "Number of cells (20-200) :",
                                                    min = 20,
                                                    max = 200,
                                                    value = NA)
                             ),
                             column(6, numericInput("C_dilute",           
                                                    "Dilution factor:",
                                                    min = 1,
                                                    max = 10,
                                                    value = NA)
                             )
                           )
                         ),
                         card_footer(withMathJax(),
                                     uiOutput("C_total")
                         )
                         
                    )
                   ),
h6("Hong Lab | All Rights Reserverd | HW & SS",
   style = "text-align: center; color: #CCCCCC;")
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
      helpText(paste("Molarity = $$", M_result, "$$"))
    )
  })

##############################################################################    
  
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
  V_c <- reactive(input$concentration_unit)
  V_factor2 <- reactive(result_c[as.numeric(V_c())])
  
  output$V_result <- renderUI({
    V_temp <- as.numeric(V_mass()) * as.numeric(V_factor1()) / (as.numeric(V_weight()) * as.numeric(V_concentration()) * as.numeric(V_factor2()))
    
    V_result <- if(is.na(V_temp)) {NA
    } else if (V_temp >= 1) {
      paste(V_temp, "L")
    } else if (V_temp >= 0.001) {
      paste(V_temp * 1000, "mL")
    } else if (V_temp >= 10^(-6)) {
      paste(V_temp * 10^6, "µL")
    } else {"too low"}
    
    withMathJax(
      helpText(paste("Volume = $$", V_result, "$$"))
    )
  })
  
##############################################################################
  #W
  W_concen <- reactive({input$W_concen})
  W_weight <- reactive({input$W_weight})
  W_volume <- reactive({input$W_volume})
  
  output$selected_units <- renderPrint({
    list(
      concen_unit = input$W_concen_unit,
      vol_unit = input$W_vol_unit
    )
  })
  
  result_concen<- c(1, 10^-3, 10^-6, 10^-9, 10^-12)
  w_con <- reactive(input$W_concen_unit)
  W_factor1 <- reactive(result_concen[as.numeric(w_con())])
  
  result_v <- c(1, 10^-3, 10^-6)
  w_vol <- reactive(input$W_vol_unit)
  W_factor2 <- reactive(result_v[as.numeric(w_vol())])
  
  output$W_result <- renderUI({
    W_temp <- as.numeric(W_concen()) * as.numeric(W_factor1()) * (as.numeric(W_weight()) * as.numeric(W_volume()) * as.numeric(W_factor2()))
    
    W_result <- if(is.na(W_temp)) {NA
    } else if (W_temp >= 1) {
      paste(W_temp, "g")
    } else if (W_temp >= 0.001) {
      paste(W_temp * 1000, "mg")
    } else if (W_temp >= 10^(-6)) {
      paste(W_temp * 10^6, "µg")
    } else if (W_temp >= 10^(-9)) {
      paste(W_temp * 10^9, "ng")
    } else if (W_temp >= 10^(-12)) {
      paste(W_temp * 1e-12, "pg")
    } else {"too low"}
    withMathJax(
      helpText(paste("Mass = $$", W_result, "$$"))
    )
  })
  
##############################################################################  
  #D
  D_s_concen <- reactive({input$D_s_concen})
  D_d_concen <- reactive({input$D_d_concen})
  D_volume <- reactive({input$D_volume})
  
  output$selected_units <- renderPrint({
    list(
      s_concen_unit = input$D_s_concen_unit,
      d_concen_unit = input$D_d_concen_unit,
      D_vol_unit = input$D_vol_unit
    )
  })
  
  unit_concen<- c(1, 10^-3, 10^-6, 10^-9, 10^-12)
  s_con <- reactive({input$D_s_concen_unit})
  d_con <- reactive({input$D_d_concen_unit})
  s_factor <- reactive(unit_concen[as.numeric(s_con())])
  d_factor <- reactive(unit_concen[as.numeric(d_con())])
  
  vol_concen <- c(1, 10^-3, 10^-6)
  vol <- reactive({input$D_vol_unit})
  vol_factor <- reactive({vol_concen[as.numeric(vol())]})
  
  output$D_result <- renderUI({
    D_temp <- as.numeric(D_d_concen()) * as.numeric(D_volume()) * as.numeric(d_factor()) * as.numeric(vol_factor()) / as.numeric(D_s_concen()) * as.numeric(s_factor())
    
    D_result <- if (is.na(D_temp)) {NA
      } else if (D_temp >= 1) {
      paste(D_temp, "L") 
      } else if (D_temp >= 0.001) {
      paste(D_temp * 1000, "mL")
      } else if (D_temp >= 10^(-6)) {
      paste(D_temp * 1e6, "uL")
      } else if (D_temp >=10^(-9)) {
      paste(D_temp * 1e9, "nL")
      } else {"too low"}
    
    withMathJax(
      helpText(paste("Required volume = $$", D_result, "$$"))
    )
  })  
    
##############################################################################
  C_num <- reactive({input$C_num})
  C_dilute <- reactive({input$C_dilute})
  
  output$C_total <- renderUI({
    C_total <- as.numeric(C_num()) * 10^4 * 10^(as.numeric(C_dilute()))
    
    C_ten <- nchar(as.character(format(C_total, scientific = F))) - 1
    C_final <- C_total/(10^(C_ten))
    
    C_7 <- 1e7 / C_total * 1000
    
    withMathJax(
      helpText(paste("Total Cell number = $$", C_final, "x 10^", C_ten, "$$ \n",
                     "For 1x10^7 = $$", C_7, " µL $$"))
    )
  })
  
}

# Running function (fix grammar)
shinyApp(ui = ui, server = server)