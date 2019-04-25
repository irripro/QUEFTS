#
# QUEFTS Shiny App

library(shiny)
library(shinyjs)
library(shinydashboard)

library(raster)
library(magrittr)


ui = dashboardPage(
  
  dashboardHeader(title = 'QUEFTS MODEL', titleWidth = 400),
  
  dashboardSidebar(
    
    useShinyjs(),
    width = 400,
    sidebarMenu(
      
      # The Inputs to the Model
      h2("INPUTS"),
      
      div( style = "width:400px",
           h4("1 Fertilizer Information"),
           
           checkboxGroupInput(inputId = 'in21', label = '1.1 Fertilizer Mass Fraction (N,P,K)',
                              choices = list("NPK (0.14, 0.061, 0.116)" = "NPK",
                                             "UREA (0.46, 0.0, 0.0)" = "UREA",
                                             "Fert 3 (.180, .209, 0)" = "Fert_3"
                              ),
                              #fert_list,
                              selected = "NPK (0.14, 0.061, 0.116)"
           ),
           
           shinyjs::hidden(
             div( style = "background-color: rgb(180, 180, 180);height:80px;width:300px", id = "NPK",
                  numericInput(inputId = 'in27', label = "NPK Quantity (KG)", value = 130, min = 0, max = NA, step = NA)
             )
           ),
           shinyjs::hidden(
             div( style = "background-color: rgb(180, 180, 180);height:80px;width:300px", id = "UREA",
                  numericInput(inputId = 'in28', label = "UREA Quantity (KG)", value = 10, min = 0, max = NA, step = NA)
             )
           ),
           shinyjs::hidden(
             div( style = "background-color: rgb(180, 180, 180);height:80px;width:300px", id = "Fert_3",
                  numericInput(inputId = 'in29', label = "Fert_3 Quantity (KG)", value = 10, min = 0, max = NA, step = NA)
             )
           )
      ),
      
      # numericInput(inputId = 'in310', label = "Total Fertilizer Cost", 
      #              value = 10, min = 0, max = NA, step = NA),
      #Farm Information
      div( style = "display:inline;background-color: rgb(120, 120, 120);color:White;width:400px", #;height:120px
           h4("2 Farm Information"),
           # numericInput(inputId = 'in11', label = "1.1 Farm Size (HA)", 
           #              value = 10, min = 0, max = 1000, step = NA)
           h5("Estimate yield in a location"),
           numericInput(inputId = 'in12', label = "Longitude",
                        value = 1650000, min = 1037375, max = 2247375, step = NA),
           numericInput(inputId = 'in13', label = "Latitude",
                        value = -1250000, min = -1841875, max = -650875, step = NA)
      ),
      actionButton(inputId = 'run_point', label = "Get Point Estimate"),
      
      
      # div( style = "color:White;width:400px", #;height:200px
      #      h4("3 Maize Information"),
      #      numericInput(inputId = 'in31', label = "3.1 Expected Income per KG", value = 20, min = 0, max = NA, step = NA)
      # ),
      
      # Auto hide advanced options unless needed
      a(id = "AdvancedOptions", "3 Advanced Options"),
      shinyjs::hidden(
        div( style = "width:400px",
             id = "advanced",
             numericInput(inputId = "in51", label = "4.1 Advanced Option 1", value = 30, min = 0, max = NA, step = NA),
             numericInput(inputId = "in52", label = "4.2 Advanced Option 2", value = 45, min = 0, max = NA, step = NA)
        )
      ),
      actionButton(inputId = 'run', label = "Get Country Estimate")
      
      
    )
  ),
  
  # SHOW input value in the meantime
  dashboardBody(
    fluidRow(
    #verbatimTextOutput('point_yield_est'),
    tabBox(
      # Title can include an icon
      title = tagList(shiny::icon("corn"), ""),
      tabPanel("Point Prediction",
               "Longitude:",
               verbatimTextOutput('long'),
               "Latitude:",
               verbatimTextOutput('lati'),
               'soil Carbon:',   
               verbatimTextOutput('soilC'),
               'soil Polsen:',
               verbatimTextOutput('soilPolsen'),
               'soil Pottasium:',
               verbatimTextOutput('soilK'),
               'soil pH',
               verbatimTextOutput('soilpH'),
               "Predicted Yield ",
               verbatimTextOutput('point_yield_est') 
               )
      ),
    # infoBoxOutput('infoBox1'),
    tabBox(
      # Title can include an icon
      title = tagList(shiny::icon("corn"), ""),
      tabPanel("Country Prediction",
               "",
               plotOutput("country_yield_est")
               )
      )
    )
    )
  )



server = function(input, output, session) {
  source('QUEFTS.R')
  source('QUEFTS_Point.R')
  
  source('nutrientUPT.R')
  source('nutrientUPT_Point.R')
  
  source('combine_yields.R')
  source('combine_yields_Point.R')
  
  source('load_rasters.R')
  
  co_ordinate <- reactiveValues()
  co_ordinate$long <- eventReactive( input$run_point, {input$in12})
  co_ordinate$lati <- eventReactive( input$run_point, {input$in13})
  co_ordinate$longlati <- eventReactive( input$run_point, {data.frame(co_ordinate$long(), co_ordinate$lati())})
  
  
  point <- reactiveValues() 
  point$soilC <- eventReactive( input$run_point, {raster::extract(soilC, co_ordinate$longlati())})
  point$soilPolsen <- eventReactive( input$run_point, {raster::extract(soilPolsen, co_ordinate$longlati())})
  point$soilK <- eventReactive( input$run_point, {raster::extract(soilK, co_ordinate$longlati())})
  point$soilpH <- eventReactive( input$run_point, {raster::extract(soilpH, co_ordinate$longlati())})
  
  fert_list <- list("NPK (0.14, 0.061, 0.116)" = "NPK",
                    "UREA (0.46, 0.0, 0.0)" = "UREA",
                    "Fert 3 (.180, .209, 0)" = "Fert_3"
  )
  
  # Show/Hide advanced options
  shinyjs::onclick("AdvancedOptions", toggle(id = "advanced", anim = TRUE))
  
  # Show/Hide Fertilizer Quantity options
  observe({ toggle(id = "NPK", condition = {"NPK" %in% input$in21} )})
  observe({ toggle(id = "UREA", condition = {"UREA" %in% input$in21} )})
  observe({ toggle(id = "Fert_3", condition = {"Fert_3" %in% input$in21} )})
  
  
  
  seeinputs <- eventReactive(input$run, {
    str(sapply(sprintf('in%d', c(11, 21:24, 27:29, 210, 31)), function(id) {
      input[[id]]
    }))
  })
  
  
  ##### Point Estimates ####
  run.pointmodel <- eventReactive(input$run_point, {
    # Inputs
    
    ##### Extract Point SoilNutrient Values ####
    siteSoilNutrient <- matrix(c(point$soilC(), point$soilPolsen(), point$soilK(), point$soilpH()), ncol = 4, byrow = TRUE)
    colnames(siteSoilNutrient) <- c('soilC', 'soilPolsen', 'soilK', 'soilpH')
    
    #### Calculate Point nutrients(kg/ha) Values ####
    fert_amtNPK <- fert_amtUREA <- fert_amtFert_3 <- c(0)
    if ("NPK" %in% input$in21){
      fert_amtNPK <- c(input$in27)
    }
    if ("UREA" %in% input$in21){
      fert_amtUREA <- c(input$in28)
    }
    if ("Fert_3" %in% input$in21){
      fert_amtFert_3 <- c(input$in29)
    }
    fert_amt <-  matrix(c(fert_amtNPK, fert_amtUREA, fert_amtFert_3), ncol= 3)
    #Mass fraction for NPK (0.14, 0.061, 0.116) and Urea (0.46, 0.0, 0.0)
    fert_massfrac <-  matrix(c(0.14, 0.061, 0.116, 0.46, 0.0, 0.0, .180, .209, 0), ncol = 3, byrow = TRUE) 
    nutrients_kg.ha <- fert_amt %*% t(fert_massfrac)
    
    ad <- matrix(c(26, 180, 24, 60, 540, 96)) #from Sattari 2014
    
    
    QUEFTS_point(siteSoilNutrient = siteSoilNutrient,
                 nutrients_kg.ha = nutrients_kg.ha,
                 ad = ad
                 )
     
    #siteSoilNutrient[,1]
    #colnames(siteSoilNutrient)
    #siteSoilNutrient[,'soilC']
    
  })

  
  ##### Country Estimates Model ####
  run.countrymodel <- eventReactive(input$run,{
    # Load the data
    rasters_input <-
      sapply(list(soilC, soilPolsen, soilK, soilpH, WY), getValues) %>% cbind(index =
                                                                                1:(nrow(.)))
    # # rasters_input <-
    # #   sapply(list(farm.soilC, farm.soilPolsen, farm.soilK, farm.soilpH, farm.WY), getValues) %>% cbind(index =
    # #                                                                                                      1:(nrow(.)))
    
    rasters_input <-
      rasters_input[complete.cases(rasters_input), ]  #compute only values with all data
    colnames(rasters_input) <-
      c('soilC', 'soilPolsen', 'soilK', 'soilpH', 'WY', 'index')
    
    fert_amtNPK <- fert_amtUREA <- fert_amtFert_3 <- c(0)
    fert_massfracNPK <- fert_massfracUREA <- fert_massfracFert_3 <- c(0,0,0)
    
    if ("NPK" %in% input$in21){
      fert_amtNPK <- c(input$in27)
      fert_massfracNPK <- c(0.14, 0.061, 0.116)
      
    }
    if ("UREA" %in% input$in21){
      fert_amtUREA <- c(input$in28)
      fert_massfracUREA <- c(0.46, 0.0, 0.0)
      
    }
    if ("Fert_3" %in% input$in21){
      fert_amtFert_3 <- c(input$in29)
      fert_massfracFert_3 <- c(.180, .209, 0)
      
    }
    
    
    nutrients_kg.ha <- fert_amtNPK * fert_massfracNPK +
      fert_amtUREA * fert_massfracUREA  +
      fert_amtFert_3 * fert_massfracFert_3
    
    yields <-
      apply(
        rasters_input,
        FUN = QUEFTS,
        MARGIN = 1,
        nutrients_kg.ha = nutrients_kg.ha
      )
    
    # Plot the results - after converting to raster
    results  <- soilC
    results[] <- NA
    results[rasters_input[, 'index']] <- yields
    
    plot(TZA_boundary, main = "Predicted Yield")
    plot(results, add=TRUE)
    # Add Map Scale
    #maps::map.scale(x=1100000, y=-1700000, ratio=FALSE, metric = TRUE) 
    # Add North arrow
    GISTools::north.arrow(xb=2100000, yb=-800000, len=50000, lab="N", col='black')
    
  })
  
  ##### Outputs ####
  output$country_yield_est <- renderPlot({run.countrymodel()})
  
  output$long <- renderText({co_ordinate$long()})
  output$lati <- renderText({co_ordinate$lati()})
  
  output$soilC <- renderText({point$soilC()})
  output$soilPolsen <- renderText({point$soilPolsen()})
  output$soilK <- renderText({point$soilK()})
  output$soilpH <- renderText({point$soilpH()})
  output$point_yield_est <- renderText({run.pointmodel()})
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

