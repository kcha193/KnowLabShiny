

library(shiny)
library(shinydashboard)
library(DT)
library(rhandsontable)
library(snowfall)
library(stringr)
library(stringi)
library(simarioV2)


shinyServer(function(input, output, session) {
  
 
  source("SimmoduleMELC1_21.R")
  
  initialSim <<- readRDS("base/initialSim.rds")
  env.base  <<- readRDS("base/FullBaseRun.rds")
  
  NUM_ITERATIONS <<- 21
  dict <<- initialSim$dict
  limits <<- initialSim$limits
  binbreaks <<- initialSim$binbreaks
  catToContModels <<- initialSim$catToContModels
  models <<- initialSim$models
  PropensityModels <<- initialSim$PropensityModels
  children <<- initialSim$children
 
  
  #First page of the website  
  varName = env.base$dict$descriptions 
  
  freqList1 = sort(as.character(varName[names(env.base$modules[[1]]$run_results_collated[["freqs"]])]))
  freqListByGroup1 = sort(as.character(varName[names(env.base$modules[[1]]$run_results_collated[["freqs_continuousGrouped"]])]))
  meansList1 = sort(as.character(varName[names(env.base$modules[[1]]$run_results_collated[["means"]])]))
  quantilesList1 =   sort(as.character(varName[names(env.base$modules[[1]]$run_results_collated[["quantiles"]])]))
  
  output$ui <- renderUI({
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "Percentage" = selectInput("dynamic", "Variable",freqList1),
           "Percentage - continous grouped" = selectInput("dynamic", "Variable",freqListByGroup1),
           "Means" = selectInput("dynamic", "Variable", meansList1),
           "Quantiles" = selectInput("dynamic", "Variable", quantilesList1)
    )
  })
  
  
  summaryOutput <- reactive({ 
    
    inputType = c("freqs", "freqs_continuousGrouped", "means", "quantiles")
    
    names(inputType) = c("Percentage", "Percentage - continous grouped", "Means","Quantiles" )
    
    varName = switch(input$input_type,
                     "Percentage" = varName[names(env.base$modules[[1]]$run_results_collated[["freqs"]])],
                     "Percentage - continous grouped" = varName[names(env.base$modules[[1]]$run_results_collated[["freqs_continuousGrouped"]])],
                     "Means" = varName[names(env.base$modules[[1]]$run_results_collated[["means"]])],
                     "Quantiles" = varName[names(env.base$modules[[1]]$run_results_collated[["quantiles"]])])
    
  
    results <- env.base$modules[[1]]$run_results_collated[[
      inputType[input$input_type]]][[names(which(trimws(varName) == trimws(input$dynamic)))]]
    
    if(is.numeric(results))
      as.data.frame(results)
    else 
      results
    
  })
  
  output$result  <- DT::renderDataTable({
    input$previewBS
    isolate( 
      summaryOutput()
    )   
  }, rownames = TRUE, options = list(pageLength = 21))
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(input$input_type, '.csv', sep='') 
    },
    content = function(file) {
      write.csv(summaryOutput(), file)
    }
  )
  
  
  #################################################################################################################
  #Scenario Builder
  
  env.scenario  <<- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")
  varName_SB = sort(as.character(varName[names(env.base$cat.adjustments)]))
  varName_SB = varName_SB[!is.na(varName_SB)]
  
  newSB <- function(){
    
    env.scenario <<- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")
    
  }
  
  
  output$uiSB <- renderUI({   
    selectInput("var_SB", "Select Variable to Examine",
                choices = varName_SB, selectize=FALSE)    
  })
  
  
  baseOutputSB <- reactive({ 
    
    print(names(which(trimws(varName) == trimws(input$var_SB))))
    
    results <- round(suppressWarnings(tableBuilder(envName = env.base, 
             statistic = "frequencies", 
             dict= env.base$dict,
             variableName =  names(which(trimws(varName_SB) == trimws(input$var_SB)))[1], 
             grpbyName = "", CI = FALSE, 
             logisetexpr=NULL)), 4)
    
    if(!is.matrix(results))
      as.matrix(results)
    else
      as.data.frame(results)
    
  })
  
  output$previewSB  <- renderTable({
    input$actionPreviewSB
    isolate( 
      baseOutputSB()
    )
  })
  
  # hotable
  output$hotable <- renderRHandsontable({
    catAdj = env.base$cat.adjustments[[ as.character(names(which(varName == input$var_SB)))]]
    
    tbl <-
    if(nrow(catAdj) == 1){	
      temp = cbind(Rowname = colnames(catAdj),  as.data.frame(apply(catAdj,2, as.numeric)))
      colnames(temp) <- c("Level", "Year 1")
      
      temp
    }else {
      cbind(Rowname = rownames(catAdj), as.data.frame(apply(catAdj,2, as.numeric), stringsAsFactors = FALSE))
      
    }
    rhandsontable(tbl, readOnly = FALSE) 
  })
  
  
  changeSB <- eventReactive( input$actionAddSB, {
    
    catAdj <-t(t(hot_to_r(input$hotable)))[,-1] 
    
    if(is.matrix(catAdj)){
      catAdj = apply(catAdj, 2, as.numeric)
      
      for(i in 1:nrow(catAdj))
        env.scenario$cat.adjustments[[
          as.character(names(which(varName == input$var_SB)))]][i,] <<- catAdj[i,]		   
      
    } else {
      env.scenario$cat.adjustments[[
        as.character(names(which(varName == input$var_SB)))]][1,] <<- as.numeric(catAdj)	
    }   
    
    print(catAdj)
    
  })
  
  simulateSB <- eventReactive(input$actionSB, { 

    sfInit(parallel=TRUE, cpus = 5, slaveOutfile = "test.txt" )

    sfExportAll()
    
    sfLibrary(Hmisc)
    sfLibrary(snowfall)
    sfLibrary(simarioV2)
    sfLibrary(stringr)
    
    env.scenario <<- simulatePShiny(env.scenario, input$nRun)
    
    sfStop()
    
    return("Done!!")
  } )
  
  output$resultSB  <- renderText({
   
    #setGlobalSubgroupFilterExpression(input$subGrpFor_SB)
    changeSB()
    
    out <- "Working!"
    
    out <-simulateSB()

    return(out)  
  })  
  
#######################################################################################################
#Second Page: Table builder
  catvars <- c(getOutcomeVars(initialSim$simframe, "categorical"), "SESBTH", "r1stchildethn", 
               "r1stmeduc", "r1stfeduc", "z1single0", "fage", 
               "z1single0Lvl1", "bthorder", "NPRESCH", "z1genderLvl1")
  contvars <- c(getOutcomeVars(initialSim$simframe, "continuous"), "bwkg", "pregalc", 
                "ga", "INTERACT", "PUNISH", "MAGE", "pregsmk")
  
  freqList = sort(as.character(varName[catvars]))
  meansList =  quantilesList =  sort(as.character(varName[contvars]))
  
  
  output$uiTB <- renderUI({
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type_TB,
           "Percentage" = selectInput("dynamicTB", "Variable",choices = freqList),
           "Means" = selectInput("dynamicTB", "Variable", choices = meansList),
           "Quantiles" = selectInput("dynamicTB", "Variable", choices = quantilesList)
    )
  })
  
  output$uiSubGrpTB <- renderUI({
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    selectInput("subGrp_TB", "Select Subgroup:",choices = c(None='', freqList), selectize=FALSE)
  })
  
  summaryOutputTB <- reactive({ #print(getwd())
    
    print(names(which(varName == input$subGrp_TB)))
    
    inputType = c("frequencies", "means", "quintiles")
    
    names(inputType) = c("Percentage", "Means","Quantiles" )
    
    grpbyName = names(which(varName == input$subGrp_TB))
    
    if(length(grpbyName) == 0) grpbyName = ""
    

      results <- round(suppressWarnings(tableBuilder(env.base, 
                   statistic = inputType[input$input_type_TB], 
                   dict= env.base$dict,
                   variableName = rev(names(which(trimws(varName) == trimws(input$dynamicTB))))[1], 
                   grpbyName = grpbyName[1], CI = input$ci, 
                   logisetexpr=NULL)), 4)

    
    if(!is.matrix(results))
      as.matrix(results)
    else
      as.data.frame(results)
  })
  
  output$resultTB  <- DT::renderDataTable({
    
    input$actionTB
    isolate( 
      summaryOutputTB()
    )
    
  }, rownames = TRUE, options = list(pageLength = 21))
  
  summaryOutputSBTB <- reactive({ #print(getwd())
    
    print(names(which(varName == input$subGrp_TB)))
    
    inputType = c("frequencies", "means", "quintiles")
    
    names(inputType) = c("Percentage", "Means","Quantiles" )
    
    grpbyName = names(which(varName == input$subGrp_TB))
    
    if(length(grpbyName) == 0) grpbyName = ""

      results <- round(suppressWarnings(tableBuilder(env.scenario, 
           statistic = inputType[input$input_type_TB], 
           dict= env.base$dict,
           variableName = rev(names(which(trimws(varName) == trimws(input$dynamicTB))))[1], 
           grpbyName = grpbyName[1], CI = input$ci, 
           logisetexpr=NULL)), 4)

    
    
    if(!is.matrix(results))
      as.matrix(results)
    else
      as.data.frame(results)
  })
  
  output$resultSBTB  <- DT::renderDataTable({
    
    input$scenario
    isolate( 
      summaryOutputSBTB()
    )
    
  }, rownames = TRUE, options = list(pageLength = 21))

})  