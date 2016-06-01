

library(shiny)
library(shinydashboard)
library(DT)
library(rhandsontable)
library(snowfall)
library(stringr)
library(stringi)
library(simarioV2)
library(plotly)

shinyServer(function(input, output, session) {
  
 
  textareaInput <- function(inputId, label, value="", placeholder="", rows=2){
    tagList(
      div(strong(label), style="margin-top: 5px;"),
      tags$style(type="text/css", "textarea {width:100%; margin-top: 5px;}"),
      tags$textarea(id = inputId, placeholder = placeholder, rows = rows, value))
  }
  
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
  transition_probabilities <<- initialSim$transition_probabilities
  
  
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
    
    #print(input$var_SB)
    
    results <- round(suppressWarnings(tableBuilder(env.base, 
             statistic = "frequencies", 
             dict = env.base$dict,
             variableName =  rev(names(which(trimws(varName) == trimws(input$var_SB))))[1], 
             grpbyName = "", CI = FALSE, 
             logisetexpr=NULL)), 4)
    
    if(!is.matrix(results))
      as.matrix(results)
    else
      as.data.frame(results)
    
  })
  
  output$previewSB  <- DT::renderDataTable({
    input$var_SB
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
  })
  
  # startSB <- eventReactive(input$actionSB, { return("Simulation is Started!")} )
  # 
  # output$processSB <- renderText( { return(startSB())})

  simulateSB <- eventReactive(input$actionSB, { 
    
    #env.scenario <- setGlobalSubgroupFilterExpression(env.scenario, input$subGrpFor_SB)
   
    sfInit(parallel=TRUE, cpus = 4, slaveOutfile = "test.txt" )

    sfExportAll()
    
    sfLibrary(Hmisc)
    sfLibrary(snowfall)
    sfLibrary(simarioV2)
    sfLibrary(stringr)
    
    env.scenario <<- simulatePShiny(env.scenario, input$nRun)
    
    sfStop()
   
    return("Simulation is Finished!")
  } )
  
  output$resultSB  <- renderPrint({

   
    changeSB()

    
    print( simulateSB())
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

    selectInput("subGrp_TB", "Select ByGroup:",
                choices = c(None='',  freqList), selectize=FALSE)
  })
  
  
  output$uiExprTB <- renderUI({
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    
    selectInput("subGrp_TB1", "Select Subgroup for subgroup formula:",
                choices = c(None='',  c(freqList,meansList)), selectize=FALSE)
  })
   
  output$uiExprTB1 <- renderUI({
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    
    print(names(env.base$dict$codings[[names(which(varName == input$subGrp_TB1))]]))
    
    choice <- names(env.base$dict$codings[[names(which(varName == input$subGrp_TB1))]])
    
    if(is.null(choice)){
      selectInput("subGrp_TB2", input$subGrp_TB1, 
                  choices = list("Equals" = "==",  "Less than" = "<", 
                                 "Greater than" = ">", "Less than or equal to" = "<=", 
                                 "Greater than or equal to" = ">=", "Not equals to " = "!="), 
                  selectize=FALSE)
      
    } else {
    selectInput("subGrp_TB2", input$subGrp_TB1,
          choices = choice, 
          selectize=FALSE)
    }
  })
  

  logisetexprTB <-eventReactive(input$andTB | input$orTB,{
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
  
    index = env.base$dict$codings[[names(which(varName == input$subGrp_TB1))]][
      (names(env.base$dict$codings[[names(which(varName == input$subGrp_TB1))]])==input$subGrp_TB2)]

    print(index)
    if(is.null(index)){
      paste(names(which(varName == input$subGrp_TB1)), input$subGrp_TB2, sep = " ")    
    }else{
      paste(names(which(varName == input$subGrp_TB1)), index, sep = "==")
    }

  })
  
  
  finalFormula <<- NULL
  
  orlogisetexprTB <-
    eventReactive(input$orTB , finalFormula <<- paste(finalFormula, logisetexprTB(), " | "))  
  
  andlogisetexprTB <-
    eventReactive(input$andTB , finalFormula <<- paste(finalFormula, logisetexprTB(), " & ") )  
  
  
  output$uilogisetexprTB <- renderUI({

    final <- 
    isolate({ 
      finalFormula <<- NULL
      
      orlogisetexprTB()
      andlogisetexprTB()
      
      return(finalFormula)
    })
    
    textareaInput("logisetexprTB",  "Subgroup formula:", value = final)
    
  })
  
  
   
  # output$uilogisetexpr_TB < renderUI({
  #   # Depending on input$input_type, we'll generate a different
  #   # UI component and send it to the client.
  # 

  # })
  
  baseTB <<- NULL 
  
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
                 grpbyName = rev(grpbyName)[1], CI = input$ci, 
                 logisetexpr=NULL)), 4)
      
    baseTB <- 
      if(!is.matrix(results)){
        as.matrix(results)
      }else{
        as.data.frame(results)
      } 
    
    baseTB<<- baseTB
    
    baseTB
  })
  
  
  output$resultTB  <- DT::renderDataTable({
    
    input$actionTB
    isolate( 
      summaryOutputTB()
    )
    
  }, rownames = TRUE, options = list(pageLength = 21))
  
  
  SBTB <<- NULL
  
  summaryOutputSBTB <- reactive({ 
    
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
    
    SBTB <-   
      if(!is.matrix(results)){
        as.matrix(results)
      }else{
        as.data.frame(results)
      } 
      
    SBTB <<-SBTB
    
    SBTB
  })
  
 
  
  output$resultSBTB  <- DT::renderDataTable({
    
    input$actionTB
    isolate( 
      summaryOutputSBTB()
    )
    
  }, rownames = TRUE, options = list(pageLength = 21))
  
  limitsGGplot <<- dodge <<- NULL
  
  combineResults <- reactive({
    
    baseTB <-summaryOutputTB()
   
    SBTB <- try(summaryOutputSBTB(), silent = TRUE)
    if(class(SBTB) == "try-error") SBTB <- NULL
    
    colname <- names(baseTB)
    
    if(any(grepl("Lower", colname))){
      start <- seq(1,ncol(baseTB), 3)
      
      tables.list <- vector(length(start), mode = "list")
      index <- 1
      for(i in start){
        tables.list[[index]] <- na.omit(data.frame(Year = 1:nrow(baseTB), 
                                                   Scenario = "Base", 
                                                   baseTB[,i:(i+2)]))
        if(!is.null(SBTB))
          tables.list[[index]] <- rbind(tables.list[[index]],
                                        na.omit(data.frame(Year = 1:nrow(SBTB), 
                                                           Scenario = "Scenario", 
                                                           SBTB[,i:(i+2)])))  
        
        names(tables.list[[index]]) = c("Year", "Scenario", "Percentage", "Lower", "Upper")
        names(tables.list)[index] <- colname[i]
        
        index <- index+1
      }
      
      limitsGGplot <<- aes(ymax = Upper, ymin=Lower)
      dodge <<- position_dodge(width=0.9)
      
    } else {
      tables.list <- vector(length(colname), mode = "list")
      index <- 1
      for(i in 1:length(colname)){
        tables.list[[index]] <-  na.omit(data.frame(Year = 1:nrow(baseTB), 
                                                    Scenario = "Base", 
                                                    baseTB[,i]))
        
        if(!is.null(SBTB))
          tables.list[[index]] <- rbind(tables.list[[index]],
                                        na.omit(data.frame(Year = 1:nrow(SBTB), 
                                                           Scenario = "Scenario", 
                                                           SBTB[,i])))  
        
        names(tables.list[[index]]) = c("Year", "Scenario","Percentage")
        names(tables.list)[index] <- colname[i]
        
        index <- index+1
      }
    }
    
    tables.list
  })
  
  
  output$barchart<- renderPlotly({
   
    tables.list <- combineResults()
    
    colname <- names(baseTB)
    
    p <- 
    ggplot(tables.list[[2]], aes(fill=Scenario, y = Percentage, x = Year)) + 
      ggtitle(names(tables.list)[2]) + 
      geom_bar(position="dodge", stat = "identity")
      
    if(any(grepl("Lower", colname)))
      p <- p + geom_errorbar(limitsGGplot, position=dodge, width=0.25)
    
    ggplotly(p)
  })
  
  
  output$linePlot<- renderPlotly({
    
 
    tables.list <- combineResults()
    
    colname <- names(baseTB)
    
    p <- 
      ggplot(tables.list[[2]], aes(fill=Scenario, y = Percentage, x = Year)) + 
      ggtitle(names(tables.list)[2]) + 
      geom_path()
    
    if(any(grepl("Lower", colname)))
      p <- p + geom_errorbar(limitsGGplot, position=dodge, width=0.25)
    
    ggplotly(p)
  })
  
})  