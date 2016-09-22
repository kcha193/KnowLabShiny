

#devtools::install_github("kcha193/simarioV2")

library(shiny)
library(shinydashboard)
library(DT)
library(rhandsontable)
library(simarioV2)
library(plotly)
library(dplyr)
library(tidyr)
library(openxlsx)
library(reshape2)
library(visNetwork)

shinyServer(function(input, output, session) {
  
  textareaInput <- function(inputId, label, value="", placeholder="", rows=2){
    tagList(
      div(strong(label), style="margin-top: 5px;"),
      tags$style(type="text/css", "textarea {width:100%; margin-top: 5px;}"),
      tags$textarea(id = inputId, placeholder = placeholder, rows = rows, value))
  }
  
  
  # dataModal <- function(failed = FALSE) {
  #   modalDialog(
  #     passwordInput("pwd", "Enter password" ),
  #     span('Hint: Name of the R package used.'),
  # 
  #     if (failed)
  #       div(tags$b("Invalid password", style = "color: red;")),
  # 
  #     footer = tagList(
  #       actionButton("ok", "OK")
  #     )
  #   )
  # }
  # 
  # observe({
  #   showModal(dataModal())
  # })
  # 
  # observeEvent(input$ok, {
  #   # Check that data object exists and is data frame.
  #   if(input$pwd == "simario") {
  #     removeModal()
  #   } else {
  #     showModal(dataModal(failed = TRUE))
  #   }
  # })
  
  
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
  
  rv <- reactiveValues(env.scenario = NULL, finalFormulaSB = NULL, savedScenario = list())
  
  #############################################################################################
  
  
  output$oModel <- renderVisNetwork({
    # customization adding more variables (see visNodes and visEdges)
    
    nodes <- read.csv("base/nodes.csv")
    edges  <- read.csv("base/edges.csv")
    
   
    visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
      visOptions(highlightNearest = list(enabled = TRUE, degree = 2, hover = TRUE), 
                 nodesIdSelection = TRUE)  %>%
      visEvents( selectNode = "function(properties) {
                 Shiny.onInputChange('var_SB', properties.nodes);
                 Shiny.onInputChange('dynamicTB', properties.nodes);}",
                 doubleClick  = "function(properties) {
                 var number = Math.random();  
                 Shiny.onInputChange('switchTB', number);}",
                 click  = "function(properties) {
                 var number = Math.random();  
                 Shiny.onInputChange('switchSB', number);}")%>% 
      visHierarchicalLayout(direction = "RL", levelSeparation = 250)
  })
    
  observeEvent(input$switchSB, {
    updateTabItems(session, "tabs", "sb")
  })
  
  observeEvent(input$switchTB, {
    updateTabItems(session, "tabs", "tb")
  })  
  
  
  
  
  #############################################################################################
  #Sorting out the Variables
  
  varName <- initialSim$dict$descriptions 
  
  time_invariant_vars <- attr(initialSim$simframe, "time_invariant_vars")
  
  catvars <- unique(c(time_invariant_vars$Varname[time_invariant_vars$Outcome_type=="categorical"], 
                      getOutcomeVars(initialSim$simframe, "categorical"),  names(binbreaks)))
  
  contvars <- unique(c(getOutcomeVars(initialSim$simframe, "continuous"), 
                time_invariant_vars$Varname[time_invariant_vars$Outcome_type=="continuous"]))
  
  freqList <-  data.frame(Var =catvars, 
                          Name = as.character(trimws(varName[catvars])), 
                          stringsAsFactors = FALSE)

  meansList <-  quantilesList <-data.frame(Var = contvars, 
                                           Name = as.character(trimws(varName[contvars])), 
                                           stringsAsFactors = FALSE)
  
  quantilesList <- quantilesList[order(quantilesList$Name),]  
  
  varList <-data.frame(Var =  unique(c(catvars, contvars)), 
                       Name = as.character(trimws(varName[unique(c(catvars, contvars))])), 
                       stringsAsFactors = FALSE)
  
  var_SB <- names(env.base$cat.adjustments)

  var_SB_New <- sapply(strip_lvl_suffix(var_SB), 
                              function(x) varList$Var[ strip_lvl_suffix(varList$Var) %in% x])

  varName_SB <-  data.frame(old =var_SB,
                            Var =var_SB_New, 
                            Name = trimws(varName[var_SB_New]), 
                            stringsAsFactors = FALSE)
  
  varName_SB <- varName_SB[order(varName_SB$Name),]  
  
  varList <- varList[order(varList$Name),]  
  
  meansList <- meansList[order(meansList$Name),]  
 
  freqList <- freqList[order(freqList$Name),]  
  
  ################################################################################
  #Scenario Builder
  
 
  env.scenario <<- NULL
  
  observeEvent(input$newSB ,{ 
      env.scenario <<- createSimenv(input$nameSB, 
                                    initialSim$simframe, initialSim$dict, "years1_21")
      rv$finalFormulaSB<- NULL
  })
  
  
  output$uiSB <- renderUI({  
    
    selectInput("var_SB", "Select Variable to Examine", selected = input$var_SB,
                choices = varName_SB$Name)    
  })
  
  output$uiExprSB <- renderUI({
    selectInput("subGrp_SB", "Select Subgroup for subgroup formula:",
                choices = c(None='None',  c(varList$Name)))
  })
  
  output$uiExprSB1 <- renderUI({
    if(input$subGrp_SB == 'None')
      return()
    else  
      choice <- names(env.base$dict$codings[[varList$Var[varList$Name == input$subGrp_SB]]])
    
    if(is.null(choice)){
      inputPanel(
        selectInput("subGrp_SB1", input$subGrp_SB, 
                    choices = c("Equals" = "==",  "Less than" = "<", 
                                "Greater than" = ">", "Less than or equal to" = "<=", 
                                "Greater than or equal to" = ">=", "Not equals to " = "!="), 
                    selectize=FALSE),
        textInput("subGrpNum_SB2", "" )
      )
    } else {
      selectInput("subGrp_SB1", input$subGrp_SB,
                  choices = choice, 
                  selectize=FALSE)
    }
  })
  
  
  logisetexprSB <-eventReactive( input$andSB | input$orSB | input$completeSB, {
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    
    index = env.base$dict$codings[[varList$Var[varList$Name == input$subGrp_SB]]][
      (names(env.base$dict$codings[[varList$Var[varList$Name == input$subGrp_SB]]])==input$subGrp_SB1)]
    
    print(index)
    if(is.null(index)){
      paste(varList$Var[varList$Name == input$subGrp_SB], 
            paste(input$subGrp_SB1, input$subGrpNum_SB2), sep = " ")    
    }else{
      paste(varList$Var[varList$Name == input$subGrp_SB], index, sep = " == ")
    }
    
  })
  
  observeEvent( input$andSB, { 
    rv$finalFormulaSB <- paste(rv$finalFormulaSB, logisetexprSB(), "&")
  })
  
  observeEvent( input$orSB, { 
    rv$finalFormulaSB <- paste(rv$finalFormulaSB, logisetexprSB(), "|")
  })
  
  observeEvent( input$completeSB, { 
    rv$finalFormulaSB <- paste(rv$finalFormulaSB, logisetexprSB())
  })
  
  observeEvent( input$resetSB, { 
    rv$finalFormulaSB <- NULL
  })
  
  
  output$uilogisetexprSB <- renderUI({  
    textareaInput("logisetexprSB",  "Subgroup formula:", value = rv$finalFormulaSB)
  })
  
  
  baseOutputSB <- reactive({ 
    
    print(as.character(varName_SB$Var[varName_SB$Name==input$var_SB]))
    
    results <- tableBuilderNew(env.base, statistic = "frequencies", 
                               dict = env.base$dict,
                               variableName = as.character(varName_SB$Var[varName_SB$Name==input$var_SB]),
                               grpbyName = "", 
                               logisetexpr=trimws(input$logisetexprSB), digits = 5)
  
    if("Var" %in% names(results) )
      temp <- results  %>% distinct(Var, Mean, Lower, Upper, .keep_all = TRUE)
    else 
      temp <- results  %>% distinct( Mean, Lower, Upper, .keep_all = TRUE)
    
    if((nrow(temp) != nrow(results)) & (1 %in% temp$Year)){
      results <- temp %>% filter(Year == 1)
    } else if((nrow(temp) != nrow(results))){
      results <- temp
    }
    
    temp <- results %>% select(Var, Year, Mean) %>% spread(Var, Mean) 
    
    temp[, c("Year", unique(results$Var))]
  })
  
  output$previewSB  <- DT::renderDataTable({
    
    results <- baseOutputSB()
   
    if(nrow(results) == 1){
      
      rownames(results) <- results$Year
      results <- t(results[,-1])
      colnames(results) <- isolate(input$var_SB)

      datatable(results, class = 'table-condensed',  extensions = 'Scroller',
                options = list(pageLength = 21, dom = 't',
                               scrollX = TRUE, scrollY =  600,
                               scrollCollapse = TRUE,
                               fixedColumns = list(leftColumns = 2)), 
                rownames = TRUE) %>% formatRound(1 ,digits = 1)
    } else{

      datatable(results, class = 'table-condensed',  extensions = 'Scroller',
                options = list(pageLength = 21, dom = 't',
                               scrollX = TRUE, scrollY = 600,
                               scrollCollapse = TRUE,
                               fixedColumns = list(leftColumns = 2)), 
                rownames = FALSE) %>% formatRound(-1 ,digits = 1)
    }
  })
  
  
  # hotable
  output$hotable <- renderRHandsontable({
 
    catAdj <- env.base$cat.adjustments[[as.character(varName_SB$old[varName_SB$Name==input$var_SB])]]
    
    baseResults <- baseOutputSB()
    
    catAdj <- catAdj[baseResults$Year, ]
    
    
    
    tbl <-
    if(!is.matrix(catAdj)){	
      catAdj <- t(catAdj)
      
      temp = cbind(Rowname = colnames(catAdj),  as.data.frame(apply(catAdj,2, as.numeric)))
      
      if( baseResults$Year == 1)
        colnames(temp) <- c("Level",  input$var_SB)
      else 
        colnames(temp) <- c("Level",  paste0("Year ",baseResults$Year))
      
      temp
    }else {
      
      cbind(Rowname =  rownames(catAdj), as.data.frame(apply(catAdj,2, as.numeric), 
                                                      stringsAsFactors = FALSE))
      }
   
    
    
    rhandsontable(tbl, readOnly = FALSE, rowHeaders = NULL) %>% 
      hot_validate_numeric(col = 2:ncol(tbl), min = 0, max = 100, allowInvalid = TRUE)
  })
  
  
  
  output$resultSB  <- renderPrint({
    observeEvent(input$newSB ,{ 
      rv$env.scenario <- createSimenv( input$nameSB, 
                                       initialSim$simframe, 
                                       initialSim$dict, "years1_21")  
    })
    
    
    observeEvent( input$actionAddSB, {
     
      catAdj  <- t(t(hot_to_r( input$hotable)))[,-1]
      
      
      baseResults <- baseOutputSB()
        
      if(is.matrix(catAdj)){

        catAdjFinal <- env.base$cat.adjustments[[as.character(varName_SB$old[varName_SB$Name==input$var_SB])]]
        catAdjFinal[baseResults$Year,] <- catAdj
        
        catAdj <- apply(catAdjFinal, 2, function(x) as.numeric(x)/100)
        
        for(i in 1:nrow(catAdj))
          rv$env.scenario$cat.adjustments[[
            as.character(varName_SB$old[varName_SB$Name==input$var_SB])]][i,] <- catAdj[i,]		   
        
      } else {
        
        rv$env.scenario$cat.adjustments[[
          as.character(varName_SB$old[varName_SB$Name==input$var_SB])]][1,] <- as.numeric(catAdj)/100	
      }
    })
    

    
    observeEvent(input$actionSB, { 
      
      if(!is.null(input$uilogisetexprSB))
        rv$env.scenario <- setGlobalSubgroupFilterExpression( rv$env.scenario, 
                                                              input$uilogisetexprSB)
      
      #Simulation for the new scenario is here. 
      rv$env.scenario <- simulateSimario(rv$env.scenario, 10, simulateKnowLab)
      
      
      index <- 
        mapply(all.equal, env.base$modules$run_results$run1, 
               rv$env.scenario$modules$run_results$run1) != "TRUE"
      
      rv$env.scenario$modules$run_results <- 
        lapply(rv$env.scenario$modules$run_results, function(x) x[index])
      
      
      if(rv$env.scenario$name %in% names(rv$savedScenario)[length(rv$savedScenario)]  ){
        rv$savedScenario[rv$env.scenario$name] <- rv$env.scenario
      } else {
        rv$savedScenario$XxxX <-  rv$env.scenario
        
        names(rv$savedScenario)[length(rv$savedScenario)] <-rv$env.scenario$name
      }
      
      rv$env.scenario <- NULL
      
      print("Simulation is Finished!" )
    })
    
  })
  
  
  output$StartSim <-renderPrint({
    if(input$newSB == 0)
      return(NULL)
    
    if(input$newSB == input$actionSB)
      "Simulation is Finished!"
    else 
      "Setting the current scenario!"
  })
  
  
  
  ###############################################################################
  observeEvent(input$file1, {
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    load(inFile$datapath)
    
    rv$savedScenario <- c(rv$savedScenario, savedScenario)
    
    rm(savedScenario)}  
  )
  
  output$selectSB <- renderUI({
    

    selectInput("selSB", "Select saved Scenario:",
                choices = c(names(rv$savedScenario )))
    
  })
  
###################################################################################
#Table builder

  
  
  output$uiTB <- renderUI({
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type_TB,
           "Percentage" = selectInput("dynamicTB", "Variable",choices = sort(freqList$Name), selected = input$dynamicTB),
           "Means" = selectInput("dynamicTB", "Variable", choices =  sort(meansList$Name), selected = input$dynamicTB),
           "Quantiles" = selectInput("dynamicTB", "Variable", choices =  sort(quantilesList$Name), selected = input$dynamicTB)
    )
  })
  
  output$uiSubGrpTB <- renderUI({

    input$input_type_TB
    rv$finalFormulaSB<- NULL

    selectInput("subGrp_TB", "Select ByGroup:", 
                choices = c(None='None',  sort(freqList$Name)))
  })
  
  
  output$uiExprTB <- renderUI({
    input$input_type_TB
    
    selectInput("subGrp_TB1", "Select Subgroup for subgroup formula:",
                choices = c(None='None',  varList$Name ))
  })
   
  output$uiExprTB1 <- renderUI({
  
    #print(names(env.base$dict$codings[[names(which(varName == input$subGrp_TB1))]]))
    if(input$subGrp_TB1 == "None")
      return()
    else  
      choice <- names(env.base$dict$codings[[varList$Var[varList$Name == input$subGrp_TB1]]])
    
    isolate(
      if(is.null(choice)){
        inputPanel(
          selectInput("subGrp_TB2", input$subGrp_TB1, 
                      choices = c("Equals" = " == ",  "Less than" = " < ", 
                                 "Greater than" = " > ", "Less than or equal to" = " <= ", 
                                 "Greater than or equal to" = " >= ", "Not equals to " = " != "), 
                      selectize=FALSE),
          textInput("subGrpNum_TB2", "" )
        )
        
      } else {
      selectInput("subGrp_TB2", input$subGrp_TB1,
            choices = choice, 
            selectize=FALSE)
      }
    )
  })
  

  logisetexprTB <-eventReactive(input$andTB | input$orTB | input$completeTB, {
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
  
    index = env.base$dict$codings[[varList$Var[varList$Name == input$subGrp_TB1]]][
      (names(env.base$dict$codings[[varList$Var[varList$Name == input$subGrp_TB1]]])==
         input$subGrp_TB2)]

    
    if(is.null(index)){
      paste(varList$Var[varList$Name == input$subGrp_TB1], 
            paste(input$subGrp_TB2, input$subGrpNum_TB2), sep = " ")    
    }else{
      paste(varList$Var[varList$Name == input$subGrp_TB1], index, sep = " == ")
    }

  })
  
  logisetexprTB1 <-eventReactive(input$operatorTB, {
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    
    index = input$subGrp_TB2
    
    if(is.null(index)){
      paste( input$subGrp_TB1, paste(input$subGrp_TB2, input$subGrpNum_TB2), sep = " ")    
    }else{
      paste(input$subGrp_TB1, index, sep = " == ")
    }
    
  })

  
  observeEvent( input$andTB, { 
    rv$finalFormulaSB <- paste(rv$finalFormulaSB, logisetexprTB(), "&")
  })
  
  observeEvent( input$orTB, { 
    rv$finalFormulaSB <- paste(rv$finalFormulaSB, logisetexprTB(), "|")
  })
  
  observeEvent( input$completeTB, { 
    rv$finalFormulaSB <- paste(rv$finalFormulaSB, logisetexprTB())
  })
  
  observeEvent( input$resetTB, { 
    rv$finalFormulaSB <- NULL
  })
    
  output$uilogisetexprTB <- renderUI({  
    textareaInput("logisetexprTB",  "Subgroup formula:", value = rv$finalFormulaSB)
  })
  
  
  baseTB <<- NULL 
  
  summaryOutputTB <- eventReactive( input$actionTB, { 
    
    inputType = c("frequencies", "means", "quantiles")
    
    names(inputType) = c("Percentage", "Means","Quantiles" )
    

    grpbyName = varList$Var[varList$Name==input$subGrp_TB] 
    
    print(grpbyName)
    
    if(length(grpbyName) == 0) grpbyName = ""
  
    results <- tableBuilderNew(env.base, 
                               statistic = inputType[input$input_type_TB], 
                               variableName = varList$Var[varList$Name==input$dynamicTB][1],
                               grpbyName = grpbyName, CI = input$ci, 
                               logisetexpr = trimws(input$logisetexprTB), digits = 5)
    
    
    if("Var" %in% names(results) )
      temp <- results  %>% distinct(Var, Mean, Lower, Upper, .keep_all = TRUE)
    else 
      temp <- results  %>% distinct( Mean, Lower, Upper, .keep_all = TRUE)
      
    if((nrow(temp) != nrow(results)) & (1 %in% temp$Year)){
      results <- temp %>% filter(Year == 1)
    } else if((nrow(temp) != nrow(results))){
      results <- temp
    }
      
      
    baseTB <<- results
    
    results
  })
  
  tableResult <<- list()
  
  output$uiVar <- renderUI({
    if(length(unique(summaryOutputTB()$Year))!=1)
      selectInput("Var_TB", "Select a level to compare in plot:",  
                  selected = unique(summaryOutputTB()$Var)[2], 
                  choices = unique(summaryOutputTB()$Var))
  })
  
  
  output$resultTB  <- DT::renderDataTable({

    results <- summaryOutputTB()
  
    
    if(length(unique(results$Year))==1 ){
      
      if(results$Year[1] == 1)
        results$Year <- "Childhood"
      
      colnames(results)[1] <- ""
      
    } else if(input$input_type_TB == "Percentage" &  "groupByData" %in% names(results) ){
      
      if(length(unique(results$Var)) == 2)
        results <- results[results$Var==input$Var_TB, ]
      
      
      results <- dcast(melt(results, id.vars = c("Var", "groupByData", "Year")), 
            Year~groupByData + Var + variable)
      
    }else if(input$input_type_TB == "Percentage"){
      if(length(unique(results$Var)) == 2)
        results <- results[results$Var==input$Var_TB, ]
      
      results <- dcast(melt(results, id.vars = c("Var", "Year")), 
                       Year~Var + variable)

    } else if(input$input_type_TB %in% c("Means", "Quantiles") & 
              "groupByData" %in% names(results) ){
      
      if(length(unique(results$Var)) == 2)
        results <- results[results$Var==input$Var_TB, ]
      
      results <- dcast(melt(results, id.vars = c("groupByData", "Year")), 
                       Year ~ groupByData + variable)
      
    } else if(input$input_type_TB %in% c("Means", "Quantiles") &
              "Var" %in% names(results)){
      return(NULL)
    }    
    
       
    
    index <- c(grep("Lower", colnames(results)), grep("Upper", colnames(results)))
    
    if(!input$ci)
      results <- results[,-index]
    
    temp <- tableResult
    temp$Base <-results
    tableResult <<- temp
 
    colnames(results) <- 
      paste0('<span style="font-size:20px">',colnames(results),'</span>')
      
    DT::datatable(results, rownames = FALSE, extensions = 'Scroller', escape = FALSE,
              options = list(pageLength = 9999999, dom = 't',
                             scrollX = TRUE,  deferRender = TRUE, scrollY = 600,
                             scrollCollapse = TRUE))  %>%
      formatStyle(1:ncol(results), 'font-size' = '20px') %>% formatRound(-1 ,digits = 1)
  })
  

  SBTB <<- NULL
  
  summaryOutputSBTB <- eventReactive( input$actionTB, {
    
    inputType = c("frequencies", "means", "quantiles")
    
    names(inputType) = c("Percentage", "Means","Quantiles" )
    
    grpbyName = varList$Var[varList$Name==input$subGrp_TB] 
    
    print(input$selSB)
    
    if(length(grpbyName) == 0) grpbyName = ""


    if(input$basePop == "Base population (Before scenario testing)"){
      results <-tableBuilderNew(rv$savedScenario[[input$selSB]], 
                                statistic = inputType[input$input_type_TB], 
                                variableName = varList$Var[varList$Name==input$dynamicTB],
                                grpbyName = grpbyName, CI = input$ci, 
                                logisetexpr = trimws(input$logisetexprTB),
                                envBase = env.base, basePop = TRUE, digits = 5) 
    } else {
      results <-tableBuilderNew(rv$savedScenario[[input$selSB]], 
                                statistic = inputType[input$input_type_TB], 
                                variableName = varList$Var[varList$Name==input$dynamicTB],
                                grpbyName = grpbyName, CI = input$ci, 
                                logisetexpr = trimws(input$logisetexprTB),
                                envBase = env.base, digits = 5)
    }
    
    
    
    if("Var" %in% names(results) )
      temp <- results  %>% distinct(Var, Mean, Lower, Upper, .keep_all = TRUE)
    else 
      temp <- results  %>% distinct( Mean, Lower, Upper, .keep_all = TRUE)
    
    if((nrow(temp) != nrow(results)) & (1 %in% temp$Year)){
      results <- temp %>% filter(Year == 1)
    } else if((nrow(temp) != nrow(results))){
      results <- temp
    }
    
    SBTB <<-results
    
    results
  })
  
  output$resultSBTB  <- DT::renderDataTable({

    results <- summaryOutputSBTB()
    
    if(length(unique(results$Year))==1 ){
      
      colnames(results)[1] <- ""
      
      if(results$Year[1] == 1)
        results$Year <- "Childhood"
      
    } else if(input$input_type_TB == "Percentage" &  "groupByData" %in% names(results) ){
      if(length(unique(results$Var)) == 2)
        results <- results[results$Var==input$Var_TB, ]
      
      results <- dcast(melt(results, id.vars = c("Var", "groupByData", "Year")), 
                       Year~groupByData + Var + variable)
      
    }else if(input$input_type_TB == "Percentage"){
      
      if(length(unique(results$Var)) == 2)
        results <- results[results$Var==input$Var_TB, ]
      
      results <- dcast(melt(results, id.vars = c("Var", "Year")), 
                       Year~Var + variable)
      
    } else if(input$input_type_TB %in% c("Means", "Quantiles") & 
              "groupByData" %in% names(results) ){
      
      if(length(unique(results$Var)) == 2)
        results <- results[results$Var==input$Var_TB, ]
      
      results <- dcast(melt(results, id.vars = c("groupByData", "Year")), 
                       Year ~ groupByData + variable)
      
    } else if(input$input_type_TB %in% c("Means", "Quantiles") &
              "Var" %in% names(results)){
      return(NULL)
    }    
    
    index <- c(grep("Lower", colnames(results)), grep("Upper", colnames(results)))
    
    if(!input$ci)
      results <- results[,-index]
    
    temp <- tableResult
    temp$Base <-results
    tableResult <<- temp
    
    colnames(results) <- 
      paste0('<span style="font-size:20px">',colnames(results),'</span>')
    
    DT::datatable(results, rownames = FALSE, extensions = 'Scroller', escape = FALSE,
                  options = list(pageLength = 9999999, dom = 't',
                                 scrollX = TRUE,  deferRender = TRUE, scrollY = 600,
                                 scrollCollapse = TRUE))  %>%
      formatStyle(1:ncol(results), 'font-size' = '20px') %>% formatRound(-1 ,digits = 1)
    
  })
  
 
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste('Table Result-',  input$input_type_TB, " ", 
                              input$dynamicTB, " ", 
                              input$selSB, '.xlsx', sep='')
    },
    content = function(con) {
      
      print(rv$finalFormulaSB)
      
      temp <- data.frame(Variable = input$dynamicTB)
      
      if(input$selSB != "")
        temp$Scenario = input$selSB
      
      if(!is.null(rv$finalFormulaSB))
        temp$SubgroupFormula = rv$finalFormulaSB
    
      tableResult$info <- t(temp)
      write.xlsx(tableResult, con)
      tableResult <<- list()
    }
  )
  
  combineResults <- reactive({
    
    baseTB <-summaryOutputTB()
  
    SBTB <- try(summaryOutputSBTB(), silent = TRUE)
    if(class(SBTB) == "try-error") SBTB <- NULL
    
    colname <- names(baseTB)
    
    combineResults <- data.frame(Scenario = "Base", baseTB)
    
    if(!is.null(SBTB))
      combineResults <- rbind(combineResults, data.frame(Scenario = "Scenario", SBTB))
    
    combineResults
  })
  

  output$barchartBase<- renderPlotly({
    
    tables.list <- combineResults()
    
    tables.list <- tables.list %>% filter(Scenario == "Base")
    
    colname <- names(summaryOutputTB())
    
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    if(input$input_type_TB == "Percentage")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ]
    
    tables.list$Year <- factor(tables.list$Year)
   
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(fill=groupByData, y = Mean, x = Year))
    else 
      ggplot(tables.list, aes(y = Mean, x = Year)) 
    
    p  <-  p +  ggtitle(varList$Name[varList$Name==input$dynamicTB]) + 
      geom_bar(position="dodge", stat = "identity") + 
      theme(text = element_text(size = 15))
    
    
    if(isolate(input$ci))
      p <- p + geom_errorbar(limitsGGplot, position=dodge, width=0.25)
    
    ggplotly(p)
  })
  
  output$barchartSC<- renderPlotly({
    
    tables.list <- combineResults()
    
    tables.list <- tables.list %>% filter(Scenario == "Scenario")
    
    colname <- names(summaryOutputTB())
    
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    if(input$input_type_TB == "Percentage")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ]
    
    tables.list$Year <- factor(tables.list$Year)
   
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(fill=groupByData, y = Mean, x = Year))
    else 
      ggplot(tables.list, aes(y = Mean, x = Year)) 
    
    p  <-  p +  ggtitle(varList$Name[varList$Name==input$dynamicTB]) + 
      geom_bar(position="dodge", stat = "identity") + 
      theme(text = element_text(size = 15))
    
    
    if(isolate(input$ci))
      p <- p + geom_errorbar(limitsGGplot, position=dodge, width=0.25)
    
    ggplotly(p)
  })
  
  
  output$barchart<- renderPlotly({
   
    tables.list <- combineResults()
    
    colname <- names(summaryOutputTB())
    
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    if(input$input_type_TB == "Percentage")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ]
    
    tables.list$Year <- factor(tables.list$Year)
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(fill=Scenario, y = Mean, x = Year)) + facet_wrap(~groupByData)
    else 
      ggplot(tables.list, aes(fill=Scenario, y = Mean, x = Year)) 
    
    
    p <- 
      p +  
      ggtitle(varList$Name[varList$Name==input$dynamicTB]) + 
      geom_bar(position="dodge", stat = "identity") + 
      theme(text = element_text(size = 15))
    
    
    if(any(grepl("Lower", colname)))
      p <- p + geom_errorbar(limitsGGplot, position=dodge, width=0.25)
    
    ggplotly(p)
  })
  
  
  
  output$linePlotBase<- renderPlotly({
    
    tables.list <- combineResults()
    
    tables.list <- tables.list %>% filter(Scenario == "Base")
    
    colname <- names(summaryOutputTB())
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
  
    
    if(input$input_type_TB == "Percentage")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ] 
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(colour=groupByData, y = Mean, x = Year))
    else 
      ggplot(tables.list, aes(y = Mean, x = Year)) 
    
    p  <- p + ggtitle(varList$Name[varList$Name==input$dynamicTB]) +  geom_path() + 
      theme(text = element_text(size = 15))
                                                            
    if(isolate(input$ci))
      p <- p + geom_errorbar(limitsGGplot, width=0.2)
    
    ggplotly(p)
  })
  
  output$linePlotSC<- renderPlotly({
    
    
    tables.list <- combineResults()
    
    tables.list <- tables.list %>% filter(Scenario == "Scenario")
    
    colname <- names(summaryOutputTB())
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    if(input$input_type_TB == "Percentage")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ] 
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(colour=groupByData, y = Mean, x = Year))
    else 
      ggplot(tables.list, aes(y = Mean, x = Year)) 
    
    p  <- p+  ggtitle(varList$Name[varList$Name==input$dynamicTB]) +  geom_path() + 
      theme(text = element_text(size = 15))
    
    if(isolate(input$ci))
      p <- p + geom_errorbar(limitsGGplot, width=0.2)
    
    ggplotly(p)
  })
  
  
  output$linePlot<- renderPlotly({
    
    tables.list <- combineResults()
    
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    if(input$input_type_TB == "Percentage")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ] 
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(colour=groupByData, y = Mean, x = Year))
    else 
      ggplot(tables.list, aes(y = Mean, x = Year)) 
    
    p <- p +  ggtitle(varList$Name[varList$Name==input$dynamicTB]) +  
      geom_path(aes(linetype = Scenario)) + 
      theme(text = element_text(size = 15))

    if(isolate(input$ci))
      p <- p + geom_errorbar(limitsGGplot, width=0.2)
    
    
    ggplotly(p)
  })
  
  
 
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      if(last_plot()$data[[1]]$type == "scatter")
        type <- "Line"
      else 
        type <- "Bar"
      
      paste(type,'Plot-', input$input_type_TB, "-", 
            input$dynamicTB, '.png', sep='')
    },
    content = function(con) {
      ggsave(con)
    }
  )
  
  
  output$saveWrkspace <- downloadHandler(
    filename = function() { 
      paste(input$wrkSpaceName,".RData", sep = "") 
    },
    content = function(file) {
      savedScenario <- rv$savedScenario
      
      save(savedScenario, file = file)
    }
  )
  

  
  
})  