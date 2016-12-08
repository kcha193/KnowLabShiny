

#devtools::install_github("kcha193/simarioV2")

library(RColorBrewer)
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
  
  rv <- reactiveValues(env.scenario = NULL, 
                       finalFormulaSB = NULL, 
                       savedScenario = list(),
                       message = "Choose a Variable to Examine.", 
                       currSB = "NULL", 
                       tableResult = list())
  
 
  
  
  
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
  
  #############################################################################################
  
  
  output$oModel <- renderVisNetwork({
    # customization adding more variables (see visNodes and visEdges)
    
    nodes <- read.csv("base/nodes.csv")
    
    
    edges  <- read.csv("base/edges.csv")


    visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
      visNodes(value=1, shadow=FALSE,	font=list(size=30)) %>% 
      visEdges(width=6, shadow=FALSE,	font=list(size=30), 
               dashes=FALSE, length = 100, smooth=FALSE) %>% 
      visOptions(highlightNearest = list(enabled = TRUE, degree = 2, hover = TRUE), 
                 nodesIdSelection = TRUE)  %>%
      visEvents( selectNode = "function(properties) {
                 Shiny.onInputChange('var_SB', properties.nodes);
                 Shiny.onInputChange('dynamicTB', properties.nodes);}",
                 selectEdge = "function(properties) {
                 window.open(properties.edges);}")%>%
      visPhysics(solver="forceAtlas2Based", forceAtlas2Based=list(avoidOverlap = 0.9), 
                 maxVelocity = 120, minVelocity =120)
   }) 
  
  observeEvent(input$switchSB, {
    updateTabItems(session, "tabs", "sb")
  })
  
  observeEvent(input$switchTB, {
    updateTabItems(session, "tabs", "tb")
  })  
  
  
  ################################################################################
  #Scenario Builder
  
  
  output$uiNameSB <- renderUI({
    
    textInput("nameSB", "Name of your scenario", value = "Scenario1")
  })
  
  observe({ 
    rv$env.scenario <- createSimenv(input$nameSB, 
                                  initialSim$simframe, 
                                  initialSim$dict, "years1_21")
    rv$finalFormulaSB<- NULL
    rv$currSB <- "NULL"
    rv$finalFormulaSB<- NULL
    
    if(length(rv$savedScenario) > 0 )
      rv$message <- "Simulation is Finished! <br> Choose another Variable to Examine for next Scenario."
    
    else 
      rv$message <- "Choose a Variable to Examine."
    
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
  
  
  logisetexprSB <-eventReactive(input$completeSB, {
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
    rv$finalFormulaSB <- paste(rv$finalFormulaSB, "&")
  })
  
  observeEvent( input$orSB, { 
    rv$finalFormulaSB <- paste(rv$finalFormulaSB, "|")
  })
  
  observeEvent( input$completeSB, { 
    rv$finalFormulaSB <- paste(rv$finalFormulaSB, logisetexprSB())
  })
  
  observeEvent( input$leftBrackSB, { 
    rv$finalFormulaSB <- paste0("(", rv$finalFormulaSB)
  })
  
  observeEvent( input$rightBrackSB, { 
    rv$finalFormulaSB <-  paste0(rv$finalFormulaSB,")")
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
 
    newDim <- dim(baseOutputSB())
    
    if(is.null(input$hotable))
      oldDim <- c(0,0)
    else 
      oldDim <- dim(hot_to_r(input$hotable))
    
    
    if(nrow(baseOutputSB()) == 1)
      newDim = oldDim
    
    
    if(is.null(input$hotable) | (isolate(rv$currSB) != input$var_SB) | any(newDim != oldDim)){
      
      catAdj <- env.base$cat.adjustments[[as.character(varName_SB$old[varName_SB$Name==input$var_SB])]]
    
      rv$currSB <- input$var_SB
      
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
 
    }else {
      tbl <- hot_to_r(input$hotable)

      if(ncol(tbl) != 2){
        index <- which(apply(tbl, 1, function(x) sum(is.na(x)) == 1))
        
         if(length(index) == 0)
           return(rhandsontable(tbl, readOnly = FALSE, rowHeaders = NULL) %>% 
             hot_validate_numeric(col = 2:ncol(tbl), min = 0, max = 100, allowInvalid = TRUE))
         
        temp <- tbl[index,-1]
        
        temp[is.na(temp)] <- 100 - sum(temp, na.rm = TRUE)
        
        tbl[index,-1] <- temp
       
      } else {
        temp <- tbl[,2]
        
        if(sum(is.na(temp)) != 1)
          return(rhandsontable(tbl, readOnly = FALSE, rowHeaders = NULL) %>%
                   hot_validate_numeric(col = 2:ncol(tbl), min = 0, max = 100, allowInvalid = TRUE))
        
        temp[is.na(temp)] <- 100 - sum(temp, na.rm = TRUE)
        
        tbl[,2] <- temp
        
      }
    }
    
    return(rhandsontable(tbl, readOnly = FALSE, rowHeaders = NULL) %>% 
      hot_validate_numeric(col = 2:ncol(tbl), min = 0, max = 100, allowInvalid = TRUE))
  })
    
    observeEvent( input$actionAddSB, {
      
      catAdj  <- hot_to_r( isolate(input$hotable))[,-1]
      
     
      baseResults <- baseOutputSB()
        
      if(is.data.frame(catAdj)){
        
        index <- which(apply(catAdj, 1, function(x) sum(is.na(x)) == 1))

        if(length(index) > 0){
          temp <- catAdj[index,, drop = FALSE]
          temp  <- t(apply(temp, 1, function(x){ 
            x[is.na(x)]<-100 - sum(x, na.rm = TRUE)
            return(x)
            } ))
          catAdj[index,] <- temp
        }
        
        catAdjFinal <- env.base$cat.adjustments[[as.character(varName_SB$old[varName_SB$Name==input$var_SB])]]
        catAdjFinal[baseResults$Year,] <- as.matrix(catAdj)
        
        catAdj <- apply(catAdjFinal, 2, function(x) as.numeric(x)/100)
        
        for(i in 1:nrow(catAdj))
          rv$env.scenario$cat.adjustments[[
            as.character(varName_SB$old[varName_SB$Name==input$var_SB])]][i,] <- catAdj[i,]		   
        
      } else {
        
        if(any(is.na(catAdj)))
          catAdj[is.na(catAdj)] <- 100 - sum(catAdj, na.rm = TRUE)
        
        if(baseResults$Year != 1)
          rv$env.scenario$cat.adjustments[[
            as.character(varName_SB$old[varName_SB$Name==input$var_SB])]][baseResults$Year,] <-
            as.numeric(catAdj)/100	
        else 
          rv$env.scenario$cat.adjustments[[
            as.character(varName_SB$old[varName_SB$Name==input$var_SB])]][1,] <- as.numeric(catAdj)/100	
      }
      
      message <-  paste(input$var_SB, "is inserted in the scenario!")
      
      
      rv$message <- 
        if(rv$message == "Choose a Variable to Examine." | grepl(message, rv$message))
          paste(message)
        else 
          paste(rv$message, message, sep = "<br/>" )
    })
    

    
    observeEvent(input$actionSB, { 
      
      if(!is.null(input$uilogisetexprSB))
        rv$env.scenario <- setGlobalSubgroupFilterExpression( rv$env.scenario, 
                                                              input$uilogisetexprSB)
      
      #Simulation for the new scenario is here. 
      
      showModal(modalDialog(
        title = "Simulation in progress",
        "This may take a while...",
        footer = NULL
      ))

    
      rv$env.scenario <- simulateSimario(rv$env.scenario, 10, simulateKnowLab)
      
      # withProgress(message = 'Simulation in progress', style = "old",
      #              detail = 'This may take a while...', value = 0, {
      #                for (i in 1:15) {
      #                  incProgress(1/15)
      #                  Sys.sleep(0.25)
      #                }
      #                
      #   rv$env.scenario <- simulateSimario(rv$env.scenario, 10, simulateKnowLab)            
      #                
      # })
      
      removeModal()

      
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

      updateTextInput(session, "nameSB", "Name of your scenario", 
                      value = paste0("Scenario", length(rv$savedScenario) + 1) )
      
    })
  
  
  output$StartSim <-renderUI({
   
      return(HTML(paste("<font color=\"red\">",rv$message, "</font>")))
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
  
  output$saveWrkspace <- downloadHandler(
    filename = function() { 
      paste(input$wrkSpaceName,".RData", sep = "") 
    },
    content = function(file) {
      savedScenario <- rv$savedScenario
      
      save(savedScenario, file = file)
    }
  )
  
  
###################################################################################
#Table builder

  output$uiTB <- renderUI({
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.

    switch(input$input_type_TB,
           "Percentage" = selectInput("dynamicTB", "Variable",choices = sort(freqList$Name), 
                                      selected = input$dynamicTB),
           "Mean" = selectInput("dynamicTB", "Variable", choices =  sort(meansList$Name),
                                selected = input$dynamicTB),
           "Quantile" = selectInput("dynamicTB", "Variable", choices =  sort(quantilesList$Name),
                                    selected = input$dynamicTB)
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
  

  logisetexprTB <-eventReactive( input$completeTB, {
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
  
  observeEvent( input$leftBrackTB, { 
    rv$finalFormulaSB <- paste0("(", rv$finalFormulaSB)
  })
  
  observeEvent( input$rightBrackTB, { 
    rv$finalFormulaSB <-  paste0(rv$finalFormulaSB,")")
  })
  
  
  observeEvent( input$andTB, { 
    rv$finalFormulaSB <- paste(rv$finalFormulaSB, "&")
  })
  
  observeEvent( input$orTB, { 
    rv$finalFormulaSB <- paste(rv$finalFormulaSB,  "|")
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
  
  summaryOutputTB <- reactive( { 
    
    inputType = c("frequencies", "means", "quantiles")
    
    names(inputType) = c("Percentage", "Mean","Quantile" )
    

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
    else if("Mean" %in% names(results) )
      temp <- results  %>% distinct( Mean, Lower, Upper, .keep_all = TRUE)
    else 
      temp <- results  %>% distinct(Min, X10th, X25th, X50th, 
                                    X75th, X90th, Max, .keep_all = TRUE)
    
    if(length(unique(temp$Year)) > 1)
      temp <- results[results$Year >= range(temp$Year)[1] & results$Year <= range(temp$Year)[2],]
    
    if((nrow(temp) != nrow(results)) & (1 %in% temp$Year)){
      results <- temp %>% filter(Year == 1)
    } else if((nrow(temp) != nrow(results))){
      results <- temp
    }
      
      
    baseTB <<- results
    
    results
  })
  

  
  output$uiVar <- renderUI({
   # if(length(unique(summaryOutputTB()$Year))!=1)
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

    } else if(input$input_type_TB %in% c("Mean", "Quantile") & 
              "groupByData" %in% names(results) ){
      
      if(length(unique(results$Var)) == 2)
        results <- results[results$Var==input$Var_TB, ]
      
      results <- dcast(melt(results, id.vars = c("groupByData", "Year")), 
                       Year ~ groupByData + variable)
      
    } else if(input$input_type_TB %in% c("Mean", "Quantile") &
              "Var" %in% names(results)){
      return(NULL)
    }    
    
       
    
    index <- c(grep("Lower", colnames(results)), grep("Upper", colnames(results)))
    
    if(!input$ci)
      results <- results[,-index]
    
    if(input$input_type_TB == "Percentage")
      colnames(results) <- gsub("Mean", "Percent", colnames(results))
    
    rv$tableResult$Base <- results
      
    colnames(results) <- 
      paste0('<span style="font-size:20px">',colnames(results),'</span>')
      
    
    
    DT::datatable(results, rownames = FALSE, extensions = 'Scroller', escape = FALSE,
              options = list(pageLength = 9999999, dom = 't',
                             scrollX = TRUE,  deferRender = TRUE, scrollY = 600,
                             scrollCollapse = TRUE))  %>%
      formatStyle(1:ncol(results), 'font-size' = '20px') %>% formatRound(-1 ,digits = 1)
  })
  

  SBTB <<- NULL
  
  summaryOutputSBTB <- reactive({
    
    inputType = c("frequencies", "means", "quantiles")
    
    names(inputType) = c("Percentage", "Mean","Quantile" )
    
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
    else if("Mean" %in% names(results) )
      temp <- results  %>% distinct( Mean, Lower, Upper, .keep_all = TRUE)
    else 
      temp <- results  %>% distinct(Min, X10th, X25th, X50th, X75th, X90th, Max, .keep_all = TRUE)
    
    if(length(unique(temp$Year)) > 1)
      temp <- results[results$Year >= range(temp$Year)[1] & results$Year <= range(temp$Year)[2],]
    
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
      
    } else if(input$input_type_TB %in% c("Mean", "Quantile") & 
              "groupByData" %in% names(results) ){
      
      if(length(unique(results$Var)) == 2)
        results <- results[results$Var==input$Var_TB, ]
      
      results <- dcast(melt(results, id.vars = c("groupByData", "Year")), 
                       Year ~ groupByData + variable)
      
    } else if(input$input_type_TB %in% c("Mean", "Quantile") &
              "Var" %in% names(results)){
      return(NULL)
    }    
    
    index <- c(grep("Lower", colnames(results)), grep("Upper", colnames(results)))
    
    if(!input$ci)
      results <- results[,-index]
    
    if(input$input_type_TB == "Percentage")
      colnames(results) <- gsub("Mean", "Percent", colnames(results))
    
      
    rv$tableResult$Scenario <- results
    
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
    
      rv$tableResult$info <- t(temp)
      write.xlsx(rv$tableResult, con)
      rv$tableResult <- list()
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
    
    if(input$input_type_TB == "Percentage")
     p  <-  p + ylab("Percentage")
    
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
    
    
    if(input$input_type_TB == "Percentage")
     p  <-  p + ylab("Percentage")
    
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
    
    
    if(input$input_type_TB == "Percentage")
     p  <-  p + ylab("Percentage")

    
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
   
    if(input$input_type_TB == "Percentage")
     p  <-  p + ylab("Percentage")
                                                         
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
    
    if(input$input_type_TB == "Percentage")
     p  <-  p + ylab("Percentage")

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

    if(input$input_type_TB == "Percentage")
     p  <-  p + ylab("Percentage")

    
    if(isolate(input$ci))
      p <- p + geom_errorbar(limitsGGplot, width=0.2)
    
    
    ggplotly(p)
  })
  
  
  output$boxPlotBase<- renderPlot({
    
    tables.list <- combineResults()
    
    tables.list <- tables.list %>% filter(Scenario == "Base")
    
    colname <- names(summaryOutputTB())

    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(fill =groupByData, x = Year,  ymin = `Min`, lower = `X25th`, 
                              middle = `X50th`, upper = `X75th`, ymax = `Max`)) 
    else 
      ggplot(tables.list, aes(x = Year,  ymin = `Min`, lower = `X25th`, 
                              middle = `X50th`, upper = `X75th`, ymax = `Max`)) 
    
    p  <- p + ggtitle(varList$Name[varList$Name==input$dynamicTB]) + 
      geom_boxplot(stat = "identity") + theme(text = element_text(size = 15))
  
    p
  })
  
  
  
  output$boxPlotSC<- renderPlot({
    
    tables.list <- combineResults()
    
    tables.list <- tables.list %>% filter(Scenario == "Scenario")
    
    colname <- names(summaryOutputTB())
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(fill =groupByData, x = Year,  ymin = `Min`, lower = `X25th`, 
                              middle = `X50th`, upper = `X75th`, ymax = `Max`)) 
    else 
      ggplot(tables.list, aes(x = Year,  ymin = `Min`, lower = `X25th`, 
                              middle = `X50th`, upper = `X75th`, ymax = `Max`)) 
    
    p  <- p + ggtitle(varList$Name[varList$Name==input$dynamicTB]) + 
      geom_boxplot(stat = "identity") + theme(text = element_text(size = 15))
    
    p
   
  })
  
  output$boxPlot<- renderPlot({
    
    tables.list <- combineResults()
    
    colname <- names(summaryOutputTB())
    
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(fill =Scenario, x = Year,  ymin = `Min`, lower = `X25th`, 
                              middle = `X50th`, upper = `X75th`, ymax = `Max`)) + facet_wrap(~groupByData)
    else 
      ggplot(tables.list, aes(fill =Scenario, x = Year,  ymin = `Min`, lower = `X25th`, 
                              middle = `X50th`, upper = `X75th`, ymax = `Max`)) 
    
    p  <- p + ggtitle(varList$Name[varList$Name==input$dynamicTB]) + 
      geom_boxplot(stat = "identity") + theme(text = element_text(size = 15))
    
    p
    
  })
  
  # nz1 <- getData("GADM", country = "NZ", level = 1)
  # nz1 <- spTransform(nz1, CRS("+init=epsg:2135"))
  # 
  # ## Extract polygon corners and merge with shapefile data
  # nz1@data$id <- rownames(nz1@data)
  # nz1.ff <- fortify(nz1)
  # nz1.df <- merge(nz1@data, nz1.ff, by = "id", all.y = TRUE)
  # rm(nz1.ff)
  # rm(nz1)
  # 
  # nz1.df <- nz1.df[c("long", "lat", "group", "NAME_1")]
  # 
  # toRemove <- which(nz1.df$NAME_1 == "Chatham Islands")
  # 
  # nz1.df <- nz1.df[-toRemove,]
  # 
  # 
  # output$uiYear <- renderUI({
  #   selectInput("year", "Select Year:", unique(combineResults()$Year))
  # })
  # 
  # 
  # output$map<- renderPlotly({
  #   
  #   tables.list <- combineResults()
  #   
  #   if(input$input_type_TB == "Percentage")
  #     tables.list <- tables.list[tables.list$Var==input$Var_TB, ] 
  #     
  #   if(length(unique(tables.list$Year)) != 1)
  #     tables.list <- tables.list[tables.list$Year == input$year, ] 
  #   
  #   
  #   if( length(unique(tables.list$Scenario))==2){
  #     nz1.df <- data.frame(Scenario = rep(c("Base", "Scenario"), 
  #                                         each = nrow(nz1.df)), 
  #                                         rbind(nz1.df, nz1.df))
  #     
  #     nz1.df$Outcome <- as.factor(nz1.df$NAME_1)
  #     
  #     tables.list$Mean[tables.list$Scenario == "Base" & tables.list$groupByData == "NEL"] <- 
  #       mean(c(tables.list$Mean[tables.list$Scenario == "Base" & tables.list$groupByData == "NEL"], 
  #              tables.list$Mean[tables.list$Scenario == "Base" & tables.list$groupByData == "TAS"]))
  #     
  #     tables.list$Mean[tables.list$Scenario == "Scenario" & tables.list$groupByData == "NEL"] <- 
  #       mean(c(tables.list$Mean[tables.list$Scenario == "Scenario" & tables.list$groupByData == "NEL"], 
  #              tables.list$Mean[tables.list$Scenario == "Scenario" & tables.list$groupByData == "TAS"]))
  #     
  #     tables.list <- tables.list[tables.list$groupByData != "TAS", ]
  #     
  #     levels(nz1.df$Outcome ) <- tables.list$Mean
  #     
  #     
  #     nz1.df$Outcome <- as.numeric(as.character(nz1.df$Outcome))
  #     
  #     
  #   }else{ 
  # 
  #     nz1.df <- data.frame(Scenario = rep(c("Base"),each = nrow(nz1.df)), 
  #                          nz1.df)
  #   
  #     nz1.df$Outcome <- as.factor(nz1.df$NAME_1)
  #     
  #     tables.list$Mean[tables.list$groupByData == "NEL"] <- 
  #       mean(c(tables.list$Mean[tables.list$groupByData == "NEL"], 
  #              tables.list$Mean[tables.list$groupByData == "TAS"]))
  #     
  #     tables.list <- tables.list[tables.list$groupByData != "TAS", ]
  #     
  #     levels(nz1.df$Outcome ) <- tables.list$Mean
  #       
  #     
  #     nz1.df$Outcome <- as.numeric(as.character(nz1.df$Outcome))
  #     
  #   }
  #   
  #   ## Plot map
  #   g <- ggplot(data = nz1.df, aes(x = long, y = lat, group = group, 
  #                                  fill = Outcome, text = NAME_1)) + 
  #     geom_polygon(show.legend = TRUE) + 
  #     scale_fill_gradientn(colours = brewer.pal(6, "RdYlBu")) +
  #     theme_bw() + facet_wrap(~Scenario)+
  #     theme(axis.title=element_blank(),
  #           axis.text=element_blank(),
  #           axis.ticks=element_blank())
  #   
  #   ggplotly(g, tooltip = c("text", "fill"))
  # })

  
  output$downloadPlot <- downloadHandler(
    
    
    filename = function() {

      if(input$input_type_TB == "Quantile"){
        type <- "Box"
      }else{   
        if(last_plot()$x$data[[1]]$type == "scatter")
          type <- "Line"
        else 
          type <- "Bar"
      }
      
      paste(type,'Plot-', input$input_type_TB, "-", 
            input$dynamicTB, '.png', sep='')
    },
    content = function(con) {
      ggsave(con)
    }
  )
  


  
  
})  