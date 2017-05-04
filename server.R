

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
  
  # observeEvent(input$switchMI, {
  #   updateTabItems(session, "tabs", "mi")
  # })
  # observeEvent(input$switchSB, {
  #   updateTabItems(session, "tabs", "sb")
  # })
  # 
  # observeEvent(input$switchTB, {
  #   updateTabItems(session, "tabs", "tb")
  # })  
  
  
  
  #source("SimmoduleMELC1_21.R")
  source("simulateKnowLab.R")
  
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
                       tableResult = list(),
                       messageList = "", 
                       compareFreq = numeric(3))
  
  
  
  #############################################################################################
  #Sorting out the Variables
  
  varName <- initialSim$dict$descriptions 
  
  time_invariant_vars <- attr(initialSim$simframe, "time_invariant_vars")
  
  # catvars <- unique(c(time_invariant_vars$Varname[time_invariant_vars$Outcome_type=="categorical"], 
  #                     getOutcomeVars(initialSim$simframe, "categorical"),  names(binbreaks)))
  
  catvars <- c( "r1stchildethn", "SESBTH", "z1genderLvl1", "z1CaesareanLvl1", "z1BreakfastLvl1", "r1mBMI", 
                "r1ParentEduc", "z1GALvl1", "z1stmDiabeteLvl1", "z1HearingLvl1", "z1ECELvl1", "z1PrintExpLvl1", 
                "z1ADHDLvl1", "z1ParentInvolveLvl1", "r1Region", "z1OverweightLvl1", "z1ObeseLvl1", "r1Sleep",
                "r1Score", "z1NEETLvl1", "z1BullyLvl1", "r1SchoolFunding", 
                "r1SchoolGender", "z1AlcAbuseLvl1", "z1DepressLvl1", "z1PUNISHLvl1", "z1INTERACTLvl1", 
                "z1ParentAlcLvl1", "z1ParentDepressLvl1", "z1WatchTVLvl1", "pregalc", "pregsmk", "bwkg", 
                "BREAST", "IQ")
  
  
  #contvars <- unique(c(getOutcomeVars(initialSim$simframe, "continuous"), 
  #              time_invariant_vars$Varname[time_invariant_vars$Outcome_type=="continuous"]))
  
  contvars <- c( "IQ", "BREAST", "bwkg",  "pregalc", "pregsmk")
  
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
  
  #var_SB <- names(env.base$cat.adjustments)
  
  var_SB <- c("z1Overweight", "z1Obese", "r1Sleep", "r1Score", "z1Bully", 
              "r1SchoolFunding", "r1SchoolGender",  "z1AlcAbuse", "z1Depress", "z1PUNISH","z1INTERACT",          
              "z1ParentAlc", "z1ParentDepress", "z1WatchTVLvl1", "BREAST", "bwkg", 
              "pregalc", "pregsmk", "r1stchildethn", "SESBTH", "z1genderLvl1", "z1CaesareanLvl1", 
              "z1BreakfastLvl1", "r1mBMI",  "r1ParentEduc", "z1GALvl1", "z1stmDiabeteLvl1", "z1HearingLvl1",      
              "z1ECELvl1", "z1PrintExpLvl1", "z1ADHDLvl1", "z1ParentInvolveLvl1")
  
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
  
  # output$menu <- renderMenu({
  #   sidebarMenu(
  #    
  #     menuItem("Model input", tabName = "mi", icon = icon("line-chart")),
  #     menuItem("Scenario Builder", tabName = "sb", icon = icon("refresh")),
  #     menuItem("Table Builder", tabName = "tb", icon = icon("table"))
  #   )
  # })
  

  #############################################################################################
  
  
  output$oModel <- renderVisNetwork({
    # customization adding more variables (see visNodes and visEdges)
    
    nodes <- read.csv("base/nodes.csv")
    
    
    edges  <- read.csv("base/edges.csv")
    
    
    visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
      visNodes(value=1, shadow=FALSE,	font=list(size=15)) %>% 
      visEdges(width=5, shadow=FALSE,	font=list(size=30), 
               dashes=FALSE, length = 130, smooth=TRUE) %>% 
      visOptions(highlightNearest = list(enabled = TRUE, degree = 2, hover = TRUE), 
                 nodesIdSelection = FALSE)  %>%
      visEvents( selectEdge = "function(properties) {
                 if(properties.nodes == ''){
                 window.open(properties.edges);
                 }
  }", 
                 selectNode = "function(properties) {
                 Shiny.onInputChange('var_SB', properties.nodes);
                 Shiny.onInputChange('dynamicTB', properties.nodes);}") %>%
      visLayout(randomSeed = 0220171)
    }) 
  
  
  
  ################################################################################
  #Scenario Builder
  output$uiNameSB <- renderUI({
    
    textInput("nameSB", HTML("<font size=\"4\">STEP 1: </font>Name your scenario"), value = "Scenario1")
  })
  
  observe({ 
    rv$env.scenario <- createSimenv(input$nameSB, 
                                    initialSim$simframe, 
                                    initialSim$dict, "years1_21")
    rv$finalFormulaSB<- NULL
    rv$currSB <- "NULL"
    rv$finalFormulaSB<- NULL
    
    if(length(rv$savedScenario) > 0 )
      rv$message <- ""
    else 
      rv$message <- ""
    
  })
  
  output$uiSB <- renderUI({  
    
    selectInput("var_SB", HTML("<font size=\"4\">STEP 2: </font>Select Variable to Examine"),
                selected = input$var_SB,
                choices = varName_SB$Name)    
  })
  
  output$uiExprSB <- renderUI({
    selectInput("subGrp_SB", HTML("<font size=\"4\">STEP 4 (optional): </font>Select Subgroup for subgroup formula:"),
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
    
    if(results$Year[1]=="Childhood" | results$Year[1]=="At birth")
      return(results %>% select(Var, Year, Mean))
    
    results %>% select(Var, Year, Mean) %>% spread(Var, Mean) 
  })
  
  output$SBvar <- renderUI({
    HTML(paste("<font size = \"4\"> <b>", input$var_SB, "</b> </font>")) 
  })
  
  output$previewSB  <- DT::renderDataTable({
    
    results <- baseOutputSB()
    
    
    datatable(results, class = 'table-condensed',  extensions = 'Scroller',
              options = list(pageLength = 21, dom = 't',
                             scrollX = TRUE, scrollY = 600,
                             scrollCollapse = TRUE,
                             fixedColumns = list(leftColumns = 2)), 
              rownames = FALSE) %>% formatRound(-1 ,digits = 1)
  })
  
  
  # hotable
  output$hotable <- renderRHandsontable({
   
    if(is.null(input$hotable) | (isolate(rv$currSB) != input$var_SB)){
      
      catAdj <- env.base$cat.adjustments[[as.character(varName_SB$old[varName_SB$Name==input$var_SB])]]
      
      rv$currSB <- input$var_SB
      
      ageRange <- strsplit(dict$age[as.character(varName_SB$Var[varName_SB$Name==input$var_SB])], "--")[[1]]
      
    
      
      if(ageRange[1] == "Childhood" | ageRange[1] == "At birth"){
        catAdj <- catAdj[1, , drop=FALSE]
      }else {
        ageRange <- as.numeric(ageRange)
        if(length(ageRange) == 2)
          catAdj <- catAdj[ageRange[1]:ageRange[2], ]
        else 
          catAdj <- catAdj[ageRange, , drop=FALSE]
      }
      
      
      tbl <-
        if(nrow(catAdj) == 1){	
          
          catAdj <- t(catAdj)
          
          temp = cbind(Rowname = rownames(catAdj),  as.data.frame(apply(catAdj,2, as.numeric)))
          
          if(ageRange == "Childhood" | ageRange == "At birth")
            colnames(temp) <- c("Level",  input$var_SB)
          else 
            colnames(temp) <- c("Level",  paste0("Year ",ageRange))
          
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
          return(rhandsontable(tbl, readOnly = FALSE, rowHeaders = NULL, contextMenu = FALSE) %>%  hot_cols(colWidths = 130) %>% 
                   hot_validate_numeric(col = 2:ncol(tbl), min = 0, max = 100, allowInvalid = TRUE))
        
        #browser()
        
        temp <- 
          t(sapply(index, function(x) {
            temp <- tbl[x,-1];
            temp[is.na(temp)] <- 100 - sum(temp, na.rm = TRUE);
            temp
          } ))
        
        if(nrow(temp) > 1)
          tbl[index,-1] <- unlist(temp)
        else 
          tbl[index,-1] <- temp
        
      } else {
        
        temp <- tbl[,2]
        
        if(sum(is.na(temp)) != 1)
          return(rhandsontable(tbl, readOnly = FALSE, rowHeaders = NULL, contextMenu = FALSE) %>%  hot_cols(colWidths = 130) %>%
                   hot_validate_numeric(col = 2:ncol(tbl), min = 0, max = 100, allowInvalid = TRUE))
        
        temp[is.na(temp)] <- 100 - sum(temp, na.rm = TRUE)
        
        tbl[,2] <- temp
        
      }
    }
    
    return(rhandsontable(tbl, readOnly = FALSE, rowHeaders = NULL, contextMenu = FALSE) %>%  hot_cols(colWidths = 130) %>% 
             hot_validate_numeric(col = 2:ncol(tbl), min = 0, max = 100, allowInvalid = TRUE ))
  })
  
  output$actionAddSBUI <- renderUI({
    tagList(
      HTML("<b> <font size=\"4\">STEP 5: </font></b> Click after every variable adjusment"),
      actionButton("actionAddSB", label = "Add Scenario")
    )
  })
  
  observeEvent( input$actionAddSB, {
    
    catAdj  <- hot_to_r( isolate(input$hotable))[,-1]
    
    ageRange <- strsplit(dict$age[as.character(varName_SB$Var[varName_SB$Name==input$var_SB])], "--")[[1]]
    
    if(is.data.frame(catAdj)){
      
      index <- which(apply(catAdj, 1, function(x) sum(is.na(x)) == 1))
      ageRange <- as.numeric(ageRange)
      catAdjFinal <- env.base$cat.adjustments[[as.character(varName_SB$old[varName_SB$Name==input$var_SB])]]
      catAdjFinal[ageRange[1]:ageRange[2],] <- as.matrix(catAdj)
      
      catAdj <- apply(catAdjFinal, 2, function(x) as.numeric(x)/100)
      
      for(i in 1:nrow(catAdj))
        rv$env.scenario$cat.adjustments[[
          as.character(varName_SB$old[varName_SB$Name==input$var_SB])]][i,] <- catAdj[i,]		   
      
    } else {
      
      if(ageRange[1] == "Childhood" | ageRange[1] == "At birth")
        rv$env.scenario$cat.adjustments[[
          as.character(varName_SB$old[varName_SB$Name==input$var_SB])]][1,] <- as.numeric(catAdj)/100	
      else                 
        rv$env.scenario$cat.adjustments[[
          as.character(varName_SB$old[varName_SB$Name==input$var_SB])]][as.numeric(ageRange),] <-
          as.numeric(catAdj)/100	
    }
    
    message <-  paste(input$var_SB, "has been added in the scenario.")
    
    rv$message <- 
      if(rv$message == "Choose a Variable to Examine." | grepl(message, rv$message))
        paste(message)
    else 
      paste(rv$message, message, sep = "<br/>" )
  })
  

  
  
  output$actionSBUI <- renderUI({
    tagList(
      h4(strong("Step 7:")),
      actionButton("actionSB", label = "Run Scenario")
    )
  })
  
  observeEvent(input$actionSB, {
    showModal(modalDialog(
      title = HTML(paste("You have made changes: <br> ", rv$message)),
      HTML("Is this everything you want to change for this scenario? <br>
      NB., if variables are missing from above, it may be because you haven't clicked Add scenario after changing each variable"),
      size = "l",
      footer = tagList(
        modalButton("No"),
        actionButton("actionSB1", "Yes, Run Scenario")
      )
    ))
  }) 
  
  observeEvent(input$actionSB1, { 
    
    if(!is.null(input$uilogisetexprSB))
      rv$env.scenario <- setGlobalSubgroupFilterExpression( rv$env.scenario, 
                                                            input$uilogisetexprSB)
    
    
    #Simulation for the new scenario is here. 
    showModal(modalDialog(
      title = "Simulation in progress",
      "This may take a while...",
      size = "l",
      footer = NULL
    ))
    
    rv$messageList <- c(rv$messageList, rv$message)
    
    rv$env.scenario <- simulateSimario(rv$env.scenario, isolate(input$nRun), simulateKnowLab)
    
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
      
      
      rv$savedScenario <- rv$savedScenario[length(rv$savedScenario):1] 
      
    }
    
    rv$env.scenario <- NULL
    
    updateTextInput(session, "nameSB",
                    value = paste0("Scenario", length(rv$savedScenario) + 1) )
    
  })
  
  
  output$StartSim <-renderUI({
    
    return(HTML(rv$message))
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
    
    
    selectInput("selSB", "Select Scenario for comparison:",
                choices = c(names(rv$savedScenario )))
    
  })
  
  output$displaySB <- renderUI({
    
    return(HTML(rv$messageList[which(names(rv$savedScenario ) == input$selSB)]))
    
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
    temp <- HTML("<b> <font size=\"4\">STEP 2: </font></b> Choose variable:")
    
    switch(input$input_type_TB,
           "Percentage" = selectInput("dynamicTB", temp,choices = sort(freqList$Name), 
                                      selected = input$dynamicTB),
           "Mean" = selectInput("dynamicTB", temp, choices =  sort(meansList$Name),
                                selected = input$dynamicTB),
           "Quantile" = selectInput("dynamicTB", temp, choices =  sort(quantilesList$Name),
                                    selected = input$dynamicTB)
    )
  })
  
  output$uiSubGrpTB <- renderUI({
    
    input$input_type_TB
    rv$finalFormulaSB<- NULL
    
    selectInput("subGrp_TB", HTML("<b> <font size=\"4\">STEP 3 (optional): </font></b> Select ByGroup:"), 
                choices = c(None='None',  sort(freqList$Name)))
  })
  
  
  output$uiExprTB <- renderUI({
    input$input_type_TB
    
    selectInput("subGrp_TB1", HTML("<b> <font size=\"4\">STEP 4 (optional): </font></b> Select Subgroup for subgroup formula:"),
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
          div( class = "ui-hide-label", style="float:left",
          selectInput("subGrp_TB2", input$subGrp_TB1, 
                      choices = c("Equals" = " == ",  "Less than" = " < ", 
                                  "Greater than" = " > ", "Less than or equal to" = " <= ", 
                                  "Greater than or equal to" = " >= ", "Not equals to " = " != "), 
                      selectize=FALSE)),
          div( class = "ui-hide-label", style="float:left", textInput("subGrpNum_TB2", ""))
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
    
    
    # if("Var" %in% names(results) )
    #   temp <- results  %>% distinct(Var, Mean, Lower, Upper, .keep_all = TRUE)
    # else if("Mean" %in% names(results) )
    #   temp <- results  %>% distinct( Mean, Lower, Upper, .keep_all = TRUE)
    # else 
    #   temp <- results  %>% distinct(Min, X10th, X25th, X50th, 
    #                                 X75th, X90th, Max, .keep_all = TRUE)
    # 
    # if(length(unique(temp$Year)) > 1)
    #   temp <- results[results$Year >= range(temp$Year)[1] & results$Year <= range(temp$Year)[2],]
    # 
    # if((nrow(temp) != nrow(results)) & (1 %in% temp$Year)){
    #   results <- temp %>% filter(Year == 1)
    # } else if((nrow(temp) != nrow(results))){
    #   results <- temp
    # }
    
    
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
    
    
    if(results$Year[1] ==  "Childhood" | results$Year[1] ==  "At birth"){
      
      
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
    
    notToRound <- c("<span style=\"font-size:20px\">Year</span>", 
                 "<span style=\"font-size:20px\">groupByData</span>", 
                 "<span style=\"font-size:20px\">Var</span>")
    
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
      formatStyle(1:ncol(results), 'font-size' = '20px') %>% 
      formatRound(which(!colnames(results) %in% notToRound), digits = 1)
  })
  
  
  SBTB <<- NULL
  
  compareFreq <- 
    function(env.base, env.scenario, varName){
      
      if(varName %in% time_invariant_vars$Varname){
        Base <-	table(env.base$simframe[[varName]])[-1]/5000
        
        Scenario <-	table(env.scenario$simframe[[varName]])[-1]/5000
        
      } else {
        Base <-	
          sapply(env.base$modules$run_results, function(x) 
            apply(x[[varName]],  2, table)[-1,])/5000
        
        Scenario <-
          sapply(env.scenario$modules$run_results, function(x) 
            apply(x[[varName]],  2, table)[-1,])/5000
      }
      
      results <- numeric(3)
      
      for(i in 1:20){
        lm.fit <- lm(c(Base[i,], Scenario[i,]) ~ factor(rep(c("B", "S"), each = 10)))
        
        results <- 
          rbind(results,c(summary(lm.fit )$coef[2,1],confint(	lm.fit )[2,]))
      }
      
      results <- round(apply(results, 2, mean, na.rm = TRUE)*100, 4)	
      
      names(results) <- c("Mean Diff", "Lower CI", "Upper CI")
      
      results
    }
  
  summaryOutputSBTB <- reactive({
    
    rv$compareFreq <- numeric(3)
    
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


    if (inputType[input$input_type_TB] ==  "frequencies" &
        !varList$Var[varList$Name == input$dynamicTB] %in% time_invariant_vars$Varname &
        varList$Var[varList$Name == input$dynamicTB] %in%
        names(rv$savedScenario[[input$selSB]]$modules$run_results$run1)
    )
      rv$compareFreq <- compareFreq(env.base, rv$savedScenario[[input$selSB]], 
                                    varList$Var[varList$Name==input$dynamicTB])
      
      
    SBTB <<-results
    
    results
  })
  
  output$resultSBTB  <- DT::renderDataTable({
    
    results <- summaryOutputSBTB()
    
    
    if(results$Year[1] ==  "Childhood" | results$Year[1] ==  "At birth"){
      
      
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
    
    notToRound <- c("<span style=\"font-size:20px\">Year</span>", 
                    "<span style=\"font-size:20px\">groupByData</span>", 
                    "<span style=\"font-size:20px\">Var</span>")
    
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
      formatStyle(1:ncol(results), 'font-size' = '20px') %>% 
      formatRound(which(!colnames(results) %in% notToRound), digits = 1)
    
  })
  
  output$ciUI <- 
    renderUI({
      tagList(
        h4(strong("STEP 6 (optional):")),
        checkboxInput("ci", label = "Confidence Interval", value = TRUE)
      )
    })
  
  output$downloadUI <- 
    renderUI({
      tagList(
        h4(strong("STEP 7 (optional):")),
        downloadButton('downloadTable', 'Download Table'),
        downloadButton('downloadPlot', 'Download Plot')
      )
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
      combineResults <- rbind(combineResults, 
                              data.frame(Scenario = "Scenario", SBTB))
    
    
    combineResults
  })
  
  output$compare <- 
    renderPrint({
      rv$compareFreq
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
    
    if(input$ci)
      p <- p + geom_errorbar(limitsGGplot, position=dodge, width=0.25)
    
    ggplotly(p)
  })
  
  output$barchartSC<- renderPlotly({
    
    input$ci
    
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
    
    if(input$ci)
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
      p + ggtitle(varList$Name[varList$Name==input$dynamicTB]) + 
      geom_bar(position=dodge, stat = "identity") + 
      theme(text = element_text(size = 15))
    
    
    if(input$input_type_TB == "Percentage")
      p  <-  p + ylab("Percentage")
    
  
    if(input$ci)
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
      geom_point(size = 2)+ 
      theme(text = element_text(size = 15))
    
    if(input$input_type_TB == "Percentage")
      p  <-  p + ylab("Percentage")
    
    if(input$ci)
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
      geom_point(size = 2)+ theme(text = element_text(size = 15))
    
    if(input$input_type_TB == "Percentage")
      p  <-  p + ylab("Percentage")
    
    if(input$ci)
      p <- p + geom_errorbar(limitsGGplot, width=0.2)
    
    ggplotly(p)
  })
  
  
  output$linePlot<- renderPlotly({
    
    input$ci
    
    tables.list <- combineResults()
    
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    if(input$input_type_TB == "Percentage")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ] 
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(y = Mean, x = Year, colour=Scenario)) + facet_wrap(~groupByData)
    else 
      ggplot(tables.list, aes(y = Mean, x = Year, colour=Scenario)) 
    
    
    p <- p +  ggtitle(varList$Name[varList$Name==input$dynamicTB]) +  
      geom_path(position = dodge)+
      geom_point(size = 2, position = dodge) + 
      theme(text = element_text(size = 15))
    
    if(input$input_type_TB == "Percentage")
      p  <-  p + ylab("Percentage")
    
    if(input$ci)
      p <- p + geom_errorbar(limitsGGplot, width=0.25, 
                             position = dodge)
    
    ggplotly(p)
  })
  
  
  output$boxPlotBase<- renderPlot({
    
    input$ci
    
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
    
    input$ci
    
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
    
    input$ci
    
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