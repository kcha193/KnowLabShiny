

#devtools::install_github("kcha193/simarioV2")

library(shiny)
library(shinydashboard)
library(DT)
library(rhandsontable)
library(parallel)
library(simarioV2)
library(plotly)
library(dplyr)
library(tidyr)
library(openxlsx)
library(reshape2)

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
  
  varName <- initialSim$dict$descriptions 
  
  savedScenario <<- list()
  
  #############################################################################################
  #Sorting out the Variables
  
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

  var_SB_New <- unlist(sapply(var_SB, function(x) rev(varList$Var[grep(x, varList$Var)])[1]))

  varName_SB <-  data.frame(old =var_SB,
                            Var =var_SB_New, 
                            Name = trimws(varName[var_SB_New]), 
                            stringsAsFactors = FALSE)
  
  varName_SB <- varName_SB[order(varName_SB$Name),]  
  
  varList <- varList[order(varList$Name),]  
  
  meansList <- meansList[order(meansList$Name),]  
  
  
  # catvars <- unique(c("z1single0", "pregalc", getOutcomeVars(initialSim$simframe, "categorical"), "SESBTH", 
  #              "r1stmeduc", "r1stfeduc", "fage", "bthorder", "NPRESCH", "z1genderLvl1","r1stchildethn",
  #              names(binbreaks)))
  # 
  # freqList <-  data.frame(Var =catvars, 
  #                         Name = as.character(trimws(varName[catvars])), 
  #                         stringsAsFactors = FALSE)
  
  freqList <- freqList[order(freqList$Name),]  
  
  #################################################################################################################
  #Scenario Builder
  
  #env.scenario  <<- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")
  
  env.scenario <<- NULL
  
  observeEvent(input$newSB ,{ 
      env.scenario <<- createSimenv(input$nameSB, initialSim$simframe, initialSim$dict, "years1_21")  })
  
  
  output$uiSB <- renderUI({  
    
    selectInput("var_SB", "Select Variable to Examine",
                choices = varName_SB$Name)    
  })
  
 
  # hotable
  output$hotable <- renderRHandsontable({
 
    catAdj <- env.base$cat.adjustments[[as.character(varName_SB$old[varName_SB$Name==input$var_SB])]]
    
    tbl <-
    if(nrow(catAdj) == 1){	
      temp = cbind(Rowname = colnames(catAdj),  as.data.frame(apply(catAdj,2, as.numeric)))
      colnames(temp) <- c("Level",  input$var_SB)
      
      temp
    }else {
      cbind(Rowname = rownames(catAdj), as.data.frame(apply(catAdj,2, as.numeric), 
                                                      stringsAsFactors = FALSE))
      }
    
    rhandsontable(tbl, readOnly = FALSE, rowHeaders = NULL) %>% 
      hot_validate_numeric(col = 2:ncol(tbl), min = 0, max = 1, allowInvalid = TRUE)  
  })
  
  
  changeSB <- eventReactive( input$actionAddSB, {
    catAdj <-t(t(hot_to_r(input$hotable)))[,-1] 
    if(is.matrix(catAdj)){
      catAdj = apply(catAdj, 2, as.numeric)
      
      for(i in 1:nrow(catAdj))
        env.scenario$cat.adjustments[[
          as.character(varName_SB$old[varName_SB$Name==input$var_SB])]][i,] <<- catAdj[i,]		   
      
    } else {
      env.scenario$cat.adjustments[[
        as.character(varName_SB$old[varName_SB$Name==input$var_SB])]][1,] <<- as.numeric(catAdj)	
    }   
  })
  

  output$uiExprSB <- renderUI({
    selectInput("subGrp_SB", "Select Subgroup for subgroup formula:",
                choices = c(None='',  c(varList$Name)))
  })
  
  output$uiExprSB1 <- renderUI({
    if(input$subGrp_SB == '')
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
  
  
  logisetexprSB <-eventReactive(input$operatorSB, {
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    
    index = env.base$dict$codings[[varList$Var[varList$Name == input$subGrp_SB]]][
      (names(env.base$dict$codings[[varList$Var[varList$Name == input$subGrp_SB]]])==input$subGrp_SB1)]
    
    print(index)
    if(is.null(index)){
      paste(varList$Var[varList$Name == input$subGrp_SB], paste(input$subGrp_SB1, input$subGrpNum_SB2), sep = " ")    
    }else{
      paste(varList$Var[varList$Name == input$subGrp_SB], index, sep = " == ")
    }
    
  })
  
  finalFormulaSB <<- NULL
  
  finalFormSB <- reactive({
    
    finalFormulaSB <<-
      switch(input$operatorSB, 
             "And" = paste(finalFormulaSB, logisetexprSB(), "&"),
             "Or" = paste(finalFormulaSB, logisetexprSB(), "|"),
             "Complete" = paste(finalFormulaSB, logisetexprSB(), ""),
             "Reset" = "" )
    
    finalFormulaSB
  })
  
  
  output$uilogisetexprSB <- renderUI({  
    textareaInput("logisetexprSB",  "Subgroup formula:", value = finalFormSB())
  })
  
  
  baseOutputSB <- eventReactive(input$preview_SB, { 
    
    print(as.character(varName_SB$Var[varName_SB$Name==input$var_SB]))
    
    results <- tableBuilderNew(env.base, statistic = "frequencies", 
                               dict = env.base$dict,
                               variableName = as.character(varName_SB$Var[varName_SB$Name==input$var_SB]),
                               grpbyName = "", 
                               logisetexpr=trimws(input$logisetexprSB))

    temp <- results %>% select(Var, Year, Mean) %>% spread(Var, Mean) 
    
    temp[, c("Year", unique(results$Var))]
  })
  
  output$previewSB  <- DT::renderDataTable({
       
    results <- baseOutputSB()
    
    if(nrow(results) == 1){

      rownames(results) <- results$Year
      results <- t(results[,-1])
      colnames(results) <- isolate(input$var_SB)
      
      datatable(results, class = 'table-condensed', 
                options = list(pageLength = 21, dom = 't',
                               scrollX = TRUE, scrollY = TRUE,
                               fixedColumns = list(leftColumns = 2)), 
                rownames = TRUE)
    } else{
      datatable(results, class = 'table-condensed', 
               options = list(pageLength = 21, dom = 't',
                              scrollX = TRUE, scrollY = TRUE,
                                fixedColumns = list(leftColumns = 2)), 
                    rownames = FALSE)
    }
  })
  
  output$StartSim <-renderPrint({
    if(input$newSB == 0)
      return(NULL)
    
    if(input$newSB == input$actionSB)
      "Simulation is Finished!"
    else 
      "Setting scenario!"
  })
  
  simulateSB <- eventReactive(input$actionSB, { 
    
    if(!is.null(input$uilogisetexprSB))
        env.scenario <- setGlobalSubgroupFilterExpression(env.scenario, input$uilogisetexprSB)
   
    
    cl <- makeCluster(detectCores()-1)
    
    clusterExport(cl, c("binbreaks", "transition_probabilities", "models", 
                        "PropensityModels", "children"))
    
    clusterEvalQ(cl, {library(simarioV2)})
    clusterSetRNGStream(cl, 1)
   
    env.scenario <- simulatePShiny(cl, env.scenario, 10)
    
    stopCluster(cl)

    temp <- savedScenario
    temp$X <- env.scenario
    names(temp)[length(temp)] <-env.scenario$name
    savedScenario <<- temp
    
    print("Simulation is Finished!" )
  })

  output$resultSB  <- renderPrint({
    changeSB()
    
    simulateSB()
  })
  
  
  #################################################################################################################
  #Scenario Builder
  
  loadSB <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
  
    temp <- savedScenario
    
    load(inFile$datapath)
    temp <- c(temp, savedScenario)
    savedScenario <<- temp  
      
    temp
  })
  
  
  output$selectSB <- renderUI({
   
    if(input$actionSB == 0)
      selectInput("selSB", "Select saved Scenario:",
                  choices = c(names(loadSB())))
    else {
      simulateSB()
      selectInput("selSB", "Select saved Scenario:",
                choices = c(names(savedScenario)))
    }
  })
  
  
 
  
#######################################################################################################
#Table builder

  
  output$uiTB <- renderUI({
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type_TB,
           "Percentage" = selectInput("dynamicTB", "Variable",choices = sort(freqList$Name)),
           "Means" = selectInput("dynamicTB", "Variable", choices =  sort(meansList$Name)),
           "Quantiles" = selectInput("dynamicTB", "Variable", choices =  sort(quantilesList$Name))
    )
  })
  
  output$uiSubGrpTB <- renderUI({

    input$input_type_TB
  

    selectInput("subGrp_TB", "Select ByGroup:",
                choices = c(None='',  sort(freqList$Name)))
  })
  
  
  output$uiExprTB <- renderUI({
    input$input_type_TB
    
    selectInput("subGrp_TB1", "Select Subgroup for subgroup formula:",
                choices = c(None='',  varList$Name ))
  })
   
  output$uiExprTB1 <- renderUI({
  
    #print(names(env.base$dict$codings[[names(which(varName == input$subGrp_TB1))]]))
    if(input$subGrp_TB1 == "")
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
  

  logisetexprTB <-eventReactive(input$operatorTB, {
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
  
  finalFormula <<- NULL
  finalFormulaPrint <<- NULL
  
  finalForm <- reactive({
    
    finalFormulaPrint <<-
      switch(input$operatorTB, 
             "And" = paste(finalFormulaPrint, logisetexprTB1(), "&"),
             "Or" = paste(finalFormulaPrint, logisetexprTB1(), "|"),
             "Complete" = paste(finalFormulaPrint, logisetexprTB1(), ""),
             "Reset" = ""
      )
    
    finalFormula <<-
    switch(input$operatorTB, 
         "And" = paste(finalFormula, logisetexprTB(), "&"),
         "Or" = paste(finalFormula, logisetexprTB(), "|"),
         "Complete" = paste(finalFormula, logisetexprTB(), ""),
         "Reset" = ""
    )
    finalFormula
  })
  
    
  output$uilogisetexprTB <- renderUI({  
    textareaInput("logisetexprTB",  "Subgroup formula:", value = finalForm())
  })
  
  
  baseTB <<- NULL 
  
  summaryOutputTB <- eventReactive( input$actionTB, { 
    
    inputType = c("frequencies", "means", "quantiles")
    
    names(inputType) = c("Percentage", "Means","Quantiles" )
    
    grpbyName = varList$Var[varList$Name==input$subGrp_TB] 
    
    print(grpbyName)
    
    if(length(grpbyName) == 0) grpbyName = ""
 
    #browser()
    
    results <- tableBuilderNew(env.base, 
                 statistic = inputType[input$input_type_TB], 
                 variableName = varList$Var[varList$Name==input$dynamicTB],
                    grpbyName = grpbyName, CI = input$ci, 
                 logisetexpr = trimws(input$logisetexprTB))
    
    baseTB <<- results
    
    results
  })
  
  tableResult <<- list()
  
  output$resultTB  <- DT::renderDataTable({

    results <- summaryOutputTB()
    
    if(input$input_type_TB == "Percentage" & any(names(results) %in% "groupByData")){
      
      results <- dcast(melt(results, id.vars = c("Var", "groupByData", "Year")), 
            Year~  groupByData + Var + variable)
      
    }else if(input$input_type_TB == "Percentage"){
    
      results <- dcast(melt(results, id.vars = c("Var", "Year")), 
                       Year~ Var + variable)

    } else if((input$input_type_TB == "Means" | input$input_type_TB == "Quantiles") & 
              any(names(results) %in% "groupByData") ){

      results <- dcast(melt(results, id.vars = c("groupByData", "Year")), 
                       Year ~ groupByData + variable)
      
    } else if((input$input_type_TB == "Means" | input$input_type_TB == "Quantiles") &
              names(results) %in% "Var"){
    
      return(NULL)
    }    
    
    index <- c(grep("Lower", colnames(results)), grep("Upper", colnames(results)))
    
    if(!input$ci)
      results <- results[,-index]
    
    temp <- tableResult
    temp$Base <-results
    tableResult <<- temp
    
   
    
    DT::datatable(results, rownames = FALSE, 
              options = list(pageLength = 21, dom = 't',
                             scrollX = TRUE))
  })
  
  
  SBTB <<- NULL
  
  summaryOutputSBTB <- eventReactive( input$actionTB, {
    
    inputType = c("frequencies", "means", "quantiles")
    
    names(inputType) = c("Percentage", "Means","Quantiles" )
    
    grpbyName = varList$Var[varList$Name==input$subGrp_TB] 
    
    print(input$selSB)
    
    if(length(grpbyName) == 0) grpbyName = ""

    results <-tableBuilderNew(savedScenario[[input$selSB]], 
          statistic = inputType[input$input_type_TB], 
          variableName = varList$Var[varList$Name==input$dynamicTB],
          grpbyName = grpbyName, CI = input$ci, 
          logisetexpr = trimws(input$logisetexprTB))

    SBTB <<-results
    
    results
  })
  
  output$resultSBTB  <- DT::renderDataTable({

    results <- summaryOutputSBTB()
    
    if(input$input_type_TB == "Percentage" & any(names(results) %in% "groupByData")){
      results <- dcast(melt(results, id.vars = c("Var", "groupByData", "Year")), 
                       Year~  groupByData + Var + variable)
      
    }else if(input$input_type_TB == "Percentage"){
      results <- dcast(melt(results, id.vars = c("Var", "Year")), 
                       Year~ Var + variable)
      
    } else if((input$input_type_TB == "Means" | input$input_type_TB == "Quantiles") & 
              any(names(results) %in% "groupByData") ){
      
      results <- dcast(melt(results, id.vars = c("groupByData", "Year")), 
                       Year ~ groupByData + variable)
      
    } else if((input$input_type_TB == "Means" | input$input_type_TB == "Quantiles") &
              names(results) %in% "Var"){
      
      return(NULL)
    }    
    
    index <- c(grep("Lower", colnames(results)), grep("Upper", colnames(results)))
    
    if(!input$ci)
      results <- results[,-index]
    
    
    temp <- tableResult
    temp$Scenario <-results
    names(temp)[2] <- input$selSB
    
    tableResult <<- temp
    
    DT::datatable(results, rownames = FALSE, 
                options = list(pageLength = 21, dom = 't',
                               scrollX = TRUE))
  })
  
 
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste('Table Result-',  input$input_type_TB, " ", 
                              input$dynamicTB, " ", 
                              input$selSB, '.xlsx', sep='')
    },
    content = function(con) {
      
      print(finalFormulaPrint)
      
      temp <- data.frame(Variable = input$dynamicTB)
      
      if(input$selSB != "")
        temp$Scenario = input$selSB
      
      if(!is.null(finalFormulaPrint))
        temp$SubgroupFormula = finalFormulaPrint
      
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
  
  output$uiVar <- renderUI({
    selectInput("Var_TB", "Select a level to compare:",  
                selected = unique(combineResults()$Var)[2], 
                choices = unique(combineResults()$Var))
  })
  
  
  output$barchart<- renderPlotly({
   
    tables.list <- combineResults()
    
    colname <- names(summaryOutputTB())
    
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    if(input$input_type_TB == "Percentage")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ]
    
    p <- 
    ggplot(tables.list, aes(fill=Scenario, y = Mean, x = Year)) +  
      ggtitle(varList$Name[varList$Name==input$dynamicTB]) + 
      geom_bar(position="dodge", stat = "identity")
    
    
    if(any(grepl("Lower", colname)))
      p <- p + geom_errorbar(limitsGGplot, position=dodge, width=0.25)
    
    ggplotly(p)
  })
  
  output$linePlot<- renderPlotly({
    
    tables.list <- combineResults()
    
    colname <- names(summaryOutputTB())
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    if(input$input_type_TB == "Percentage")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ] 
    
    p <-
    ggplot(tables.list, aes(colour = Scenario, y = Mean, x = Year)) +  
      ggtitle(varList$Name[varList$Name==input$dynamicTB]) +  geom_path()

    if(any(grepl("Lower", colname)))
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
      
      save(savedScenario, file = file)
    }
  )
  

  
  
})  