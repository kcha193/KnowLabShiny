


library(shiny)
library(shinydashboard)
library(DT)
library(rhandsontable)
library(snowfall)
library(stringr)
library(stringi)
library(simarioV2)
library(plotly)
library(dplyr)
library(tidyr)

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
  
  
  varName <- env.base$dict$descriptions 
  
  #varName <- varName[intersect(names(env.base$simframe), names(varName))]
  
  
  # freqList1 = sort(as.character(varName[names(env.base$modules[[1]]$run_results_collated[["freqs"]])]))
  # freqListByGroup1 = sort(as.character(varName[names(env.base$modules[[1]]$run_results_collated[["freqs_continuousGrouped"]])]))
  # meansList1 = sort(as.character(varName[names(env.base$modules[[1]]$run_results_collated[["means"]])]))
  # quantilesList1 =   sort(as.character(varName[names(env.base$modules[[1]]$run_results_collated[["quantiles"]])]))
  # 
  # output$ui <- renderUI({
  #   
  #   # Depending on input$input_type, we'll generate a different
  #   # UI component and send it to the client.
  #   switch(input$input_type,
  #          "Percentage" = selectInput("dynamic", "Variable",freqList1),
  #          "Percentage - continous grouped" = selectInput("dynamic", "Variable",freqListByGroup1),
  #          "Means" = selectInput("dynamic", "Variable", meansList1),
  #          "Quantiles" = selectInput("dynamic", "Variable", quantilesList1)
  #   )
  # })
  # 
  # 
  # summaryOutput <- reactive({ 
  #   
  #   inputType = c("freqs", "freqs_continuousGrouped", "means", "quantiles")
  #   
  #   names(inputType) = c("Percentage", "Percentage - continous grouped", "Means","Quantiles" )
  #   
  #   varName = switch(input$input_type,
  #                    "Percentage" = varName[names(env.base$modules[[1]]$run_results_collated[["freqs"]])],
  #                    "Percentage - continous grouped" = varName[names(env.base$modules[[1]]$run_results_collated[["freqs_continuousGrouped"]])],
  #                    "Means" = varName[names(env.base$modules[[1]]$run_results_collated[["means"]])],
  #                    "Quantiles" = varName[names(env.base$modules[[1]]$run_results_collated[["quantiles"]])])
  #   
  # 
  #   results <- env.base$modules[[1]]$run_results_collated[[
  #     inputType[input$input_type]]][[names(which(trimws(varName) == trimws(input$dynamic)))]]
  #   
  #   if(is.numeric(results))
  #     as.data.frame(results)
  #   else 
  #     results
  #   
  # })
  # 
  # output$result  <- DT::renderDataTable({
  #   input$previewBS
  #   isolate( 
  #     summaryOutput()
  #   )   
  # }, rownames = TRUE, options = list(pageLength = 21))
  # 
  # output$downloadData <- downloadHandler(
  #   filename = function() { 
  #     paste(input$input_type, '.csv', sep='') 
  #   },
  #   content = function(file) {
  #     write.csv(summaryOutput(), file)
  #   }
  # )
  
  
  #################################################################################################################
  #Scenario Builder
  catvars <- c(getOutcomeVars(initialSim$simframe, "categorical"), "SESBTH", 
               "r1stmeduc", "r1stfeduc", "fage",
               "z1single0Lvl1", "bthorder", "NPRESCH", "z1genderLvl1")
  contvars <- c(getOutcomeVars(initialSim$simframe, "continuous"), "bwkg", "pregalc", 
                "ga", "INTERACT", "PUNISH", "MAGE", "pregsmk")
  
  freqList <-  data.frame(Var =catvars, Name = as.character(trimws(varName[catvars])), stringsAsFactors = FALSE)
  
  freqList <- freqList[order(freqList$Name),]
  
  meansList <-  quantilesList <-data.frame(Var = contvars, Name = as.character(trimws(varName[contvars])), stringsAsFactors = FALSE)
  
  meansList <- meansList[order(meansList$Name),]  
  quantilesList <- quantilesList[order(quantilesList$Name),]  
  
  varList <- rbind(meansList,freqList )  
  
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
   
    results <- tableBuilderNew(env.base, statistic = "frequencies", 
             dict = env.base$dict,
             variableName = rev(names(which(trimws(varName) == trimws(input$var_SB))))[1], 
             grpbyName = "", logisetexpr="")
    
    
    results %>% select(Var, Year, Mean) %>% spread(Var, Mean)
    
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
  

  output$uiExprSB <- renderUI({
     
    selectInput("subGrp_SB", "Select Subgroup for subgroup formula:",
                choices = c(None='',  c(freqList$Name,meansList$Name)), selectize=FALSE)
  })
  
  output$uiExprSB1 <- renderUI({
    
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
      paste(varList$Var[varList$Name == input$subGrp_SB], index, sep = "==")
    }
    
  })
  
  finalFormulaSB <<- NULL
  
  finalFormSB <- reactive({
    
    finalFormulaSB <<-
      switch(input$operatorSB, 
             "And" = paste(finalFormulaSB, logisetexprSB(), " & "),
             "Or" = paste(finalFormulaSB, logisetexprSB(), " | "),
             "Complete" = paste(finalFormulaSB, logisetexprSB(), ""),
             "Reset" = ""
      )
    finalFormulaSB
  })
  
  
  output$uilogisetexprSB <- renderUI({  
    textareaInput("logisetexprSB",  "Subgroup formula:", value = finalFormSB())
  })
  
  startsim <- eventReactive(input$actionSB, { return("Start simulation!")})
  
  
  simulateSB <- eventReactive(input$actionSB, { 
    
    if(!is.null(input$uilogisetexprSB))
        env.scenario <- setGlobalSubgroupFilterExpression(env.scenario, input$uilogisetexprSB)
   
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
  
  
  output$StartSim <-renderPrint({
    print(startsim())
  })
  
  output$resultSB  <- renderPrint({

   
    changeSB()

    
    print( simulateSB())
  })
  
 
  
#######################################################################################################
#Table builder

  
  output$uiTB <- renderUI({
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type_TB,
           "Percentage" = selectInput("dynamicTB", "Variable",choices = freqList$Name),
           "Means" = selectInput("dynamicTB", "Variable", choices =  meansList$Name),
           "Quantiles" = selectInput("dynamicTB", "Variable", choices =  quantilesList$Name)
    )
  })
  
  output$uiSubGrpTB <- renderUI({
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.

    selectInput("subGrp_TB", "Select ByGroup:",
                choices = c(None='',  freqList$Name), selectize=FALSE)
  })
  
  
  output$uiExprTB <- renderUI({
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    
    selectInput("subGrp_TB1", "Select Subgroup for subgroup formula:",
                choices = c(None='',  c(freqList$Name,meansList$Name)), selectize=FALSE)
  })
   
  output$uiExprTB1 <- renderUI({
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    
    #print(names(env.base$dict$codings[[names(which(varName == input$subGrp_TB1))]]))
    
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
      (names(env.base$dict$codings[[varList$Var[varList$Name == input$subGrp_TB1]]])==input$subGrp_TB2)]

    print(index)
    if(is.null(index)){
      paste(varList$Var[varList$Name == input$subGrp_TB1], paste(input$subGrp_TB2, input$subGrpNum_TB2), sep = " ")    
    }else{
      paste(varList$Var[varList$Name == input$subGrp_TB1], index, sep = " == ")
    }

  })
  
  finalFormula <<- NULL
  
  finalForm <- reactive({
    
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
  
  summaryOutputTB <- eventReactive( input$actionTB, { #print(getwd())
    
    
    inputType = c("frequencies", "means", "quintiles")
    
    names(inputType) = c("Percentage", "Means","Quantiles" )
    
    grpbyName = varList$Var[varList$Name==input$subGrp_TB] 
    
    print(grpbyName)
    
    if(length(grpbyName) == 0) grpbyName = ""
 
    results <- tableBuilderNew(env.base, 
                 statistic = inputType[input$input_type_TB], 
                 variableName = varList$Var[varList$Name==input$dynamicTB],
                    grpbyName = grpbyName, CI = input$ci, 
                 logisetexpr = trimws(input$logisetexprTB))
      
   
    baseTB <<- results
    
    results
  })
  
  
  output$resultTB  <- DT::renderDataTable({

      DT::datatable( summaryOutputTB(), rownames = TRUE, 
                     extensions = 'Buttons',  
                     options = list(pageLength = 21, dom = 'Bfrtip',
                                    buttons = 
                                      list('copy', 'print', list(
                                        extend = 'collection',
                                        buttons = c('csv', 'excel', 'pdf'),
                                        text = 'Download'
                                      ))))
  })
  
  
  SBTB <<- NULL
  
  summaryOutputSBTB <- eventReactive( input$actionTB, {
    
    print(names(which(varName == input$subGrp_TB)))
    
    inputType = c("frequencies", "means", "quintiles")
    
    names(inputType) = c("Percentage", "Means","Quantile" )
    
    grpbyName = varList$Var[varList$Name==input$subGrp_TB] 
    
    if(length(grpbyName) == 0) grpbyName = ""


    results <-tableBuilderNew(env.scenario, 
         statistic = inputType[input$input_type_TB],
         variableName = varList$Var[varList$Name==input$dynamicTB],
         grpbyName = rev(grpbyName)[1], CI = input$ci, 
         logisetexpr=trimws(input$logisetexprTB))


  
    SBTB <<-results
    
    results
  })
  
 
  
  output$resultSBTB  <- DT::renderDataTable({

      datatable(summaryOutputSBTB(), rownames = TRUE, 
                extensions = 'Buttons',  
                options = list(pageLength = 21, dom = 'Bfrtip',
                               buttons = 
                                 list('copy', 'print', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 ))))
 
  })
  
 
  combineResults <- reactive({
    
    baseTB <-summaryOutputTB()
  
    
    SBTB <- try(summaryOutputSBTB(), silent = TRUE)
    if(class(SBTB) == "try-error") SBTB <- NULL
    
    colname <- names(baseTB)
    
    
    combineResults <- data.frame(Scenario = "Base", baseTB)
    
    if(!is.null(SBTB))
      combineResults <- rbind(combineResults,data.frame(Scenario = "Scenario", SBTB))
    
    combineResults
  })
  
  output$uiVar <- renderUI({
    selectInput("Var_TB", "Select Variable Group:",  
                selected = unique(combineResults()$Var)[1], 
                choices = unique(combineResults()$Var), selectize=FALSE)
  })
  
  
  # output$uiGrpBy <- renderUI({
  #   selectInput("subGrp_TB", "Select ByGroup:",
  #               selected = unique(combineResults()$groupByData)[1], 
  #               choices = unique(combineResults()$groupByData), selectize=FALSE)
  # })
  
  
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
  
})  