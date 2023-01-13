

library(shiny)
library(shinydashboard)
library(DT)
library(rhandsontable)
library(plotly)
library(visNetwork)
library(shinyBS)

options(shiny.maxRequestSize=10000*1024^2)


dbHeader <- dashboardHeader(title = "Knowledge Lab",
                            tags$li(a(href = 'https://drive.google.com/open?id=0ByGI4aqoCDeldHdNaDBLQzFLXzg',
                                      "User guide", target = "_blank"),
                                    class = "dropdown",
                                    tags$style(".main-header {max-height: 58px}"),
                                    tags$style(".main-header .logo {height: 60px}")
                            ))

dSiderBar <- dashboardSidebar(
  sidebarMenu( 
    id = "tabs",
    menuItem("First Page", tabName = "fb", icon = icon("home")),
    #sidebarMenuOutput("menu"),
    menuItem("Model input", tabName = "mi", icon = icon("line-chart")),
    menuItem("Scenario Builder", tabName = "sb", icon = icon("refresh")),
    menuItem("Table Builder", tabName = "tb", icon = icon("table")),
    # bsPopover("menu", "",trigger = "manual",
    #           paste0("Model input - Visualising the Conceptual framework. <br> <br>",
    #                  "Scenario Builder – Simulation with a new Scenario. <br> <br>", 
    #                  "Table Builder - Tabulate and/or plot the simulated outcome and compare to original."),
    #           placement ="right", options = list(container = "body")),
    br(),
    box(title ="Project upload", background ="black", status = "danger",solidHeader = TRUE, 
        fileInput('file1', 'Choose Project File', accept=c('.RData')),  
        width = 12),
    # bsPopover('file1', "",
    #           paste0("Loads saved projects, including scenario settings and results."),
    #           placement ="right", options = list(container = "body")),
    br(),
    box(title ="Scenarios Run", background ="black",  status = "danger",solidHeader = TRUE, 
        uiOutput("selectSB"), width = 12),
    # bsPopover("selectSB", "",
    #           paste0("Lists all scenarios run and allows a scenario to be selected for comparison with base outcomes."),
    #           placement ="right", options = list(container = "body")),
    br(),
    box(textInput("wrkSpaceName", label = "Name the Project:"),
        downloadButton('saveWrkspace', "Save Project"),    
    h5("Latest Update:"),
    h5("2019-03-06"),
    h5("Contact email:"), 
    a("Barry Milne", href= "mailto:b.milne@auckland.ac.nz"),
    br(),
    a("Kevin Chang", href= "mailto:k.chang@auckland.ac.nz"), 
    width = 12, background ="black"), 
    bsPopover("saveWrkspace", "", trigger = "manual",
              paste0("Saves project and its scenario settings and results. "),
              placement ="right", options = list(container = "body"))), 
  tags$style(".left-side, .main-sidebar {padding-top: 60px}"))


dashboardPage(skin = "red", title = "Knowledge Lab",
              dbHeader,
              dSiderBar, 
              dashboardBody(
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
                  tags$head(includeScript("google-analytics.js")),
                  tags$style(HTML('
                                  .main-header .logo {
                                  font-weight: bold;
                                  font-size: 24px;
                                  }
                                  '))),
                tabItems( 
                  tabItem("fb", 
                            # box(title ="Quick Link", width = 12,
                            #     status = "primary",solidHeader = TRUE, 
                            #     p(actionButton("switchMI", "Model input"), "Visualising the Conceptual framework."), 
                            #     p(actionButton("switchSB", "Scenario Builder"), "Simulation with a new Scenario"), 
                            #     p(actionButton("switchTB", "Table Builder"), "Tabulate and/or plot the simulated outcome and compare to original.")
                            #     ),
                            box( width = 12, 
                                 h3("KNOWLEDGE LAB (A knowledge laboratory of the early life-course)"),
                                 p("Knowledge Lab is a microsimulation model of New Zealand children’s development 
                                   from birth to age 21. Micro-simulation is a technique that creates a virtual world 
                                   which mimics the real world, with the population of ‘virtual’ individuals looking very 
                                   much like the population of real individuals – in our case, children developing from birth 
                                   through to early adulthood.  A key feature of microsimulation is that allows virtual experiments 
                                   to be carried out, where the effects of changing aspects of children’s lives can be simulated, 
                                   and the results quantified. What if we could reduce child bullying? What if fewer children had 
                                   ear infections? What if we could improve the diet and activity of children? How would children’s 
                                   lives improve as a result of these changes?  These are the sorts of questions Knowledge Lab has 
                                   been set up to answer."),
                                 p("To construct Knowledge Lab, we first identified key determinants of child and adolescent 
                                   outcomes, in association with policy representative from the New Zealand Ministries of Health,
                                   Education, Social Development and Justice, as well as Te Puni Kōkiri, the Social Policy 
                                   Evaluation and Research Unit (SuPERU), and the Children’s Commission.  We then integrated 
                                   estimates from systematic reviews and meta-analyses for the impact of these determinants 
                                   into a working micro-simulation model of the early life-course, building on an earlier 
                                   microsimulation model we had developed:", a("Modelling the Early life-course.", 
             href="http://www.arts.auckland.ac.nz/en/about/our-research/research-centres-and-archives/compass/compass-projects/a-modelling-tool-to-improve-the-policy-response-on-issues-concerning-children-and-young-people.html")),
                                 p("Steps in this process have involved (i) identifying published systematic reviews 
                                   and meta analyses relating to key outcomes for children and adolescents (to age 21); 
                                   (ii) integrating estimates from these studies into, and thus enhancing, an existing 
                                   micro-simulation model of the early life-course; (iii) validating the enhanced model, 
                                   and thus published estimates, by comparing simulated results to published New Zealand 
                                   benchmarks; and (iv) using the validated enhanced model to test the impact of various 
                                   policies on key child and adolescent outcomes."),
                                 p("The end product is an expert decision-support tool that is available for use by the
                                   public policy community. This tool have been developed as an interactive web application
                                   using Shiny R package and R programming language. Thus, the Shiny app can be shared as a web page, 
                                   which allows the user to run across a number of different platforms, and does not require 
                                   any specialist software to be installed."),
                                p("For the live demonstration video of this Shiny application, please find it in this URL ", 
                                  a("https://youtu.be/3eTUwUIa4jI", href = "https://youtu.be/3eTUwUIa4jI"), "."),
                                h4("To cite this application, please use the following,"),
                                 p("Milne, B., Chang, K., Shackleton, N., Zhu, T., Liu, C., von Randow, M., Lay-Yee, R., McLay, J., Davis, P. (2017) Shiny application: Knowledge laboratory of the early life-course. http://compassnz.shinyapps.io/knowlabshiny/"),
                                 h4("The source code is stored in three places:"), 
                                  HTML("<ul>
                                        <li>Simario R package is at: <a href = \"https://github.com/kcha193/simarioV2\"> https://github.com/kcha193/simarioV2 </a>.</li>
                                        <li>Models of knowledge laboratory is at: <a href = \"https://bitbucket.org/Kcha193/knowlab\"> https://bitbucket.org/Kcha193/knowlab </a>.</li>
                                        <li>Shiny application is at: <a href = \"https://github.com/kcha193/KnowLabShiny\"> https://github.com/kcha193/KnowLabShiny </a>.</li>
                                      </ul>" ),
                                  p(""),
                                    a(href="http://www.arts.auckland.ac.nz/en/about/our-research/research-centres-and-archives/compass.html",
                                   img(src="compass.png", 
                                       width = 200)), 
             br(),br(),
             actionButton(inputId='ab1', label=HTML("<b> <font size=\"4\"> Getting Started </font> </b> <br><b> <font size=\"4\"> (User Guide)  </font></b>"), 
                          width = "200px", hight = "100px",
                          onclick ="window.open('https://drive.google.com/open?id=0ByGI4aqoCDeldHdNaDBLQzFLXzg', '_blank')"), 
             actionButton(inputId='ab1', label=HTML("<b> <font size=\"4\"> Getting Started </font> </b> <br><b> <font size=\"4\"> (Live Demo)  </font></b>"), 
                          width = "200px", hight = "100px",
                          style="color: #fff; background-color: #b7325a; border-color: #b7325a",
                          onclick ="window.open('https://youtu.be/3eTUwUIa4jI', '_blank')")
             )),   
                            #box( width = 6,  includeHTML("ppt.Rhtml"))),
                                
                    tabItem("mi", 
                          fluidRow(
                            column(width = 3,
                            box(title ="Instruction", width = 12,
                               status = "primary",solidHeader = TRUE,
                               tags$head(includeScript("google-analytics.js")),
                    HTML("<p> <b> HOVER OVER </b> an arrow to see the coefficient and citation for that path. </p> <br> <br>
                          <p> <b> CLICK ON </b> an arrow to open the citation for that coefficient.  </p> <br> <br>
                          <p> <b> HOVER OVER </b> a bubble to see the levels of that variable. </p> <br> <br>
                          <p> <b> CLICK ON </b> a bubble to highlight all paths for models involving that variable.  
                                  NB., clicking on a variable will pre-load this variable in scenario builder and 
                                  table builder – click on scenario builder or table builder to go there. </p> ")), 
                    box(title ="Comments and Suggestions", width = 12,
                        status = "warning", solidHeader = TRUE, 
                        paste0("We encourage users to provide comments and suggestions about the ", 
                               "conceptual framework and estimates. In particular, we welcome ", 
                               "suggestions for changes and additions where supporting evidence ",  
                               "from the literature can be provided."), 
                        h5("Contact email:"), 
                        a("Barry Milne", href= "mailto:b.milne@auckland.ac.nz"),
                        br(),
                        a("Kevin Chang", href= "mailto:k.chang@auckland.ac.nz") )) , 
                          box(title ="Conceptual Framework", width = 9,
                              status = "success",solidHeader = TRUE,
                          visNetworkOutput('oModel', width = "100%", height = "800px")))),
                  tabItem("sb",
                          fluidRow(
                            box(title ="Variable", width = 3,
                                status = "primary",solidHeader = TRUE,
                                tags$head(includeScript("google-analytics.js")),
                                uiOutput("uiNameSB"),
                                # bsPopover("uiNameSB", "", trigger = "manual",
                                #           paste0("Type in the box to give your  ",
                                #                  "scenario a name – it can help ", 
                                #                  "to remind you what the ", 
                                #                  "scenario was testing."),
                                #           placement ="right", options = list(container = "body")),
                                uiOutput("uiSB"),
                                bsPopover("uiSB", "", trigger = "manual",
                                          paste0("Choose the variable you want to change. ",
                                                 "Baseline distribution of this variable will ", 
                                                 "appear in the “Base value for the Variable” panel."),
                                          placement ="right", options = list(container = "body")),
                                uiOutput("uiExprSB"),
                                # bsPopover("uiExprSB", "", trigger = "manual",
                                #           paste0("<p> Table will be displayed just for those in the subgroup.",
                                #                  "Choose the variable you want to subgroup by", 
                                #                  "(default is set at “None” for the whole population). ", 
                                #                  "Then choose the subgroup of interest.  </p> <br>",
                                #                  "<p> Different variables will have different options. ",
                                #                  "Categorical variables will list the categories. Select ", 
                                #                  "the category you want (e.g. “Child ethnicity” and “Maori”and then ", 
                                #                  "CLICK “Insert” (IMPORTANT!).  </p> <br>",
                                #                  "<p> Continuous variables will give interval options such as ‘equals’ ",
                                #                  "or ‘less than’ etc, to which you add values in the adjacent box. ", 
                                #                  "E.g., if you want IQ < 100 as your subgroup, select IQ as the variable, ", 
                                #                  "select ‘less than’ as the option, type ‘100’ and ", 
                                #                  "then CLICK “Insert” (IMPORTANT!).  </p> <br>",
                                #                  "<p> Alternatively, you can click in the formula box ",
                                #                  "and edit manually (it helps to know R code if you want ", 
                                #                  "to edit manually)  </p> "),
                                #           placement ="right", options = list(container = "body")),
                                uiOutput("uiExprSB1"),
                                actionButton("completeSB", "Insert"),
                                actionButton("leftBrackSB", "("),
                                actionButton("rightBrackSB", ")"),
                                actionButton("andSB", "And"),
                                actionButton("orSB", "Or"),
                                actionButton("resetSB", "Reset"),
                                uiOutput("uilogisetexprSB"),
                                # actionButton("preview_SB", label = "Preview"),
                                uiOutput("actionAddSBUI"),
                                # bsPopover("actionAddSBUI", "", trigger = "manual",
                                #           paste0("<p> Click “Add Scenario” to update base simulation ",
                                #                  "percentages for the current variable. </p> <br>", 
                                #                  "<p>A summary of your simulation will be shown in the “Scenario simulation log”.</p>"),
                                #           placement ="right", options = list(container = "body")),
                                selectInput("nRun",HTML("<b> <font size=\"4\">STEP 6 (optional): </font></b> Choose number of Runs:"), c(1:10, 15, 20), selected = 10),
                                # bsPopover("nRun", "",trigger = "manual",
                                #           paste0("<p> Choose from the dropdown menu the number of runs ",
                                #                  "for the simulation. The default is set as the maximum run allowed at 10. </p> <br>"),
                                #           placement ="right", options = list(container = "body")),
                                h4("Scenario simulation log:"),
                                h4(strong(htmlOutput('StartSim'))),
                                uiOutput("actionSBUI"),
                                bsPopover("actionSBUI", "",trigger = "manual",
                                          paste0("<p>More runs will give you tighter confidence intervals, but the simulation will take longer to run. </p>"),
                                          placement ="right", options = list(container = "body"))),
                            # Show a plot of the generated distribution
                            box(title ="Setting the Scenario", status = "warning", solidHeader = TRUE,
                                box (title = HTML("<font size=\"4\">STEP 3: </font>Variable Adjustment"), status = "success", 
                                     solidHeader = TRUE,
                                     rHandsontableOutput("hotable"), 
                                     bsPopover("hotable", "",trigger = "manual",
                                               paste0("Change the distribution of this variable to ",
                                                      "that desired. Make sure that the distribution ", 
                                                      "is bounded to 100% in total. "),
                                               placement ="bottom", options = list(container = "body")),
                                     width = 6, height = 600),
                                box (title = "Base value for the Variable:", status = "info", solidHeader = TRUE, 
                                     htmlOutput("SBvar"), dataTableOutput("previewSB"),  
                                    width = 6, height = 780),  width = 9))),
                  tabItem("tb",
                          # Sidebar with a slider input for the number of bins
                          fluidRow(
                            box(title ="Variable",  width = 3,
                                status = "primary",solidHeader = TRUE,
                                tags$head(includeScript("google-analytics.js")),
                                selectInput("input_type_TB", HTML("<b> <font size=\"4\">STEP 1: </font></b> Select Summary Measure"),
                                            c("Percentage", "Mean","Quantile" )),
                                # bsPopover("input_type_TB", "", trigger = "manual",
                                #           paste0("<p> Select either percentages, means or quantiles. Only categorical variables will be",
                                #                  " displayed as percentages and only continuous variables will be displayed as means and quantiles. </p>"),
                                #           placement ="right", options = list(container = "body")),
                                
                                uiOutput("uiTB"),
                                # bsPopover("uiTB", "",trigger = "manual",
                                #           paste0("<p> Select the variable you want to summarise. </p>"),
                                #           placement ="right", options = list(container = "body")),
                                uiOutput("uiVar"),
                                # bsPopover("uiVar", "",trigger = "manual",
                                #           paste0( "<p> Select level you want to display in plot (categorical variables only). </p>"),
                                #           placement ="right", options = list(container = "body")),
                                uiOutput("uiSubGrpTB"),
                                uiOutput("uiExprTB"),
                                uiOutput("uiExprTB1"),
                                # bsPopover("uiSubGrpTB", "",
                                #           paste0("<p> Select a grouping variable (optional) if you want tables stratified by a second variable. </p>"),
                                #           placement ="right", options = list(container = "body")),
                                # bsPopover("uiExprTB1", "",trigger = "manual",
                                #           paste0("<p> Table will be displayed just for those in the subgroup.",
                                #                  "Choose the variable you want to subgroup by", 
                                #                  "(default is set at “None” for the whole population). ", 
                                #                  "Then choose the subgroup of interest.  </p> <br>",
                                #                  "<p> Different variables will have different options. ",
                                #                  "Categorical variables will list the categories. Select ", 
                                #                  "the category you want (e.g. “Child ethnicity” and “Maori”and then ", 
                                #                  "CLICK “Insert” (IMPORTANT!).  </p> <br>",
                                #                  "<p> Continuous variables will give interval options such as ‘equals’ ",
                                #                  "or ‘less than’ etc, to which you add values in the adjacent box. ", 
                                #                  "E.g., if you want IQ < 100 as your subgroup, select IQ as the variable, ", 
                                #                  "select ‘less than’ as the option, type ‘100’ and ", 
                                #                  "then CLICK “Insert” (IMPORTANT!).  </p> <br>",
                                #                  "<p> Alternatively, you can click in the formula box ",
                                #                  "and edit manually (it helps to know R code if you want ", 
                                #                  "to edit manually)  </p> "),
                                #           placement ="right", options = list(container = "body")),
                                actionButton("completeTB", "Insert"),
                                actionButton("leftBrackTB", "("),
                                actionButton("rightBrackTB", ")"),
                                actionButton("andTB", "And"),
                                actionButton("orTB", "Or"),
                                actionButton("resetTB", "Reset"),
                                uiOutput("uilogisetexprTB"),
                                 selectInput("basePop", HTML("<b> <font size=\"4\">STEP 5 (optional): </font></b> Apply subgroup to:"), 
                                            c("Base population (Before scenario testing)", 
                                              "Scenario population (After scenario testing)"), 
                                            selected = "Scenario population (After scenario testing)"),
                                # bsPopover("basePop", "",trigger = "manual",
                                #   paste0("<p> Choose <b>Scenario Population</b> if you want to apply the subgroup ",
                                #     "to the population as it is <u>after</u> applying the scenario (the default). </p> <br>", 
                                #     "<p> Choose <b>Base Population</b> if you want to apply the subgroup to the ", 
                                #       "population as it was <u>before</u> applying the scenario.  Select this if you want ", 
                                #     "to see the effect of a scenario specifically for a group that you changed ", 
                                #     "– i.e., if the subgrouping variable is the same as a scenario variable.  ", 
                                #     "For example, you may want to assess the effect of ECE attendance specifically ", 
                                #     "for those who changed from non-attendance to attendance.</p>"),
                                #   placement ="right", options = list(container = "body")),
                                uiOutput("ciUI"),
                                # bsPopover("ciUI", "",trigger = "manual",
                                #           paste0("Tick or untick to show or hide confidence intervals."),
                                #           placement ="right", options = list(container = "body")),
                                uiOutput("downloadUI"),
                                bsPopover("downloadUI", "",trigger = "manual",
                                          paste0("You can save tables in xls format by clicking “Download Table” and save plots in png format by clicking “Download Plot”."),
                                          placement ="right", options = list(container = "body"))
                            ),
                            # Show a plot of the generated distribution
                            tabBox(width = 9, height = "750px", id = "mainTabset",
                                   tabPanel(title = "Base", dataTableOutput('resultTB')),
                                   tabPanel(title = "Scenario",  dataTableOutput('resultSBTB')),
                                   navbarMenu("Barchart (Percentage and Mean only)" ,
                                              tabPanel("Base only", plotlyOutput("barchartBase", width = "90%", height = "700px")),  
                                              tabPanel("Scenario only", plotlyOutput("barchartSC", width = "90%", height = "700px")),
                                              tabPanel("Base versus Scenario",
                                                       #verbatimTextOutput("compare"),
                                                       plotlyOutput("barchart", width = "90%", height = "700px"))),
                                   navbarMenu("Line plot (Percentage and Mean only)" ,
                                              tabPanel("Base only", plotlyOutput("linePlotBase", width = "90%", height = "700px")),  
                                              tabPanel("Scenario only", plotlyOutput("linePlotSC", width = "90%", height = "700px")),
                                              tabPanel("Base versus Scenario", 
                                                       plotlyOutput("linePlot", width = "90%", height = "700px"))),
                                   navbarMenu("Box plot (Quantile only)" ,
                                              tabPanel("Base only", plotOutput("boxPlotBase", width = "90%", height = "700px")),  
                                              tabPanel("Scenario only", plotOutput("boxPlotSC", width = "90%", height = "700px")),
                                              tabPanel("Base versus Scenario", plotOutput("boxPlot", width = "90%", height = "700px")))
                                   
                            )))
                  )))


