

library(shiny)
library(shinydashboard)
library(DT)
library(rhandsontable)
library(plotly)
library(visNetwork)
library(rdrop2)

options(shiny.maxRequestSize=10000*1024^2)

dashboardPage(skin = "red", title = "Knowledge Lab",
              dashboardHeader(title = "Knowledge Lab" ),
              # Application title
              dashboardSidebar(
                sidebarMenu( 
                  id = "tabs",
                  menuItem("First Page", tabName = "fb", icon = icon("home")),
                  menuItem("Model input", tabName = "mi", icon = icon("line-chart")),
                  menuItem("Scenario Builder", tabName = "sb", icon = icon("refresh")),
                  menuItem("Table Builder", tabName = "tb", icon = icon("table")),
                  br(),
                  box(title ="Project upload", background ="black", status = "primary",solidHeader = TRUE, 
                      fileInput('file1', 'Choose Project File', accept=c('.RData')),  
                      width = 12),
                  br(),
                  box(title ="Saved Scenarios", background ="black",  status = "primary",solidHeader = TRUE, 
                      uiOutput("selectSB"), width = 12),
                  br(),
                  box(textInput("wrkSpaceName", label = "Name the Project:"),
                      downloadButton('saveWrkspace', "Save Project") ,
                      h3("Latest Update:"),
                      h4("2016-12-15"),
                      h3("Contact email:"), 
                      a("k.chang@auckland.ac.nz", 
                        href= "mailto:k.chang@auckland.ac.nz"), 
                      width = 12, background ="black"))), 
              dashboardBody(
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
                  tags$style(HTML('
                                  .main-header .logo {
                                  font-weight: bold;
                                  font-size: 24px;
                                  }
                                  '))),
                tabItems( 
                  tabItem("fb", 
                          fluidRow(
                            box( width = 6, 
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
                                 p(""),
                                 a(href="http://www.arts.auckland.ac.nz/en/about/our-research/research-centres-and-archives/compass.html",
                                   img(src="http://www.arts.auckland.ac.nz/en/about/our-research/research-centres-and-archives/compass/_jcr_content/par/textimage/image.img.png/1443396492336.png", 
                                       width = 200)))  #, 
                            #box( width = 6,  includeHTML("ppt.html"))
                                 )),
                  tabItem("mi", "Hover to the line to examine the coefficients or the odds ratio of the models. 
                          Click on the line to open a new webpage of the citiation. 
                          Hover to the eclipse to examine the levels of the variables.
                          Click on the eclipse to select the variable for the scenario builder or the table builder.", 
                          visNetworkOutput('oModel', width = "100%", height = "800px")),
                  tabItem("sb",
                          fluidRow(
                            box(title ="Variable", width = 3,
                                status = "primary",solidHeader = TRUE,
                                uiOutput("uiNameSB"),
                                uiOutput("uiSB"),
                                uiOutput("uiSubGrpSB"),
                                uiOutput("uiExprSB"),
                                uiOutput("uiExprSB1"),
                                actionButton("completeSB", "Insert"),
                                actionButton("leftBrackSB", "("),
                                actionButton("rightBrackSB", ")"),
                                actionButton("andSB", "And"),
                                actionButton("orSB", "Or"),
                                actionButton("resetSB", "Reset"),
                                uiOutput("uilogisetexprSB"),
                                # actionButton("preview_SB", label = "Preview"),
                                actionButton("actionAddSB", label = "Add Scenario"),
                                selectInput("nRun", "Number of Runs:", c(1:10), selected = 10),
                                actionButton("actionSB", label = "Run Scenario"),
                                h4("Scenario simulation log:"),
                                h4(strong(htmlOutput('StartSim')))),
                            # Show a plot of the generated distribution
                            box(title ="Setting the Scenario", status = "warning", solidHeader = TRUE,
                                box (title = "Variable Adjustment", status = "success", solidHeader = TRUE,
                                     rHandsontableOutput("hotable"), width = 6, height = 600),
                                box (title = "Base value", status = "info", solidHeader = TRUE, 
                                     dataTableOutput("previewSB"),  width = 6, height = 780),  width = 9))),
                  tabItem("tb",
                          # Sidebar with a slider input for the number of bins
                          fluidRow(
                            box(title ="Variable",  width = 3,
                                status = "primary",solidHeader = TRUE,
                                selectInput("input_type_TB", "Select Summary Measure",
                                            c("Percentage", "Mean","Quantile" )),
                                uiOutput("uiTB"),
                                uiOutput("uiVar"),
                                uiOutput("uiSubGrpTB"),
                                uiOutput("uiExprTB"),
                                uiOutput("uiExprTB1"),
                                actionButton("completeTB", "Insert"),
                                actionButton("leftBrackTB", "("),
                                actionButton("rightBrackTB", ")"),
                                actionButton("andTB", "And"),
                                actionButton("orTB", "Or"),
                                actionButton("resetTB", "Reset"),
                                uiOutput("uilogisetexprTB"),
                                selectInput("basePop", "Apply subgroup to:", 
                                            c("Base population (Before scenario testing)", 
                                              "Scenario population (After scenario testing)"), 
                                            selected = "Scenario population (After scenario testing)"),
                                checkboxInput("ci", label = "Confidence Interval", value = TRUE),
                                #actionButton("reset", label = "Reset"),
                                #actionButton("actionTB", label = "Show"),
                                br(),
                                downloadButton('downloadTable', 'Download Table'),
                                br(),
                                downloadButton('downloadPlot', 'Download Plot')
                            ),
                            # Show a plot of the generated distribution
                            tabBox(width = 9, height = "750px", id = "mainTabset",
                                   tabPanel(title = "Base", dataTableOutput('resultTB')),
                                   tabPanel(title = "Scenario",  dataTableOutput('resultSBTB')),
                                   navbarMenu("Barchart (Percentage and Mean only)" ,
                                              tabPanel("Base only", plotlyOutput("barchartBase", width = "90%", height = "700px")),  
                                              tabPanel("Scenario only", plotlyOutput("barchartSC", width = "90%", height = "700px")),
                                              tabPanel("Base versus Scenario", plotlyOutput("barchart", width = "90%", height = "700px"))),
                                   navbarMenu("Line plot (Percentage and Mean only)" ,
                                              tabPanel("Base only", plotlyOutput("linePlotBase", width = "90%", height = "700px")),  
                                              tabPanel("Scenario only", plotlyOutput("linePlotSC", width = "90%", height = "700px")),
                                              tabPanel("Base versus Scenario", plotlyOutput("linePlot", width = "90%", height = "700px"))),
                                   navbarMenu("Box plot (Quantile only)" ,
                                              tabPanel("Base only", plotOutput("boxPlotBase", width = "90%", height = "700px")),  
                                              tabPanel("Scenario only", plotOutput("boxPlotSC", width = "90%", height = "700px")),
                                              tabPanel("Base versus Scenario", plotOutput("boxPlot", width = "90%", height = "700px")))
                                   
                            )))
                  )))


