

library(shiny)
library(shinydashboard)
library(DT)
library(rhandsontable)
library(plotly)

dashboardPage(skin = "red",
  dashboardHeader(title = "Knowledge Lab"),
  # Application title
  dashboardSidebar(
    sidebarMenu(
      menuItem("First Page", tabName = "fb", icon = icon("home")),
      #menuItem("Base Summary", tabName = "bs", icon = icon("database")),
      menuItem("Scenario Builder", tabName = "sb", icon = icon("refresh")),
      menuItem("Table Builder", tabName = "tb", icon = icon("table"))
    )
  ),
  dashboardBody(tabItems(
  
    tabItem("fb", 
            fluidRow(
            box(h3("Developing a knowledge laboratory of the early life-course using 
                   systematic reviews and meta analyses"),
              p("This is a three -year project funded by the Ministry of Business, Innovation 
                    and Employment through its health and Society  fund in 2013."),
                p("We will identify key determinants of child and adolescent outcomes, 
                and will integrate estimates from systematic reviews and meta-analyses for 
                these determinants into a working model of the early life-course (developed 
                from an existing model we have created). We will use the working model as a 
                \"knowledge laboratory\" to (i) test the validity of the underlying behavioural 
                equations and specific knowledge sources (meta-analyses, systematic reviews), 
                and (ii) test policy scenarios by carrying out experiments on the 'virtual cohort' 
                created by the working model."),
                 p("This research will involve the development of a micro-simulation model and 
                associated computer software that allows users (policy makers, planners, analysts) 
                to easily programme simulations and view the results. The end product will be an 
                expert decision-support tool that will be available to the public policy 
                community."),
                p("The research plan involves (i) identifying published systematic reviews 
                and meta analyses relating to key outcomes for children and adolescents (to age 18); 
                  (ii) integrating estimates from these studies into, and thus enhancing, an existing
                  micro-simulation model of the early life-course; (iii) validating the enhanced model,
                  and thus published estimates, by comparing simulated results to published New Zealand 
                  benchmarks; and (iv) using the validated enhanced model to test the impact of various
                  policies on key child and adolescent outcomes."),
                p("In using these best estimates to develop a micro-simulation model with 
                which policy scenarios can be tested, our proposal will benefit NZ 
                  families/whanau by determining the policies that have the greatest 
                  impact on the lives of New Zealand children. Moreover, we will be
                  uniquely placed to assess the impact of distinctive Maori programmes, 
                  such as Kohanga reo and Whanau Ora."),
              a(href="http://www.arts.auckland.ac.nz/en/about/our-research/research-centres-and-archives/compass.html",
                img(src="http://www.arts.auckland.ac.nz/en/about/our-research/research-centres-and-archives/compass/_jcr_content/par/textimage/image.img.png/1443396492336.png", 
                    width = 200))),
            box("And Here!"))),
    
    tabItem("bs",
           # Sidebar with a slider input for the number of bins
           fluidRow(
             box(title ="Variable", 
                 status = "primary",solidHeader = TRUE,
               selectInput("input_type", "Summary Statistics",
                           c("Percentage", "Percentage - continous grouped", "Means","Quantiles" )),
               
               uiOutput("ui"),
               actionButton("previewBS", label = "Preview"),
			   downloadButton('downloadData', 'Download'),
               #selectizeInput('freqs', 'Freq', choices = NULL)
               width = 4),
             
             # Show a plot of the generated distribution
			   box(title = "Base", status = "success", solidHeader = TRUE,
               dataTableOutput('result'), width = 8
             ))),
    
    tabItem("sb",
            fluidRow(
              box(title ="Variable", 
                  status = "primary",solidHeader = TRUE,
                  uiOutput("uiSB"),
                  uiOutput("uiSubGrpSB"),
                  uiOutput("uiExprSB"),
                  uiOutput("uiExprSB1"),
                  selectizeInput("operatorSB", "Operators (And/Or/Complete/Reset):",
                                 choices = c("And" = "And", "Or" = "Or", "Complete" = "Complete", "Reset" = "Reset"),
                                 options = list(
                                   placeholder = 'Please select an operators below',
                                   onInitialize = I('function() { this.setValue(""); }')
                                 )),
                  uiOutput("uilogisetexprSB"),
                  actionButton("preview_SB", label = "Preview"),
                  selectInput("nRun", "Number of Runs:", c(1:10), selected = 4),
                  actionButton("actionSB", label = "Run Scenario"),  width = 3
              ),
              # Show a plot of the generated distribution
              box(title ="Setting the Scenario", status = "warning", solidHeader = TRUE,
                  box (title = "Cat Adjustment", status = "success", solidHeader = TRUE,
                       rHandsontableOutput("hotable"), width = 6),
                  box (title = "Base value", status = "info", solidHeader = TRUE, 
                       dataTableOutput("previewSB"), width = 6),
                  hr(),
                  box (
                    actionButton("actionAddSB", label = "Add Scenario"),
                    h2("Scenario simulation log:"),
                    verbatimTextOutput('StartSim'),
                    verbatimTextOutput('resultSB'), width = 12),  width = 9
              ))),
  
    tabItem("tb",
           # Sidebar with a slider input for the number of bins
           fluidRow(
             box(title ="Variable",  width = 3,
                 status = "primary",solidHeader = TRUE,
               selectInput("input_type_TB", "Select Summary Measure",
                           c("Percentage", "Means","Quantiles" )),
               uiOutput("uiTB"),
               uiOutput("uiVar"),
               uiOutput("uiSubGrpTB"),
               uiOutput("uiExprTB"),
               uiOutput("uiExprTB1"),
               selectizeInput("operatorTB", "Operators (And/Or/Complete/Reset):",
                           choices = c("And" = "And", "Or" = "Or", "Complete" = "Complete", "Reset" = "Reset"),
                           options = list(
                             placeholder = 'Please select an operators below',
                             onInitialize = I('function() { this.setValue(""); }')
                           )),
               uiOutput("uilogisetexprTB"),
               checkboxInput("ci", label = "Confidence Interval", value = TRUE),
               actionButton("actionTB", label = "Show")
             ),
             # Show a plot of the generated distribution
             tabBox(width = 9,height = "850px",
                tabPanel(title = "Base", dataTableOutput('resultTB')),
                tabPanel(title = "Scenario",  dataTableOutput('resultSBTB')),
                tabPanel("Barchart", plotlyOutput("barchart", width = "70%", height = "700px")),    
                tabPanel("Line plot", plotlyOutput("linePlot", width = "70%", height = "700px"))      
             )))
))
)