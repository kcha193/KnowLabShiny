
library(shiny)
library(shinydashboard)
library(DT)
library(rhandsontable)
library(snowfall)
library(stringr)
library(stringi)
library(simarioV2)


dashboardPage(skin = "red",
  dashboardHeader(title = "Knowledge Lab"),
  # Application title
  dashboardSidebar(
    sidebarMenu(
      menuItem("First Page", tabName = "fb"),
      menuItem("Base Summary", tabName = "bs"),
      menuItem("Table Builder", tabName = "tb"),
      menuItem("Scenario Builder", tabName = "sb")
    )
  ),
  dashboardBody(tabItems(
  
    tabItem("fb", 
            fluidRow(box(a(href="http://www.arts.auckland.ac.nz/en/about/our-research/research-centres-and-archives/compass.html",
                           img(src="http://www.arts.auckland.ac.nz/en/about/our-research/research-centres-and-archives/compass/_jcr_content/par/textimage/image.img.png/1443396492336.png", 
                               width = 300))),
            box("Write Something here!"),
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
  
    tabItem("tb",
           # Sidebar with a slider input for the number of bins
           fluidRow(
             box(title ="Variable", 
                 status = "primary",solidHeader = TRUE,
               #selectInput("env", "Select Scenario", choices = NULL),
               selectInput("input_type_TB", "Select Summary Measure",
                           c("Percentage", "Means","Quantiles" )),
               uiOutput("uiTB"),
               uiOutput("uiSubGrpTB"),
               checkboxInput("ci", label = "Confidence Interval", value = TRUE),
               actionButton("actionTB", label = "Show"),  
               width = 2
             ),
             # Show a plot of the generated distribution
             box(title = "Base",status = "success", solidHeader = TRUE,
               dataTableOutput('resultTB'),  width = 5),
             box(title = "Scenario",status = "info", solidHeader = TRUE,
               dataTableOutput('resultSBTB'),  width = 5)
             )),
  
    tabItem("sb",
           fluidRow(
             box(title ="Variable", 
                 status = "primary",solidHeader = TRUE,
                uiOutput("uiSB"),
               #selectInput("subGrp_SB", "Subgroup", choices = NULL),
               #textInput("subGrpFor_SB", "Subgroup Formula"),
               selectInput("nRun", "Number of Runs:", c(1:10), selected = 4),
               actionButton("actionPreviewSB", label = "Preview"),
               actionButton("actionSB", label = "Run Scenario"),  width = 4
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
               verbatimTextOutput('resultSB'), width = 12),  width = 8
             )))
))
)