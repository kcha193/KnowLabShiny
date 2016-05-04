
library(shiny)
library(shinydashboard)
library(DT)
library(rhandsontable)
library(snowfall)
library(stringr)
library(stringi)
library(simarioV2)

# Define UI for application that draws a histogram
dashboardPage(skin = "red",
  dashboardHeader(title = "Knowledge Lab"),
  # Application title
  dashboardSidebar(
    sidebarMenu(
      menuItem("Base Summary", tabName = "bs"),
      menuItem("Table Builder", tabName = "tb"),
      menuItem("Scenario Builder", tabName = "sb"),
      menuItem("Table Builder on new Scenario", tabName = "sbtb")
    )
  ),
  dashboardBody(tabItems(
  
    tabItem("bs",
           # Sidebar with a slider input for the number of bins
           fluidRow(
             box(
               selectInput("input_type", "Summary Statistics",
                           c("Percentage", "Percentage - continous grouped", "Means","Quantiles" )),
               
               uiOutput("ui"),
               actionButton("previewBS", label = "Preview"),
			   downloadButton('downloadData', 'Download'),
               #selectizeInput('freqs', 'Freq', choices = NULL)
               width = 4),
             
             # Show a plot of the generated distribution
			   box(
               dataTableOutput('result'), width = 8
             ))),
  
  
    tabItem("tb",
           # Sidebar with a slider input for the number of bins
           fluidRow(
             box(
               #selectInput("env", "Select Scenario", choices = NULL),
               selectInput("input_type_TB", "Select Summary Measure",
                           c("Percentage", "Means","Quantiles" )),
               uiOutput("uiTB"),
               uiOutput("uiSubGrpTB"),
               checkboxInput("ci", label = "Confidence Interval", value = TRUE),
               actionButton("actionTB", label = "Preview"),  width = 4
               #selectizeInput('freqs', 'Freq', choices = NULL)
             ),
             # Show a plot of the generated distribution
             box(
               dataTableOutput('resultTB'),  width = 8
             ))),
  
  
    tabItem("sb",
           # Sidebar with a slider input for the number of bins
           fluidRow(
             box(
               uiOutput("uiSB"),
               selectInput("subGrp_SB", "Subgroup", choices = NULL),
               textInput("subGrpFor_SB", "Subgroup Formula"),
               selectInput("env_SB", "Name your Scenario", choices = NULL),
               selectInput("nRun", "Number of Runs:", c(1:10), selected = 4),
               actionButton("actionPreviewSB", label = "Preview"),
               actionButton("actionSB", label = "Run Scenario"),  width = 4
               
               #selectizeInput('freqs', 'Freq', choices = NULL)
             ),
             # Show a plot of the generated distribution
             box(
               rHandsontableOutput("hotable"),
               actionButton("actionAddSB", label = "Add Scenario"),
               tableOutput('previewSB'),
               tableOutput('resultSB'),  width = 8
             ))),
    
    tabItem("sbtb",
            # Sidebar with a slider input for the number of bins
            fluidRow(
              box(
                #selectInput("env", "Select Scenario", choices = NULL),
                selectInput("input_type_SBTB", "Select Summary Measure",
                            c("Percentage", "Means","Quantiles" )),
                uiOutput("uiSBTB"),
                uiOutput("uiSubGrpSBTB"),
                checkboxInput("ciSB", label = "Confidence Interval", value = TRUE),
                actionButton("actionSBTB", label = "Preview"),  width = 4
                #selectizeInput('freqs', 'Freq', choices = NULL)
              ),
              # Show a plot of the generated distribution
              box(
                dataTableOutput('resultSBTB'),  width = 8
              )))
    
    
    
  
))
)