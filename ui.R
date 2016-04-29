
library(shiny)
library(DT)
library(rhandsontable)
library(snowfall)
library(stringr)
library(stringi)
library(simarioV2)

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  # Application title
  title = "KnowLab",
  tabPanel("Base Summary",
           # Sidebar with a slider input for the number of bins
           sidebarLayout(
             sidebarPanel(
               selectInput("input_type", "Summary Statistics",
                           c("Percentage", "Percentage - continous grouped", "Means","Quantiles" )),
               
               uiOutput("ui"),
               actionButton("previewBS", label = "Preview"),
			   downloadButton('downloadData', 'Download')
               #selectizeInput('freqs', 'Freq', choices = NULL)
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               dataTableOutput('result')
             ))),
  
  
  tabPanel("Table Builder",
           # Sidebar with a slider input for the number of bins
           sidebarLayout(
             sidebarPanel(
               #selectInput("env", "Select Scenario", choices = NULL),
               selectInput("input_type_TB", "Select Summary Measure",
                           c("Percentage", "Means","Quantiles" )),
               uiOutput("uiTB"),
               uiOutput("uiSubGrpTB"),
               checkboxInput("ci", label = "Confidence Interval", value = TRUE),
               actionButton("actionTB", label = "Preview")
               #selectizeInput('freqs', 'Freq', choices = NULL)
             ),
             # Show a plot of the generated distribution
             mainPanel(
               dataTableOutput('resultTB')
             ))),
  
  
  tabPanel("Scenario Builder",
           # Sidebar with a slider input for the number of bins
           sidebarLayout(
             sidebarPanel(
               uiOutput("uiSB"),
               selectInput("subGrp_SB", "Subgroup", choices = NULL),
               textInput("subGrpFor_SB", "Subgroup Formula"),
               selectInput("env_SB", "Name your Scenario", choices = NULL),
               selectInput("nRun", "Number of Runs:", c(1:10), selected = 4),
               actionButton("actionPreviewSB", label = "Preview"),
               actionButton("actionAddSB", label = "Add Scenario"),
               actionButton("actionSB", label = "Run Scenario")
               
               #selectizeInput('freqs', 'Freq', choices = NULL)
             ),
             # Show a plot of the generated distribution
             mainPanel(
               rHandsontableOutput("hotable"),
               tableOutput('previewSB'),
               tableOutput('resultSB')
             )))
))