
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(DT)
library(dygraphs)

dashboardPage(
  dashboardHeader(title = "Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("General", tabName = "general", icon = icon("dashboard")),
      menuItem("Plotting", tabName = "plotting", icon = icon("bar-chart")),
      menuItem("Segmenting", tabName = "segmenting", icon = icon("columns")),
      menuItem("Biclustering", tabName = "biclustering", icon = icon("th")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "general",
        fluidRow(
        
          box(
            width = 12,
            title = "Dataset option",
            # sliderInput("slider", "Number of observations:", 1, 100, 50)
            
  
            fluidRow(
              box(
                width = 3,
                radioButtons('sep', 'Separator',
                             c(Comma=',',
                               Semicolon=';',
                               Tab='\t'),
                             ',')
              ),
              box(
                width = 3,
                radioButtons('quote', 'Quote',
                             c(None='',
                               'Double Quote'='"',
                               'Single Quote'="'"),
                             '"')
              ),
              box(
                width = 3,
                checkboxInput('header', 'Header', TRUE)
                
              ),
              box(
                width = 3,
                checkboxInput('normalizing', 'Normalizing', TRUE)
              )
            )
          ),
          
          
          box(
            width = 12,
            title = "Upload",
            
            fileInput('file1', 'Choose CSV File',
                      accept=c('text/csv',
                               'text/comma-separated-values,text/plain',
                               '.csv'))
          ),
          
          tabBox(
            width = 12,
            tabPanel("Summary", verbatimTextOutput("summary")),
            # tabPanel("Data Table", tableOutput("table"))
            tabPanel("Data Table", DT::dataTableOutput('table'))
            
          )
        )
      ),
      # </general>
      
      
      tabItem(tabName = 'plotting',
        fluidRow(
          box(
            width = 6,
            selectInput('plotY', 'Y Varaible(s)', choices = c('Please select a dataset'), multiple = T),
            selectInput('plotX', 'X Varaible', choices = c('Please select a dataset'), multiple = F)
           
            # 
            # tags$hr(),
            # 
            # 
            # actionButton("plotButton", "Plot"),
            # actionButton("mplotButton", "Multi-plot"),
            # actionButton("corButton", "Correlation")
            # 
          ),
          
          tabBox(
            width = 12,
            tabPanel("Plot", dygraphOutput("plot"))
            ,tabPanel("Multi-plot", uiOutput("mplot"))
            ,tabPanel("Correlation", plotOutput("corplot"))
            # tabPanel("Data Table", DT::dataTableOutput('table'))
            
          )
          # ,
          
          # box(
          #   width = 12,
          #   title = 'Plot',
          #   dygraphOutput("plot")
          # )
        )
        
      ),
      #</plotting>
      
      tabItem(tabName = 'segmenting',
        
        fluidRow(
          box(
            width = 12,
            title = 'General settings',
            
            fluidRow(
              box(
                width = 6,
                sliderInput("windowSize", "Window Size input:",
                            min = 1, max = 1000, value = 100),
                
                sliderInput("overlap", "Overlap input:",
                            min = 0, max = 1, value = 0.5),
                
                sliderInput("threshold", "Threshold input:",
                            min = 0, max = 1, value = 0.9),
                
                radioButtons("univariate", "Variate type:",
                             c("univariate" = 1,
                               "multi-variate" = 0))
              ),
              box(
                width = 6,
                selectInput('segExcV', 'Excluding Variable(s)', choices = c('Please select a dataset'), multiple = T)
                ,icon("info-circle"),"Select variable(s) to be excluded from segmentation."
              ),
              box(
                width = 6,
                selectInput('segIndV', 'Individually-tuned Variable(s)', choices = c('Please select a dataset'), multiple = T)
                ,icon("info-circle"),"Select variable(s) to create an individual setting tab to tune parameters.")
              )
              
            
            
          ),
          
          uiOutput('segIndTabs'),
          
          
          actionButton('segbutton', 'Start Segmenting'),
          
          box(
            title = 'Plot',
            width = '12',
           
            
            plotOutput("segplot")
            # verbatimTextOutput("segplot")
          )
        )
      )
      #</segmenting>
      
    )
  )
)
