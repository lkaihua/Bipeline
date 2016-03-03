
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(dygraphs)

# read external js 
fileName <- "shinyjsSeg.js"
# readLines import file asynchronously and fails shinyjs
# jsCode <- paste(readLines("./shinyjsSeg.js"), collapse=" ")
jsCode <- readChar(fileName, file.info(fileName)$size)

dashboardPage(
  dashboardHeader(title = "Analysis Dashboard"),
  dashboardSidebar(
    # change dashboard sidebar appearance
    shinyjs::inlineCSS(list(
      ".sidebar-menu>li" = "font-size: 18px",
      ".sidebar-menu>li>a>.fa" = "width: 30px"
    )),
    sidebarMenu(
      menuItem("Import", tabName = "import", icon = icon("file-o")),
      menuItem("Pre-processing", tabName = "preprocessing", icon = icon("cogs")),
      menuItem("Plotting", tabName = "plotting", icon = icon("bar-chart")),
      menuItem("Segmenting", tabName = "segmenting", icon = icon("columns")),
      menuItem("Biclustering", tabName = "biclustering", icon = icon("th")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    # extend js
    extendShinyjs(text = jsCode, functions = c("updateSeg", "prevSeg", "nextSeg", "showSeg")),
    tabItems(
      # First tab content
      tabItem(
        tabName = "import",
        fluidRow(
        
          box(
            width = 12,
            title = "Upload",
            
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
                
              )
              
            ),
            
            fluidRow(
              box(
                width = 12,
                fileInput('file1', 'Choose CSV File',
                          accept=c('text/csv',
                                   'text/comma-separated-values,text/plain',
                                   '.csv'))
              )
            )
          ),
          
          
          
          # allow x-flow for DT:dataTable
          shinyjs::inlineCSS(list(
            ".dataTables_wrapper" = "overflow-x: scroll"
          )),
          
          tabBox(
            width = 12,
           # tabPanel("Data Table", tableOutput("table"))
            tabPanel("Raw Data", DT::dataTableOutput('rawTable')),
            tabPanel("Summary", verbatimTextOutput("rawSummary"))
          )
        )
      ),
      # </import>
      
      tabItem(
        tabName = 'preprocessing',
        fluidRow(
          box(
            width = 3,
            title = "Normalization",
            checkboxInput('normalizing', 'Normalizing', FALSE),
            actionButton('goNormalizing', 'Go')
          ),
          
          box(
            width = 3,
            title = "Conditions",
            p('If'),
            selectInput('variableCon', '', choices = c('Please select a dataset'), multiple = F),
            selectInput('equalCon', '', choices = c('==','>=','<=','>','<'), multiple = F),
            p('Then'),
            selectInput('actionCon', '', choices = c('Remove line','Replace with'), multiple = F),
            actionButton('goConditions', 'Go')
          ),
          
          box(
            width = 3,
            title = "Outlier Removal",
            selectInput('outlier', 'variable', choices = c('Please select a dataset'), multiple = F),
            actionButton('goOutlierRemoval', 'Go')
            # ,
            # plotOutput('')
          ),
          
          box(
            width = 3,
            title = "Excludes",
            selectInput('excludings', 'Value Range', choices = c('Please select a dataset'), multiple = T)
            
          ),
          
          tabBox(
            width = 12,
            # tabPanel("Data Table", tableOutput("table"))
            tabPanel("After Pre-processing", DT::dataTableOutput('inputTable')),
            tabPanel("Summary", verbatimTextOutput("inputSummary"))
          ),
          
          downloadButton("processedDataset", label = "Download")
        )
      ),
      # </preprocessing>
      
      tabItem(
        tabName = 'plotting',
        fluidRow(
          box(
            width = 8,
            title = "Options",
            selectInput('plotY', 'Y Varaible(s)', choices = c('Please select a dataset'), multiple = T),
            selectInput('plotX', 'X Varaible', choices = c('Please select a dataset'), multiple = F)
           
            # 
            # tags$hr(),
            # 
            # 
            # actionButton("plotButton", "Plot"),
            # actionButton("mulplotButton", "Multi-plot"),
            # actionButton("corButton", "Correlation")
            # 
          ),
          
          tabBox(
            width = 12,
            tabPanel("Plot", dygraphOutput("plot"))
            ,tabPanel("Multi-plot", uiOutput("mulplot"))
            ,tabPanel("Correlation", uiOutput("corplot"))
            
          )
          
        )
        
      ),
      #</plotting>
      
      tabItem(
        tabName = 'segmenting',
        
        fluidRow(  
          uiOutput('segIndTabs'),
          
          box(
            width = 4,
            selectInput('segExcV', 'Excluding Variable(s)', choices = c('Please select a dataset'), multiple = T)
            ,icon("info-circle"),"Select variable(s) to be excluded from segmentation."
          ),
          box(
            width = 4,
            selectInput('segIndV', 'Individual Setting', choices = c('Please select a dataset'), multiple = T)
            ,icon("info-circle"),"Create an individual setting tab for each variable selected."
          ),
              
          
          box(
            width = 4,
            actionButton('segbutton', 'Start', icon = icon("arrow-circle-right"))
          ),
          
          # Plot
          box(
            width = 12,
            
            div(
              class="box-header",
              h3(
                class="box-title", 
                style="padding-right: 10px",
                'Plots'
              ),
              actionButton("segPrev", label = "", icon = icon("arrow-left")),
              actionButton("segNext", label = "", icon = icon("arrow-right"))
            ),
            
            # tabPanel('Now',
            div(
              id = "segHistory",
              width = 12,
              div(
                id="segLatest",
                plotOutput("segplot", width = "100%", height = "700px"),
                tableOutput("segpars")
              )
            )
          )
        )
      )
      #</segmenting>
      
    )
  )
)
