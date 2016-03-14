
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
library(shinyBS)

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
      menuItem("Plotting", tabName = "plotting", icon = icon("bar-chart")),
      menuItem("Pre-processing", tabName = "preprocessing", icon = icon("cogs")),
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

      ######################################################
      ###################### import  ######################
      #####################################################
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
            ".dataTables_wrapper" = "overflow-x: scroll; overflow-y: hidden"
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


      ######################################################
      ###################### plotting ######################
      ######################################################
      tabItem(
        tabName = 'plotting',
        fluidRow(
          box(
            width = 8,
            title = "Options",
            selectInput('plotY', 'Y Varaible(s)', choices = c('Please select a dataset'), multiple = T),
            selectInput('plotX', 'X Varaible', choices = c('Please select a dataset'), multiple = F)
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
      

      ######################################################
      ###################### preprocessing ######################
      ######################################################
      tabItem(
        tabName = 'preprocessing',
        fluidRow(
          shinyjs::inlineCSS(list(
            "#shiny-tab-preprocessing .goButton" = "position: absolute; right:10px; bottom: 20px"
          )),
          
          box(
            width = 4,
            height = "200px",
            title = "Excludes",
            # selectInput('excludings', 'Value Range', choices = c('Please select a dataset'), multiple = T)
            selectInput('excludingVar', 'Exclude variable(s)', choices = c('Please select a dataset'), multiple = T)
            # , icon("info-circle"), "Select variable(s) to be excluded."
            ,actionButton('goExcludingVar', 'Go', class="goButton", icon = icon("arrow-circle-right"))
            ,bsModal("popExcludingVar", "Excludes", "goExcludingVar", size = "small", uiOutput("uiExcludingVar"))
            
          ),

          
          box(
            width = 4,
            height = "200px",
            title = "Outlier Removal",
            selectInput('outlierRemoval', 'Select a variable', choices = c('Please select a dataset'), multiple = F),
            actionButton('goOutlierRemoval', 'Go', class="goButton", icon = icon("arrow-circle-right")),
            bsModal("popOutlierRemoval", "Outlier Removal", "goOutlierRemoval", size = "large",uiOutput("uiOutlierRemoval"))
          ),
          
          
          
          box(
            width = 4,
            height = "200px",
            title = "Normalization",
            # checkboxInput('normalizing', 'Normalizing', FALSE),
            radioButtons('normalizing', 'Normalization',
                         c('ON'=T,
                           'OFF'=F),
                         F),
            actionButton('goNormalizing', 'Go', class="goButton", icon = icon("arrow-circle-right"))
          ),
          
          box(
            width = 8,
            height = "210px",
            title = "Conditions",
            fluidRow(
              column(4,selectInput('variableCon', 'If', choices = c('Please select a dataset'), multiple = F)),
              column(4,selectInput('equalCon', '.', choices = c('==','>=','<=','>','<'), multiple = F)),
              column(4,numericInput('numberCon', label='.', value=0))
            ),
            fluidRow(
              column(5,selectInput('actionCon', 'Then', choices = c('Remove line','Replace with'), multiple = F)),
              
              conditionalPanel("input.actionCon == 'Replace with'",
                               column(4,numericInput('replaceCon', label='.', value=0))
              )
            ),
            actionButton('goConditions', 'Go', class="goButton", icon = icon("arrow-circle-right"))
          ),
          
          tabBox(
            width = 12,
            # tabPanel("Data Table", tableOutput("table"))
            tabPanel("Pre-processed Data", DT::dataTableOutput('inputTable')),
            tabPanel("Summary", verbatimTextOutput("inputSummary")),
            tabPanel(tagList(shiny::icon("download"), "Download"), 
                      div(
                        style="text-align:center; padding:30px",
                        downloadButton("processedDataset", label = "Download pre-processed data table as CSV")
                      )
                    )
          )
          
          
        )
      ),
      # </preprocessing>
      

      ######################################################
      ###################### segmenting ###################
      #####################################################
      tabItem(
        tabName = 'segmenting',
        
        fluidRow(  
          uiOutput('segIndTabs'),
          
          box(
            width = 4,
            numericInput('segMaxWindowSize', label="Maximum Window Size", value=500)
            ,
            selectInput('segIndVars', 'Individual Setting', choices = c('Please select a dataset'), multiple = T)
            ,
            icon("info-circle"),"Create an individual setting tab for each variable selected."
          ),
              
          
          box(
            width = 4,
            # tableOutput("segpars2")
            # ,
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
                tableOutput("segpars"),
                uiOutput("segplot")
                
              )
            )
          )
        )
      ),
      #</segmenting>
      
      
      ######################################################
      ###################### biclustering ##################
      ######################################################
      tabItem(
        tabName = 'biclustering',
        
        fluidRow(  
          tabBox(
            id = "biFunctions",
            width = 8,
            tabPanel(
              "Baseline",
              fluidRow(
                box(
                  width = 12,
                  checkboxInput('baselineBiSeg', 'Segmentation', TRUE)
                  ,
                  numericInput('baselineBiDelta', label="Delta", value=0.01)
                  ,
                  numericInput('baselineBiAlpha', label="Alpha", value=1)
                  ,
                  numericInput('baselineBiK', label="K", value=100)
                  
                )
              )
            )
            ,
            tabPanel(
              "PDF",
              fluidRow(
                box(
                  width = 12,
                  checkboxInput('PDFBiSeg', 'Segmentation', TRUE)
                  ,
                  numericInput('PDFBiDelta', label="Delta", value=0.01)
                  ,
                  numericInput('PDFBiK', label="K", value=100)
                  
                )
              )
            )
            ,
            tabPanel(
              "LSDD",
              fluidRow(
                box(
                  width = 12,
                  checkboxInput('LSDDBiSeg', 'Segmentation', TRUE)
                  ,
                  numericInput('LSDDBiDelta', label="Delta", value=0.2)
                  ,
                  numericInput('LSDDBiK', label="K", value=5)
                  
                )
              )
            )
          ),
          
          box(
            width = 4,
            # textOutput("biTab"),
            # checkboxInput('biDygraph', 'Use dygraph', FALSE),
            
            actionButton('biButton', 'Start', icon = icon("arrow-circle-right"))
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
              actionButton("biPlotPrev", label = "", icon = icon("arrow-left")),
              actionButton("biPlotNext", label = "", icon = icon("arrow-right"))
            ),
            
            # tabPanel('Now',
            div(
              
              shinyjs::inlineCSS(list(
                "#biDensity" = "float: right"
              )),
              
              id = "biPlotHistory",
              width = 12,
              div(
                id="biPlotLatest",
                # tableOutput("bipars"),
                # uiOutput("biplot"),
                uiOutput("biDygraph")
              )
            )
          )
        )
      ),
      #</biclustering>
      
      tabItem(
        tabName = 'about',
        
        fluidRow(  
          box(
            width = 12,
            'Hello World'
          )
        )
      )
    )
  )
)
