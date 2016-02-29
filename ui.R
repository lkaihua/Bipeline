
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

jsCode <- "
shinyjs.statSeg = {
  now: 0,
  all: 0
}

shinyjs.updateSeg = function() {
  
  var info = $('#segplot>img').length ? $('#segplot>img') : $('<div/>')
                .append($('#segplot').html())
                .attr('class', 'shiny-plot-output shiny-bound-output shiny-output-error');

  $('#segHistory').append(
    $('<div/>')
      .attr('class', 'segPiece segPiece' + shinyjs.statSeg.all)
      //.append('<p>all:' + shinyjs.statSeg.all)
      //.append('<p>now:' + shinyjs.statSeg.now)
      .append(info)
      .append($('#segpars>table'))
  )
  
  // when click go, back to now
  $('.segPiece').hide()
  $('#segLatest').show()

  ++shinyjs.statSeg.all;
  
  $('#segLatest')
    .attr('class', 'segPiece segPiece' + shinyjs.statSeg.all)

  shinyjs.statSeg.now = shinyjs.statSeg.all;
}

shinyjs.prevSeg = function() {
  // panel 0 contains nothing
  if(shinyjs.statSeg.now > 1){
    shinyjs.statSeg.now -= 1 
  }
  shinyjs.showSeg(shinyjs.statSeg.now)
}
shinyjs.nextSeg = function() {
  if(shinyjs.statSeg.now < shinyjs.statSeg.all) {
    shinyjs.statSeg.now += 1 
  }
  shinyjs.showSeg(shinyjs.statSeg.now)
}
shinyjs.showSeg = function(i) {
  $('.segPiece').hide()
  $('.segPiece' + i).show()
}
"

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
    useShinyjs(),
    # extend js
    extendShinyjs(text = jsCode, functions = c("updateSeg", "prevSeg", "nextSeg", "showSeg")),
    tabItems(
      # First tab content
      tabItem(tabName = "general",
        fluidRow(
        
          box(
            width = 12,
            title = "Dataset option",
            
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
            width = 8,
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
      
      tabItem(tabName = 'segmenting',
        
        # fluidRow(
        #   box(
        #     width = 12,
        #     title = 'General settings',
        #     
        #     fluidRow(
        #       box(
        #         width = 6,
        #         sliderInput("windowSize", "Window Size input:",
        #                     min = 1, max = 1000, value = 100),
        #         
        #         sliderInput("overlap", "Overlap input:",
        #                     min = 0, max = 1, value = 0.5),
        #         
        #         sliderInput("threshold", "Threshold input:",
        #                     min = 0, max = 1, value = 0.9),
        #         
        #         radioButtons("univariate", "Variate type:",
        #                      c("univariate" = 1,
        #                        "multi-variate" = 0))
        #       ),
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
            actionButton('segbutton', 'Start')
          ),
          
          # Plot
          box(
            title = 'Plots',
            width = '12',
            
            box(
              width = 12,
              actionButton("segPrev", label = "", icon = icon("arrow-left")),
              actionButton("segNext", label = "", icon = icon("arrow-right"))
            ),
            
            # tabPanel('Now',
            box(
              id = "segHistory",
              width = 12,
              div(
                id="segLatest",
                plotOutput("segplot", width = "100%", height = "800px"),
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
