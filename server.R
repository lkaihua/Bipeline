
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#



library(shiny)
library(dygraphs)
library(DT)
source('normalize.R')
source('LSDD.R')
source('LSDDsegmentation.R')

# change maximum file size from 5MB to 50MB
options(shiny.maxRequestSize = 50*1024^2)

DEBUG_ON = T


shinyServer(function(input, output, session) {
  
  ###################################################
  ################# General Tab  ####################
  ###################################################
  
  
  # using reactive to dynamically import dataset
  dataInput <- reactive({
    
    # get file from uploading
    if(!DEBUG_ON){
      inFile <- input$file1
  
      if (is.null(inFile))
        return(NULL)
  
      d <- data.frame( read.csv(inFile$datapath, header=input$header, sep=input$sep,
               quote=input$quote) )
    }else{
    # or debug deafult data
      d <- data.frame( read.csv('./test100.csv'))  
    }
    
    if (input$normalizing){
      d <- normalizeTS(d)
    }
    
    return(d)
  })
  

  # data table with navigation tab
  # renderTable will kill the browser when the data is large
  output$table <- DT::renderDataTable({
    d <- dataInput()
    
    if (is.null(d))
      return()
    
    DT::datatable(d, options = list(pageLength = 10))
  })
    
   
  # data summary
  output$summary <- renderPrint({
    d <- dataInput()
    
    if (is.null(d))
      return()
    
    summary(d)
  })
  
  ###################################################
  ################# Plot Tab ########################
  ###################################################
  output$plot <- renderDygraph({
    
    # dependency
    # input$plot
  
    d <- dataInput()
    
    if (is.null(d)
        ||
        is.null(input$plotY)
        )
      return(NULL)
    
  
    
    # dygraph:
    # where the first element/column provides x-axis values and 
    # all subsequent elements/columns provide one or more series of y-values.
    
    # get selected X and Y
    if(input$plotX == 'DataIndex'){
      DataIndex <- seq(from = 1, to = nrow(d))
      target <- cbind(DataIndex, d[input$plotY])
    }
    else{
      target <- d[c(input$plotX,input$plotY)]
    }
    
    dygraph(target, main = 'Selected variables')
      
    

  })
  
  
  # multi plot
  get_mplot <- function(d, inputX, inputY) {
    if(is.null(inputY))
      return()
    # cat('hi')
    result_div <- div()
    lapply(inputY, function(i){
    
      
      if(input$plotX == 'DataIndex'){
        DataIndex <- seq(from = 1, to = nrow(d))
        target <- cbind(DataIndex, d[i])
      }
      else{
        target <- d[c(inputX,i)]
      }
      
      
      dop <- dygraphOutput(paste0("mplot_dygraph_", i))
      dop <- renderDygraph({
        dygraph(target, main = i, group = 'mplot', width='1200')
      })
      result_div <- tagAppendChild(result_div, dop)
      
    
    })
    
  }
  
  # correlation plot
  get_corplot <- function(d, inputX, inputY) {
    if(is.null(inputY))
      return()
    
    lapply(inputY, function(i){
      
      if(input$plotX == 'DataIndex'){
        DataIndex <- seq(from = 1, to = nrow(d))
        target <- cbind(DataIndex, d[i])
      }
      else{
        target <- d[c(inputX,i)]
      }
      
      
      # dygraph(target, main = i, group = 'corplot') %>%
      #   dyOptions(strokeWidth = 0, drawPoints = T, pointSize = 5, fillAlpha = 0.3) %>%
      
      plot(target)
      abline(lm(target[, 2] ~ target[, 1]))
      
    })
  }
  
  
  #########################################################
  ############## update when the input changes  ###########
  #########################################################
  observe({
    
    # when data is imported
    d <- dataInput()
    dCol <- colnames(d)
    
    ############## plot tab ############## 
    updateSelectInput(session, "plotY",
                      # label = paste("Select label", dCol),
                      choices = dCol
                      # selected = sprintf("option-%d-2", x)
    )
    
    updateSelectInput(session, "plotX",
                      # label = paste("Select label", dCol),
                      choices = c("DataIndex", dCol)
                      # selected = sprintf("option-%d-2", x)
    )
    
   
    # create dynamic numders of plots
    output$mplot <- renderUI({
      get_mplot(d, input$plotX, input$plotY)
    })
    
    # create dynamic numders of plots
    output$corplot <- renderPlot({
      get_corplot(d, input$plotX, input$plotY)
    })
    
    ############## seg tab ############## 
    updateSelectInput(session, "segExcV", choices = dCol)
    updateSelectInput(session, "segIndV", choices = dCol)
    
    
    # limit the size of the window smaller than the size of all data
    # update: put a warning will be better in trycatch block
    # updateSelectInput(session, "segExcludingV", choices = dCol)
    
    
  })
  
  # reactive({
  #   if(input$segplotButton){
  #     cat('hi')
  #   }
  # })
  # 
  # segPlotting <- eventReactive(input$segbutton, {
  #   # input$windowSize
  #   plot(dataInput())
  # })
  
  output$segplot <- renderPlot({
    # segPlotting()
    get_segplot()
  })
  
  
  output$segIndTabs <- renderUI({
    segIndV <- input$segIndV
    # if(!length(segIndV)){
      # return()
    segIndV <- c('ADGeneral',segIndV)
    # }
    tempTabs <- lapply(paste(segIndV, 'Setting'), function(i){
      tabPanel(
        i,
        sliderInput(paste0(i,"WindowSize"), "Window Size input:",
                    min = 1, max = 1000, value = 100),
        
        sliderInput(paste0(i, "Overlap"), "Overlap input:",
                    min = 0, max = 1, value = 0.5),
        
        sliderInput(paste0(i, "Threshold"), "Threshold input:",
                    min = 0, max = 1, value = 0.9),
        
        radioButtons(paste0(i, "Univariate"), "Variate type:",
                     c("univariate" = 1,
                       "multi-variate" = 0))
        
      )
    })

    do.call(tabBox, tempTabs)
    # tempTabs()
  })
  
  
  ###################################################
  ################# Segment Tab #####################
  ###################################################
  
  get_segparameters <- reactive({
    # input$univariate <- ifelse(input$univariate == 1, TRUE, FALSE)
    data.frame(
      # TODO: individual parameters
      general = c(
        as.numeric(
          c(input$windowSize,
            input$overlap,
            input$threshold,
            input$univariate
          )
        )
      )
    )
  })
  
  get_segplot <- eventReactive(input$segbutton, {
    cat('segplot starts')
    
    # input$segplotButton
    # return a LSDD plot
    temp <- isolate(get_segparameters())
    d <- isolate(dataInput())
    
    # remove all non-numeric columns by default
    # TODO: show this message in the app
    # input$segExcV <- colnames(d[sapply(d, !is.numeric)])
    d <- d[sapply(d, is.numeric)]
    
    # also remove segExcV colums
    d <- d[setdiff(colnames(d),input$segExcV)]
    
    # temp
    # return('hi')
    # return(input$windowSize)
    # d
    LSDDsegment(d,
                windowSize=temp$general[1],
                overlap=temp$general[2],
                thres=temp$general[3],
                LSDDparameters=FALSE,
                univariate=temp$general[4])
    
    # plot(d)
  })
  
  
})
