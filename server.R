
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dygraphs)
library(DT)
library(ggplot2)

source('normalize.R')
source('LSDD.R')
source('LSDDsegmentation.R')
source('segSize.R')

# change maximum file size from 5MB to 50MB
options(shiny.maxRequestSize = 50*1024^2)

# global settings
DEBUG_ON = T
AD_GENERAL = 'AD2016_General'

shinyServer(function(input, output, session) {
  
  # session data storage
  v <- reactiveValues()

  ###################################################
  ################# Import Tab  ####################
  ###################################################
  
  
  # using reactive to dynamically import dataset
  dataInputRaw <- reactive({
    
    # get file from uploading
    if(!DEBUG_ON){
      inFile <- input$file1
  
      if (is.null(inFile))
        return(NULL)
  
      d <- data.frame( 
            read.csv(
              inFile$datapath, 
              header=input$header, 
              sep=input$sep,
              quote=input$quote))
    }else{
    # or debug deafult data
      d <- data.frame( read.csv('./test5000.csv'))  
    }
    
    # dataFinal <- d
    v$raw <- d
    
    # remove all non-numeric columns
    d <- d[sapply(d, is.numeric)]
    
    if (
      is.null(d)
      || ncol(d) == 0
    ){
      v$data <- NULL
    }
    else{
      v$data <- d  
    }
  
    # return rawData for display
    return(v$raw)
  })


  # data table with navigation tab
  # renderTable will kill the browser when the data is large
  output$rawTable <- DT::renderDataTable({
    d <- dataInputRaw()
    
    if (is.null(d))
      return()
    
    DT::datatable(d, options = list(pageLength = 10))
  })
    
   
  # data summary
  output$rawSummary <- renderPrint({
    d <- dataInputRaw()
    
    if (is.null(d))
      return()
    
    summary(d)
  })
  
  ###################################################
  ################# Import Tab  ####################
  ###################################################

  # dataInput <- reactive({
  # 
  #   d <- dataInputRaw()
  # 
  #   # remove all non-numeric columns
  #   d <- d[sapply(d, is.numeric)]
  # 
  #   if (
  #     is.null(d)
  #     || ncol(d) == 0
  #   ){
  #     return()
  #   }
  # 
  #   # if (input$rangeButton) {
  #   #   # see what's the limitage
  #   #   tempVars <- input$variableRange
  #   #   for(tempVar in tempVars){
  #   #     tempMin <- input[[paste0("rangeMin", tempVar)]]
  #   #     tempMax <- input[[paste0("rangeMax", tempVar)]]
  #   #
  #   #     # keep all value within range
  #   #     # TODO: there are a few more options
  #   #     d[d[tempVar] < tempMin, tempVar] <- tempMin
  #   #     d[d[tempVar] > tempMax, tempVar] <- tempMax
  #   #   }
  #   # }
  # 
  # 
  #   return(d)
  # 
  # })
  

  
  ###################################################
  ################# Pre-processing Tab ##############
  ###################################################
  
  # click go button for normalizing
  observeEvent(input$goNormalizing, {
    if(input$normalizing){
      # record the maximum for all varaibles to recover
      
      v$dataMax <- sapply(colnames(v$data),function(i){
        max(v$data[i])
      })
      v$dataMin <- sapply(colnames(v$data),function(i){
        min(v$data[i])
      })
      # print(dataMax)
      v$data <- normalizeTS(v$data)
    }
    else{
      # recover by multipling the max values and min
      if(!is.null(v$dataMax)){
        for(i in colnames(v$data)){
          v$data[i] <- v$data[i] * (v$dataMax[i] - v$dataMin[i]) + v$dataMin[i]
        }
      }
      v$data
      
    }
  })

  # data preview
  output$inputTable <- DT::renderDataTable({
    # d <- dataInput()
    d <- v$data
    if (is.null(d))
      return()
    DT::datatable(d, options = list(pageLength = 10))
  })
 
  output$inputSummary <- renderPrint({
    d <- v$data
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
  
    d <- v$data
    
    if (is.null(d)
        ||
        is.null(input$plotY)
        )
      return()
    
  
    
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
    
    dygraph(target)
      
    

  })
  
  
  # multi plot
  get_mulplot <- function(d, inputX, inputY) {
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
      
      # 1. create a container
      #     <div result/>
      #         <div for dygraph1/>
      #         <div for dygraph2/>
      #
      # 2. define output$dygraph1, output$dygraph2
      tempName <- paste0("mulplot_dygraph_", i)
      
      dopOut <- dygraphOutput(tempName, width = "100%", height = "300px")
      # dopOut <- tagAppendChild(dopOut, dop)
      # dop <- dygraphOutput(dop, width = "200px", height = "200px")
      # dopOut <- tagAppendChild(dopOut, dop)
      result_div <- tagAppendChild(result_div, dopOut)
      
      output[[tempName]] <- renderDygraph({
        dygraph(target, main = i, group = 'mulplot') %>%
          dyOptions(colors = "#131688")
      })
    
    })

  }
  
  # correlation plot
  get_corplot <- function(d, inputX, inputY) {
    if(is.null(inputY))
      return()
    
    result_div <- div()
    
    lapply(inputY, function(i){
      
      if(inputX == 'DataIndex'){
        DataIndex <- seq(from = 1, to = nrow(d))
        target <- cbind(DataIndex, d[i])
      }
      else{
        target <- d[c(inputX,i)]
      }
      
      
      # dygraph(target, main = i, group = 'corplot') %>%
      #   dyOptions(strokeWidth = 0, drawPoints = T, pointSize = 5, fillAlpha = 0.3) %>%
      
      # plot(target)
      # abline(lm(target[, 2] ~ target[, 1]), col='red', lwd = 2)
      # ggplot(target)
      tempName <- paste0("corplot_ggplot_", i)
      dop <- plotOutput(tempName, width = "100%", height = "300px")
      result_div <- tagAppendChild(result_div, dop)

      output[[tempName]] <- renderPlot({
        ggplot(target, aes_string(x = inputX, y = i)) + 
          geom_point() + 
          geom_smooth(method = "lm", se = F) +
          ggtitle(i)
      })
      
    })
  }

  
  
  ###################################################
  ################# Segment Tab #####################
  ###################################################
  
  output$segIndTabs <- renderUI({
    segIndV <- input$segIndV
    # if(!length(segIndV)){
      # return()
    # }
    
    # use AD prefix for general tab
    # to void overplace data variables
    # 
    # create dynamical number of tabs
    tempTabs <- lapply(c(AD_GENERAL, segIndV), function(i){
      if(i == AD_GENERAL){
        tempTitle = 'General'
        tempUni =  c("univariate" = 1, "multi-variate" = 0)

      }else{
        tempTitle = i
        tempUni = c("univariate" = 1)
        
      }
      
      tabPanel(
        tempTitle,
        fluidRow(
          box(
            width = 12,
          
            sliderInput(paste0(i,"WindowSize"), "Window Size input:",
                        min = 1, max = 1000, value = 100),
            
            sliderInput(paste0(i, "Overlap"), "Overlap input:",
                        min = 0, max = 1, value = 0.5),
            
            sliderInput(paste0(i, "Threshold"), "Threshold input:",
                        min = 0, max = 1, value = 0.9),
            
            radioButtons(paste0(i, "Univariate"), "Variate type:",
                         tempUni, inline = T)
          )
        )
      )
    })
    args <- c(
      tempTabs,
      width = 8
    )
    do.call("tabBox", args)
    
  })

  # get parameters from the slider
  get_segparameters <- reactive({
    
    segIndV <- input$segIndV
    
    rowNames <- c(
      "WindowSize",
      "Overlap",
      "Threshold",
      "Univariate"
    )
    
    pars <- data.frame(
      row.names = rowNames
    )
    
    all <- c(AD_GENERAL, segIndV)
    for(i in all){
      for(j in rowNames){
        pars[j,i] <- as.numeric(input[[paste0(i, j)]])
      }
    }
    
    pars
  })
  
  output$segpars <- renderTable({
    get_segtable()
  })
  
  get_segtable <- eventReactive(input$segbutton, {
    pars <- isolate(get_segparameters())
    colnames(pars)[colnames(pars) == AD_GENERAL] <- "General"
    # TODO remove excluding parameters from the table
    pars <- pars[!colnames(pars) %in% input$segExcV]
    pars
  })
  
  output$segplot <- renderPlot({
    get_segplot()
  })
  
  get_segplot <- eventReactive(input$segbutton, {
    
    segLSDDPars <- data.frame(
      row.names = c('sigma','lambda')
    )
    segLSDDUnion <- c()
    # LSDDUnion <- data.frame(
    #   col.names = c('start', 'end')
    # )
    
    # cat('segplot starts')
    
    # input$segplotButton
    # return a LSDD plot
    pars <- isolate(get_segparameters())
    d <- isolate(v$data)
    
    # also remove segExcV colums
    d <- d[setdiff(colnames(d),input$segExcV)]
    
    par(mfrow=c(ncol(d) + 1,1), mar=c(0,0,0,0))
    # apply LSDD to each of variable one by one
    for(v in colnames(d)){
      # if the variable is not individually defined
      if(!(v %in% colnames(pars))){
        par <- AD_GENERAL
      }else{
        par <- v
      }
      
      LSDDResult<- LSDDsegment(d[v],
                  windowSize=pars["WindowSize", par],
                  overlap=pars["Overlap", par],
                  thres=pars["Threshold", par],
                  LSDDparameters=T,
                  univariate=pars["Univariate", par])
      
      # dygraph(data.frame(x=1:nrow(d), y=d[,v]), main = "") 
      # %>%
        # dyEvent("1950-6-30", "Korea", labelLoc = "bottom") %>%
        # dyEvent(LSDDResult$segStart, color = 'blue')
      plot(x=1:nrow(d), y=d[,v], type="l", 
           xaxt="n", yaxt="n",
           xlab="time series index", ylab=par, main="")
      abline(v = LSDDResult$segStart, col="blue")
      abline(v = tail(LSDDResult$segEnd, 1), col="blue")
      
      # normally: thisEnd == nextStart - 1 , or == endOfAll
      # so no need to print end event line since they are distracting
      # abline(v = LSDDResult$segEnd, col="red")
      
      segLSDDPars[v] <- as.numeric(c(LSDDResult$sigma, LSDDResult$lambda))
      segLSDDUnion <- c(segLSDDUnion, LSDDResult$segStart)

    }
    
    # and apply general setting for the rest variables
    # dGeneral <- setdiff(colnames(d), colnames(pars))
    # if(!length(dGeneral)){
    #   dGeneral <- d[dGeneral]
    #   
    #   LSDDsegment(dGeneral,
    #               windowSize=pars["WindowSize", AD_GENERAL],
    #               overlap=pars["Overlap", AD_GENERAL],
    #               thres=pars["Threshold", AD_GENERAL],
    #               LSDDparameters=FALSE,
    #               univariate=pars["Univariate", AD_GENERAL])
    # }
    
    segLSDDUnion <- sort(unique(segLSDDUnion))
    # put the end
    # cat(segLSDDUnion)
    
    # after we have all seg line
    # check MinSegSize
    # if less than the throttle, merge it
    segResults <- data.frame(
      start = segLSDDUnion,
      end = tail(c(segLSDDUnion - 1, nrow(d)), -1)
      # col.names = c("start", "end")
    )
    

    newSeg <- segSize(data = d, 
                          segResults = segResults, 
                          segLSDDPars = segLSDDPars, 
                          throttle = 100
                        )
    
    plot(x=1:nrow(d), y=rep(1, nrow(d)), type="n", 
         xaxt="n", yaxt="n",
         xlab="time series index", ylab=par, main="")
    # new segment event line
    abline(v = newSeg$start, col="blue")
    abline(v = nrow(d), col="blue")
    # dotted merged event line
    abline(v = setdiff(segLSDDUnion, newSeg$start), col="red", lty = 3)
    
  })
  
  #########################################################
  ############ observe button
  # disable and re-enable the button
  observeEvent(input$segbutton, {
    
    # run external js
    # to copy previous plot and info to the history
    js$updateSeg()
    
    # disable button for 2s to prevent multiple time application
    html("segbutton", "Running...")
    disable("segbutton")
    
    # it looks like this 2000ms will make sure 
    # the button is unclickable until the server is not busy
    delay(2000, {
      html("segbutton", "Start")
      enable("segbutton")
    })
  })
  
  observeEvent(input$segPrev, {
    js$prevSeg()
  })
  
  observeEvent(input$segNext, {
    js$nextSeg()
  })
  
  
  #########################################################
  ############# observe
  observe({
    
    # when data is imported
    d <- v$data
    
    dCol <- colnames(d)
    
    ############## pre-processing ########
    # updateSelectInput(session, "variableRange", choices = dCol)
    
    ############## plot tab ############## 
    updateSelectInput(session, "plotY", choices = dCol)
    updateSelectInput(session, "plotX", choices = c("DataIndex", dCol))
    
    # create dynamic numders of plots
    output$mulplot <- renderUI({
      get_mulplot(d, input$plotX, input$plotY)
    })
    
    # create dynamic numders of plots
    output$corplot <- renderUI({
      get_corplot(d, input$plotX, input$plotY)
    })
    
    ############## seg tab ############## 
    updateSelectInput(session, "segExcV", choices = dCol)
    updateSelectInput(session, "segIndV", choices = dCol)
    
    
    # TODO: limit the size of the window smaller than the size of all data
    
    
  })
  
})
