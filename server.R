
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
DEBUG_SEGPLOT_ON = T

# varaible names
AD_GENERAL = 'AD2016General'

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
    
    DT::datatable(d, options = list(pageLength = 20))
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
    # user choose to normailizing
    if(input$normalizing){
      # record the maximum for all varaibles to recover
      
      tempMax <- sapply(colnames(v$data),function(i){
        max(v$data[i])
      })
      tempMin <- sapply(colnames(v$data),function(i){
        min(v$data[i])
      })

      if(
        length(which(tempMax == 1)) == length(tempMax)    # all tempMax == 1 
        && length(which(tempMin == 0)) == length(tempMin) # all tempMin == 0 
      ){
        # it looks like that data has been normailized already
        # so keep previous max/min and skip normalizeTS (since it has no effect)
        v$data
      }
      else{
        v$dataMax <- tempMax
        v$dataMin <- tempMin
        v$data <- normalizeTS(v$data)
      }
      
    }
    # user choose to go to original data
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
  
  # remove excluding variables
  observeEvent(input$confirmExcludingVar, {
    toggleModal(session, "popExcludingVar")
    v$data <- v$data[setdiff(colnames(v$data),input$excludingVar)]
  })
  

  # execute condition
  observeEvent(input$goConditions, {
    
    # get index of rows according to the conditions
    aIndex <- do.call(input$equalCon, list(
        v$data[input$variableCon],
        input$numberCon
      )
    )

    # at least one element
    if(TRUE %in% aIndex){
      if(input$actionCon == "Remove line"){
        v$data <- v$data[!aIndex,] 
      }
      else if(input$actionCon == "Replace with"){
        v$data[aIndex, input$variableCon] <- input$replaceCon
      }
    }
    
    
  })
  
  output$uiExcludingVar <- renderUI({
    
    
    output$showExcludingVar <- renderText({
        paste("Going to remove: ", toString(input$excludingVar))
    })
    div(
      tags$h4(textOutput("showExcludingVar")) ,
      bsButton('confirmExcludingVar', 'Confirm Excluding', style = "danger")
    )
    
  })

  # the content inside the popup
  output$uiOutlierRemoval <- renderUI({
    # there will a plot showing which points to kill
    name <- input$outlierRemoval
    X <- v$data[name]

    avgX <- mean(X[,])
    diffX <- abs(X - avgX)
    aIndex <- which(diffX == max(diffX))
    
    plotName <- paste0('outlierRemoval', name)

    output[[plotName]] <- renderDygraph({
      graph <- dygraph(cbind(seq(from = 1, to = nrow(X)), X), main = name) %>%
        dyAxis("x", drawGrid = FALSE) %>%
        dyAxis("y", drawGrid = FALSE) %>%
        dyLimit(avgX, paste0("Average of ",name),  labelLoc = "right", color = "black", strokePattern = "dotted")
      
      for(i in aIndex){
        graph <- dyEvent(graph, i, labelLoc = "bottom", color = "#DF4A32",  strokePattern = "solid")
      }
      for(i in unique(X[aIndex,])){
        graph <- dyLimit(graph, i, paste0("Farthest from average: ",i),  labelLoc = "right", color = "red", strokePattern = "dotted")   
      }
      
      graph
      
    })
  
    
    result_div <- div()
     
    dop <- dygraphOutput(plotName, width = "100%", height = "300px")
    result_div <- tagAppendChild(result_div, dop)
    result_div <- tagAppendChild(result_div, 
      # div(
      # 
      div(
        style="padding: 20px 0 10px; text-align:center;",

        tags$h4(
          "Select", tags$strong(length(aIndex)),"row(s) with value of", 
          tags$strong( toString(unique(X[aIndex,])) )
        ),
        bsButton('confirmOutlierRemoval', 'Confirm Remove', style = "danger")
      )
    )

    v$todoOutlierIndex <- aIndex
    v$todoOutlierName <- name
     
    result_div

  })


  observeEvent(input$confirmOutlierRemoval, {
    v$data <- v$data[-v$todoOutlierIndex, ]
  })



  # data preview
  output$inputTable <- DT::renderDataTable({
    d <- v$data
    if (is.null(d))
      return()
    DT::datatable(d, options = list(pageLength = 20))
  })
 
  output$inputSummary <- renderPrint({
    d <- v$data
    if (is.null(d))
      return()
    summary(d)
  })

  output$processedDataset <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(v$data[-1], con)
    }
  )

  # output$downloadData <- downloadHandler(
#   filename = function() {
#     paste('data-', Sys.Date(), '.csv', sep='')
#   },
#   content = function(con) {
#     write.csv(data, con)
#   }
# )
  
  
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

    # multiple plots
  output$mulplot <- renderUI({
   
    d <- v$data
    if(is.null(input$plotY))
      return()
    # cat('hi')
    result_div <- div()
    lapply(input$plotY, function(i){
      
      if(input$plotX == 'DataIndex'){
        DataIndex <- seq(from = 1, to = nrow(d))
        target <- cbind(DataIndex, d[i])
      }
      else{
        target <- d[c(input$plotX,i)]
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
  })
  
  # corelation plots
  output$corplot <- renderUI({
    
    d <- v$data
    if(is.null(input$plotY))
      return()
    
    result_div <- div()
    
    lapply(input$plotY, function(i){
      
      if(input$plotX == 'DataIndex'){
        DataIndex <- seq(from = 1, to = nrow(d))
        target <- cbind(DataIndex, d[i])
      }
      else{
        target <- d[c(input$plotX,i)]
      }
      
      tempName <- paste0("corplot_ggplot_", i)
      dop <- plotOutput(tempName, width = "100%", height = "300px")
      result_div <- tagAppendChild(result_div, dop)
      
      output[[tempName]] <- renderPlot({
        ggplot(target, aes_string(x = input$plotX, y = i)) + 
          geom_point() + 
          geom_smooth(method = "lm", se = F) +
          ggtitle(i)
      })
      
    })
  })
  
  
  # multi plot
  # get_mulplot <- function(d, inputX, inputY) {
  #   if(is.null(inputY))
  #     return()
  #   # cat('hi')
  #   result_div <- div()
  #   lapply(inputY, function(i){
  #   
  #     
  #     if(input$plotX == 'DataIndex'){
  #       DataIndex <- seq(from = 1, to = nrow(d))
  #       target <- cbind(DataIndex, d[i])
  #     }
  #     else{
  #       target <- d[c(inputX,i)]
  #     }
  #     
  #     # 1. create a container
  #     #     <div result/>
  #     #         <div for dygraph1/>
  #     #         <div for dygraph2/>
  #     #
  #     # 2. define output$dygraph1, output$dygraph2
  #     tempName <- paste0("mulplot_dygraph_", i)
  #     
  #     dopOut <- dygraphOutput(tempName, width = "100%", height = "300px")
  #     # dopOut <- tagAppendChild(dopOut, dop)
  #     # dop <- dygraphOutput(dop, width = "200px", height = "200px")
  #     # dopOut <- tagAppendChild(dopOut, dop)
  #     result_div <- tagAppendChild(result_div, dopOut)
  #     
  #     output[[tempName]] <- renderDygraph({
  #       dygraph(target, main = i, group = 'mulplot') %>%
  #         dyOptions(colors = "#131688")
  #     })
  #   
  #   })
  # 
  # }
  
  # correlation plot
  # get_corplot <- function(d, inputX, inputY) {
  #   if(is.null(inputY))
  #     return()
  #   
  #   result_div <- div()
  #   
  #   lapply(inputY, function(i){
  #     
  #     if(inputX == 'DataIndex'){
  #       DataIndex <- seq(from = 1, to = nrow(d))
  #       target <- cbind(DataIndex, d[i])
  #     }
  #     else{
  #       target <- d[c(inputX,i)]
  #     }
  #     
  #     
  #     # dygraph(target, main = i, group = 'corplot') %>%
  #     #   dyOptions(strokeWidth = 0, drawPoints = T, pointSize = 5, fillAlpha = 0.3) %>%
  #     
  #     # plot(target)
  #     # abline(lm(target[, 2] ~ target[, 1]), col='red', lwd = 2)
  #     # ggplot(target)
  #     tempName <- paste0("corplot_ggplot_", i)
  #     dop <- plotOutput(tempName, width = "100%", height = "300px")
  #     result_div <- tagAppendChild(result_div, dop)
  # 
  #     output[[tempName]] <- renderPlot({
  #       ggplot(target, aes_string(x = inputX, y = i)) + 
  #         geom_point() + 
  #         geom_smooth(method = "lm", se = F) +
  #         ggtitle(i)
  #     })
  #     
  #   })
  # }

  
  
  ###################################################
  ################# Segment Tab #####################
  ###################################################
  
  output$segIndTabs <- renderUI({
    # get all individual variable names
    segIndVars <- input$segIndVars
    # if(!length(segIndVars)){
    # return()
    # }
    
    # use AD prefix for general tab
    # to void overplace data variables
    # 
    # create dynamical number of tabs
    tempTabs <- lapply(c(AD_GENERAL, segIndVars), function(i){
      if(i == AD_GENERAL){
        tempTitle = 'General'
        tempUni =  c("univariate" = 1, "multi-variate" = 0)
        
      }else{
        tempTitle = i
        tempUni = c("univariate" = 1)
        
      }
      
      # update: replace value with a record of last experiment
      
      tempWS<- v$segparsPrev["WindowSize",i]
      tempO <- v$segparsPrev["Overlap",i]
      tempT <- v$segparsPrev["Threshold",i]
      tempU <- v$segparsPrev["Univariate",i]
      
      # tempWS <- NULL
      # tempO <- NULL
      # tempT <- NULL
      # tempU <- NULL
      
      tabPanel(
        tempTitle,
        fluidRow(
          box(
            width = 12,
            
            sliderInput(paste0(i,"WindowSize"), "Window Size input:",
                        min = 1, max = input$segMaxWindowSize, value = ifelse(is.null(tempWS), 100, tempWS)),
            
            sliderInput(paste0(i, "Overlap"), "Overlap input:",
                        min = 0, max = 1, value = ifelse(is.null(tempO), 0.5, tempO)),
            
            sliderInput(paste0(i, "Threshold"), "Threshold input:",
                        min = 0, max = 1, value = ifelse(is.null(tempT), 0.9, tempT)),
            
            radioButtons(paste0(i, "Univariate"), "Variate type:",
                         tempUni, ifelse(is.null(tempU), 1, tempU), inline = T)
            
            
            # sliderInput(paste0(i,"WindowSize"), "Window Size input:",
            #             min = 1, max = 2, value = 1),
            
            # sliderInput(paste0(i, "Overlap"), "Overlap input:",
            #             min = 1, max = 2, value = 1),
            
            # sliderInput(paste0(i, "Threshold"), "Threshold input:",
            #             min = 1, max = 2, value = 1),
            
            # radioButtons(paste0(i, "Univariate"), "Variate type:",
            #              tempUni, 1, inline = T)
          )
        )
      )
    })
    args <- c(
      tempTabs,
      width = 8,
      selected = segIndVars[length(segIndVars)] # select last tab
    )
    do.call(tabBox, args)
  })
  
  # observeEvent(input$segIndConfirm, {
  #   v$segIndVars <- input$segIndVars
  # })

  # get parameters from the slider
  get_segparameters <- reactive({
    
    segIndVars <- input$segIndVars
    
    rowNames <- c(
      "WindowSize",
      "Overlap",
      "Threshold",
      "Univariate"
    )
    
    pars <- data.frame(
      row.names = rowNames
    )
    
    all <- c(AD_GENERAL, segIndVars)
    for(i in all){
      for(j in rowNames){
        if(!is.null(input[[paste0(i, j)]])){
          pars[j,i] <- as.numeric(input[[paste0(i, j)]])
        }
        else{
          pars[j,i] <- 0
        }
      }
    }
    
    pars
  })
  
  
  
  # parameter preview
  # updated when the sliders are being dragged
  output$segpars2 <- renderTable({
    pars <- get_segparameters()
    v$segparsLatest <- pars
    colnames(pars)[colnames(pars) == AD_GENERAL] <- "General"
    pars
  })

  ################################################
  ####### parameter table & segmentation plots
  ################################################
  # Table
  output$segpars <- renderTable({
    get_segtable()
  })
  
  get_segtable <- eventReactive(input$segbutton, {
    pars <- isolate(v$segparsLatest)
    colnames(pars)[colnames(pars) == AD_GENERAL] <- "General"
    pars
  })

  # Plot
  output$segplot <- renderUI({
    output$segplot0 <- renderPlot({
      get_segplot()
    })
    plotOutput("segplot0", width="100%", height="700px")
  })

  # after clicking the "start" button
  # get all
  get_segplot <- eventReactive(input$segbutton, {
  
    segLSDDPars <- data.frame(
      row.names = c('sigma','lambda')
    )
    segLSDDUnion <- c()
    # LSDDUnion <- data.frame(
    #   col.names = c('start', 'end')
    # )

    # cat('segplot starts')

    # return a LSDD plot
    pars <- isolate((v$segparsLatest))
    v$segparsPrev <- pars
    
    d <- isolate(v$data)

    # keep a record for initialization
    # v$segpars <- pars

    # create n+1 rows in the plots
    par(mfrow=c(ncol(d),1), mar=c(0,0,0,0))
    
    # in order to make debug easier
    # plot just data
    if(DEBUG_SEGPLOT_ON){
      for(col in colnames(d)){
        # if the variable is not individually defined
        if(!(col %in% colnames(pars))){
          par <- AD_GENERAL
        }else{
          par <- col
        }
        plot(x=1:nrow(d), y=d[,col], type="l",
             xaxt="n", yaxt="n",
             xlab="time series index", ylab=par, main="")
      }
      return()
    }
    
    # apply LSDD to each of variable one by one
    for(col in colnames(d)){
      # if the variable is not individually defined
      if(!(col %in% colnames(pars))){
        par <- AD_GENERAL
      }else{
        par <- col
      }

      LSDDResult<- LSDDsegment(d[col],
                  windowSize=pars["WindowSize", par],
                  overlap=pars["Overlap", par],
                  thres=pars["Threshold", par],
                  LSDDparameters=T,
                  univariate=pars["Univariate", par])

      # dygraph(data.frame(x=1:nrow(d), y=d[,col]), main = "") %>%
        # dyEvent("1950-6-30", "Korea", labelLoc = "bottom") %>%
        # dyEvent(LSDDResult$segStart, color = 'blue')

      plot(x=1:nrow(d), y=d[,col], type="l",
           xaxt="n", yaxt="n",
           xlab="time series index", ylab=par, main="")
      abline(v = LSDDResult$segStart, col="blue")
      abline(v = tail(LSDDResult$segEnd, 1), col="blue")
      # normally: thisEnd == nextStart - 1 , or == endOfAll
      # so no need to print all end event lines, since they are distracting
      # abline(v = LSDDResult$segEnd, col="red")

      segLSDDPars[col] <- as.numeric(c(LSDDResult$sigma, LSDDResult$lambda))
      segLSDDUnion <- c(segLSDDUnion, LSDDResult$segStart)

    }

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

    # merge segements that below minimum size
    finalSegs <- segSize(data = d,
                      segResults = segResults,
                      segLSDDPars = segLSDDPars,
                      # TODO: throttle should be customizable
                      throttle = 100
                    )

    # for biclustering usage
    v$finalSegs = finalSegs

    # plotting union segment results
    plot(x=1:nrow(d), y=rep(1, nrow(d)), type="n",
         xaxt="n", yaxt="n",
         xlab="time series index", ylab="", main="")
    # new segment event line
    abline(v = finalSegs$start, col="blue")
    abline(v = nrow(d), col="blue")
    # dotted merged event line
    abline(v = setdiff(segLSDDUnion, finalSegs$start), col="red", lty = 3)

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
      html("segbutton", 'Start')
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
  ############# observe column names
  #########################################################
  observe({
   
    dCol <- colnames(v$data)
    
    ############## pre-processing ########
    updateSelectInput(session, "excludingVar", choices = dCol)
    # this will refresh all components
    # let's keep outlierRemoval as previous
    # TODO: Click the button, put the name in storage
    updateSelectInput(session, "outlierRemoval", choices = dCol,
                      selected = v$todoOutlierName)
    
    updateSelectInput(session, "variableCon", choices = dCol)

    ############## plot tab ############## 
    updateSelectInput(session, "plotY", choices = dCol)
    updateSelectInput(session, "plotX", choices = c("DataIndex", dCol))
    
    ############## seg tab ############## 
    # updateSelectInput(session, "segExcV", choices = dCol)
    updateSelectInput(session, "segIndVars", choices = dCol)
    
    # TODO: limit the size of the window smaller than the size of all data
    
    
  })
  

  
  # observe({
  #   d <- v$data
  #   # create dynamic numders of plots
  #   
  # })
  
})
