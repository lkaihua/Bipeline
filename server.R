
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dygraphs)
library(DT)
library(ggplot2)
library(scales)

source('normalize.R')
source('LSDD.R')
source('LSDDsegmentation.R')
source('segMerge.R')

# source('baselineAggregateBiclustering.R')
source('baselineBiclustering.R')
source('PDFbiclustering.R')
source('LSDDbiclustering.R')


# change maximum file size from 5MB to 100MB
options(shiny.maxRequestSize = 100*1024^2)

# global settings
DEBUG_UPLOAD_ON = F
DEBUG_SEGPLOT_ON = F


# varaible names
AD_GENERAL = 'AD2016General'

shinyServer(function(input, output, session) {
  
  # session data storage
  v <- reactiveValues()

  ###################################################
  ################# Import Tab  ####################
  ###################################################
  
  v$fileImport <- 1
  
  updateFileInput <- function (name = NULL){
    output$fileImport <- renderUI({
      
      index <- isolate(v$fileImport) # re-render
      result <- div()
      
      result <- tagAppendChild(
        result,
        fileInput(paste0('file', index), 'Choose CSV File',accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))
      )

      if(!is.null(name)){
        result <- tagAppendChild(
          result, 
          div(
            class="progress progress-striped",
            
            div(
              class="progress-bar",
              style="width: 100%",
              name, 
              " upload complete"
            )
          )
        )
      }

      result
      
    })
  }
  
  updateFileInput()
  
  # using reactive to dynamically import dataset
  dataInputRaw <- reactive({
    
    # upload debug
    if(DEBUG_UPLOAD_ON){
      d <- data.frame( read.csv('./data/test5000.csv'))  
      d <- d[sapply(d, is.numeric)]
      v$data <- d
      return(v$data)
    } 
    
    inFile <- input[[paste0('file', v$fileImport)]]
      
    # TICKY PART:
    # 1. If initialized, `inFile` and `v$data` are both `NULL`
    # 2. After each uploading, new `fileInput` is applied and
    #    we want to keep previous updated data.
    #    It also prevent recursive creation of new `fileInput`s.
    
    if (is.null(inFile)){
      # return(NULL)
      return(v$raw)
    }
      
    d <- data.frame( 
          read.csv(
            inFile$datapath, 
            header=input$header, 
            sep=input$sep,
            quote=input$quote
          )
        )

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
    
    if (!is.null(v$data)){
      v$fileImport <- v$fileImport + 1
      updateFileInput(name = inFile$name)
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
    
    tempText <- input$excludingVar
    output$showExcludingVar <- renderText({
      if(is.null(tempText)){
        "No column selected"
      }
      else{
        paste("Going to remove:", toString(tempText))
      }
    })
    div(
      style = "text-align:center",
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
          tags$strong(length(aIndex))," row(s) selected, with value of ", 
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
      paste('data-', Sys.time(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(v$data, file, row.names=FALSE)
    }
  )
  
  
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
    {
      return()
    }
    
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
    if(is.null(input$plotY)){
      return()
    }
    
    result_div <- div()

    out <- lapply(input$plotY, function(i){
      
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
      
      # output$xxx mush be defined before uiOutput("xxx") to make it work
      output[[tempName]] <- renderDygraph({
        dygraph(target, main = i, group = 'mulplot') %>%
          dyOptions(colors = "black")
      })
      dygraphOutput(tempName, width = "100%", height = "300px")
      
    })

    result_div <- tagAppendChild(result_div, out)
      
  })
  
  # corelation plots
  output$corplot <- renderUI({
    
    d <- v$data
    if(is.null(input$plotY))
      return()
    
    result_div <- div()
    
    dop <- lapply(input$plotY, function(i){
      
      if(input$plotX == 'DataIndex'){
        DataIndex <- seq(from = 1, to = nrow(d))
        target <- cbind(DataIndex, d[i])
      }
      else{
        target <- d[c(input$plotX,i)]
      }
      
      tempName <- paste0("corplot_ggplot_", i)
      output[[tempName]] <- renderPlot({
        ggplot(target, aes_string(x = input$plotX, y = i)) + 
          geom_point() + 
          geom_smooth(method = "lm", se = F) +
          ggtitle(i)
      })
      plotOutput(tempName, width = "100%", height = "300px")
      
    })

    result_div <- tagAppendChild(result_div, dop)
      
  })
  
  
  ###################################################
  ################# Segment Tab #####################
  ###################################################
  
  output$segIndTabs <- renderUI({
    # get all individual variable names
    segIndVars <- input$segIndVars
    
    # if data is null, show general
    # if selected variables less than all, show general
    if(
      is.null(v$data)
      ||
      length(segIndVars) < ncol(v$data)
    ){
      allTabs <- c(AD_GENERAL, segIndVars)
    }else{
      allTabs <- segIndVars
    }
    # use AD prefix for general tab
    # to void overplace data variables
    # 
    # create dynamical number of tabs
    tempTabs <- lapply(allTabs, function(i){

      # TODO: hide general when all variables are selected
      if(i == AD_GENERAL){
        tempTitle = 'General'
        tempIcon = icon("navicon")
        tempUni =  c("univariate" = 1, "multi-variate" = 0)
        
      }else{
        tempTitle = i
        tempIcon = NULL
        tempUni = c("univariate" = 1)
      }
      
      # update: replace value with a record of last experiment
      temp  <- isolate(v$segparsPrev)
      tempWS<- temp["WindowSize",i]
      tempO <- temp["Overlap",i]
      tempT <- temp["Threshold",i]
      tempU <- temp["Univariate",i]
      
      # tempWS <- NULL
      # tempO <- NULL
      # tempT <- NULL
      # tempU <- NULL
      
      tabPanel(
        tempTitle,
        icon = tempIcon,
        fluidRow(
          
          box(
            width = 12,
            
            sliderInput(paste0(i,"WindowSize"), "Window Size input:",
                        min = 0, max = input$segMaxWindowSize, value = ifelse(is.null(tempWS), 100, tempWS)),
            
            sliderInput(paste0(i, "Overlap"), "Overlap input:",
                        min = 0, max = 1, value = ifelse(is.null(tempO), 0.5, tempO)),
            
            sliderInput(paste0(i, "Threshold"), "Threshold input:",
                        min = 0, max = 1, value = ifelse(is.null(tempT), 0.9, tempT)),
            
            radioButtons(paste0(i, "Univariate"), "Variate type:",
                         tempUni, ifelse(is.null(tempU), 1, tempU), inline = T)
            
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
    
    # all <- c(AD_GENERAL, segIndVars)
    if(
      is.null(v$data)
      ||
      length(segIndVars) < ncol(v$data)
    ){
      allTabs <- c(AD_GENERAL, segIndVars)
    }else{
      allTabs <- segIndVars
    }
    for(i in allTabs){
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
  # output$segpars2 <- renderTable({
  #   pars <- get_segparameters()
  #   colnames(pars)[colnames(pars) == AD_GENERAL] <- "General"
  #   pars
  # })

  ################################################
  ####### parameter table & segmentation plots
  ################################################
  # Table
  output$segpars <- renderTable({
    get_segtable()
  })
  
  get_segtable <- eventReactive(input$segButton, {
    pars <- isolate(get_segparameters())
    colnames(pars)[colnames(pars) == AD_GENERAL] <- "General"
    pars
  })
  
  

  # Using saved segments
  segmentsUpload <- reactive({
    inFile <- input$segresultImportFile
    
    if (is.null(inFile)){
      return(FALSE)
    }
    
    d <- data.frame( 
      read.csv(
        inFile$datapath
      )
    )
    
    v$segments <- d

    return(TRUE)
  })
  
  output$segplot <- renderUI({
    output$segplot0 <- renderPlot({
      # debug mode
      if(DEBUG_SEGPLOT_ON){
        mock_segplot()
      }
      else
      {
        # when upload a seg result file
        uploaded <- segmentsUpload()
        
        if(input$segresultImport){
          if(uploaded){
            mock_segplot()
          }
        }
        else{
          get_segplot()
        }
      }
    })
    plotOutput("segplot0", width="100%", height="600px")
  })
  
  observe({
    imported <- input$segresultImport
    if(imported){
      shinyjs::disable("segButton")
    }
    else{
      shinyjs::enable("segButton")
    }
  })
  
  mock_segplot <- function(){

    d <- isolate(v$data)

    Ncol <- ncol(d)
    # create n+1 rows in the plots
    par(mfrow=c(Ncol + 1,1), mar=c(0,0,0,0))
    
    for(col in colnames(d)){

      plot(x=1:nrow(d), y=d[,col], type="l",
           xaxt="n", yaxt="n",
           xlab="", ylab="", main="")
      abline(v = v$segments$segStart, col="blue")
    }
    
    plot(x=1:nrow(d), y=rep(1, nrow(d)), type="n",
         xaxt="n", yaxt="n",
         xlab="", ylab="", main="")
    # new segment event line
    abline(v = v$segments$segStart, col="blue")
    
  }
  
  
  # after clicking the "start" button
  get_segplot <- eventReactive(input$segButton, {
    
    pars <- isolate(get_segparameters())
    v$segparsPrev <- pars
    segThrottle <- isolate(input$segThrottle)
  
    segLSDDPars <- data.frame(
      row.names = c('sigma','lambda')
    )
    segLSDDUnion <- c()
    
    d <- isolate(v$data)

    Ncol <- ncol(d)
    # create n+1 rows in the plots
    par(mfrow=c(Ncol + 1,1), mar=c(0,0,0,0))
    
    # apply LSDD to each of variable one by one
    withProgress(message = 'Segmenting in progress',
                 detail = 'Please wait...', value = 0,
    {
      ####### Starting LSDD
      for(j in 1:Ncol){
        # if the variable is not individually defined
        col <- colnames(d)[j]
        if(!(col %in% colnames(pars))){
          par <- AD_GENERAL
        }else{
          par <- col
        }
  
        LSDDResult<- LSDDsegment(
                      d[col],
                      windowSize=pars["WindowSize", par],
                      overlap=pars["Overlap", par],
                      thres=pars["Threshold", par],
                      LSDDparameters=T,
                      univariate=pars["Univariate", par]
                    )
        
        # every time finish one varialbe 
        # update the whole process while 1/N
        incProgress(1/Ncol, detail = paste(percent(j/Ncol), "done..."))
        
        plot(x=1:nrow(d), y=d[,col], type="l",
             xaxt="n", yaxt="n",
             xlab="time series index", ylab=par, main="")
        abline(v = LSDDResult$segStart, col="blue")
        abline(v = tail(LSDDResult$segEnd, 1), col="blue")
        # normally: thisEnd == nextStart - 1 , or == endOfAll
        # so no need to print all end event lines, since they are distracting
        # abline(v = LSDDResult$segEnd, col="red")
    
        # sigma/lambda are stable for one particular variable
        segLSDDPars[col] <- as.numeric(c(LSDDResult$sigma, LSDDResult$lambda))
        segLSDDUnion <- c(segLSDDUnion, LSDDResult$segStart)
  
      }
      ####### Finishing LSDD               
    })
    
    segLSDDUnion <- sort(unique(segLSDDUnion))
    
    # after we have all seg line
    segResults <- data.frame(
      segStart = segLSDDUnion,
      segEnd = tail(c(segLSDDUnion - 1, nrow(d)), -1)
      # col.names = c("start", "end")
    )

    # merge segements that below minimum size, using LSDDfast to compare
    # which direction to merge
    segments <- segMerge(data = d,
                         segResults = segResults,
                         segLSDDPars = segLSDDPars,
                         throttle = segThrottle
                        )

    # for biclustering usage
    v$segments <- segments
    v$LSDDPars <- segLSDDPars

    # plotting union segment results
    plot(x=1:nrow(d), y=rep(1, nrow(d)), type="n",
         xaxt="n", yaxt="n",
         xlab="time series index", ylab="", main="")
    # new segment event line
    abline(v = segments$segStart, col="blue")
    abline(v = nrow(d), col="blue")
    # dotted merged event line
    abline(v = setdiff(segLSDDUnion, segments$segStart), col="red", lty = 3)


  })
  
  
  output$segresultSave <- downloadHandler(
    filename = function() {
      paste('segments-', Sys.time(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(v$segments, file, row.names=FALSE)
    }
  )

  #########################################################
  ###################### observe button ###################
  #########################################################
  # disable and re-enable the button
  observeEvent(input$segButton, {

    # to copy previous plot and info to the history
    js$updateSeg()
    
    # disable button for 2s to prevent multiple time application
    html("segButton", "Running...")
    disable("segButton")
    
    # it looks like this 2000ms will make sure
    # the button is unclickable until the server is not busy
    delay(2000, {
      html("segButton", 'Start')
      enable("segButton")
    })
  })
  
  observeEvent(input$segPrev, {
    js$prevSeg()
  })
  
  observeEvent(input$segNext, {
    js$nextSeg()
  })
  
  observeEvent(input$segSave, {
    js$saveSeg()
  })

  
  
  #########################################################
  ################ observe column names ###################
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
    updateSelectInput(session, "segIndVars", choices = dCol)
    
    # TODO: limit the size of the window smaller than the size of all data
    
    
  })

  # Conditions
  observe({
    # get index of rows according to the conditions
    rowSelected <- tryCatch({
      aIndex <- do.call(input$equalCon, list(
          v$data[input$variableCon],
          input$numberCon
        )
      )
      paste0(sum(aIndex)," rows selected")
    },
    error = function(error){
      return('')
    })

    output$rowSelected <- renderText({
      rowSelected
    })
  })
  
    
  ###################################################
  ################# biclustering Tab ################
  ###################################################

  if(DEBUG_SEGPLOT_ON){
    # for 5000 csv
    v$segments <- data.frame(
      segStart = c(1,391,631,1591,2071,2311,2551,3991,4711),
      segEnd = c(390,630,1590,2070,2310,2550,3990,4710,5335)
    )
    v$LSDDPars <- data.frame(
      sigma = c(0.25,2.625,0.25,0.25),
      lambda = c(0.001,5.0005,0.001,0.003162)
    )
  }

  # get_biplot <- eventReactive(input$biButton, {
  #   # see which tab is selected
  #   method <- input$biMethod
  #   
  #   if(method == "Baseline"){
  #     prefix <- "baselineBi"  
  #   }
  #   else if(method == "PDF"){
  #     prefix <- "PDFBi"  
  #   }
  #   else if(method == "LSDD"){
  #     prefix <- "LSDDBi"  
  #   }
  #   
  #   # go for baseline first
  #   pars <- c('Seg', 'Delta', 'Alpha', 'K')
  #   pars2 <- sapply(pars, function(i){
  #     list(
  #       input[[ paste0(prefix, i) ]] 
  #     )
  #   })
  #   
  #   # TODO when select segments on, check whether done segmentation
  #   
  #   if(prefix == "baselineBi"){
  #     
  #     fit <- plotCCBiclustering(v$data, v$segments, delta=pars2$Delta, alpha=pars2$Alpha, number=pars2$K)
  #     # get result and then start plotting
  #     
  #     aSegStart <- v$segments$segStart
  #     aSegEnd <- v$segments$segEnd
  #     data <- v$data
  #     K <- fit@Number
  #     Ncol <- ncol(data)
  #     Nrow <- nrow(data)
  #     
  #     ###################### Try normal plotting  ######################
  #     ##################################################################
  #     
  #     # open a new window and plot the original time series
  #     par(mfrow=c(1,1))
  #     plot(x=1:Nrow,y=data[,1], type="l",xlab="time series index",ylab="",main="CC Biclustering, Aggregated.",ylim=c(min(data,na.rm=TRUE), max(data,na.rm=TRUE)))
  #     columns <- 1:Ncol
  #     for(j in 2:Ncol) {
  #       lines(x=1:Nrow, y=data[,j], type="l")
  #     }
  #     abline(v = c(aSegStart,Nrow), col="blue")
  #     # identify the biclusters on the time series
  #     for(k in 1:K) {
  #       aStart <- aSegStart[fit@RowxNumber[,k]]
  #       aEnd <- aSegEnd[fit@RowxNumber[,k]]
  #       S <- length(aStart)
  #       C <- fit@NumberxCol[k,]
  #       for(i in 1:S) {
  #         for(j in columns[C])
  #           lines(x=aStart[i]:aEnd[i], y=data[aStart[i]:aEnd[i],j], col=k+1, type="l")
  #       }
  #     }
  #     
  #   }
  #   
  #   
  #   if(prefix == 'PDFBi'){
  #     result <- PDFbiclustering(data = v$data, segments = v$segments, delta = 0.0000000001, k = 2)
  #     
  #     # TODO: return all results and then call plots function
  #     # biplot(result)
  #   }
  #   
  # })
  # 
  # output$biplot <- renderUI({
  #   
  #   output$biplot0 <- renderPlot({
  #     get_biplot()
  #   })
  #   plotOutput("biplot0", width="100%", height="600px")
  # 
  # })
  


  ###################### Try dygraph + distribution  ###############
  ##################################################################
    
  get_biGraph <- eventReactive(input$biButton, {

    # see which tab is selected
    func <- input$biFunctions
    
    if(func == "Baseline"){
      prefix <- "baselineBi"  
    }
    else if(func == "PDF"){
      prefix <- "PDFBi"  
    }
    else if(func == "LSDD"){
      prefix <- "LSDDBi"  
    }
    
    # all options here
    # if the option name is missing in one tab
    # it would be null and discarded
    parOptions <- c('Seg', 'Delta', 'Alpha', 'K')
    pars <- list()
    for(par in parOptions){
      temp <- input[[ paste0(prefix, par) ]] 
      if(!is.null(temp)){
        pars[[par]] <- temp
      }
    }

    v$bipars <- pars
    v$bifunc <- func
    
    data <- v$data
    Ncol <- ncol(data)
    Nrow <- nrow(data)

    # TODO when select segments on, check whether done segmentation
    segOn <- TRUE
    if(!is.null(pars$Seg)){
      segOn <- pars$Seg
    }
    
    if(segOn){
      segments <- v$segments
      aSegStart <- segments$segStart
      aSegEnd <- segments$segEnd
    }
    else{
      # when the option is turned off, we choose not to aggregate points
      # with the segmentation result,
      # and it contains #Nrow segments, which means every single point is a segment
      segments <- NULL
      aSegStart <- 1:Nrow
      aSegEnd <- 1:Nrow
    }
    
    progressBar = TRUE
    withProgress(message = 'Biclustering in progress',
                 detail = 'Please wait...', value = 0,
      {
        if(prefix == "baselineBi"){
          fit <- baselineBiclustering(data=data, segments=segments, method=BCCC(), delta=pars$Delta, alpha=pars$Alpha, number=pars$K)
        }
        else if(prefix == "PDFBi"){
          fit <- PDFbiclustering(data=data, segments=segments, delta=pars$Delta, k=pars$K, progressBar=progressBar)
        }
        else if(prefix == "LSDDBi"){
          # segments should contain sigma and lambda for LSDD
          # make as list, since sigma and segStart could have different length
          segments <- as.list(segments)
          if(is.null(v$LSDDPars)){
            return("NO Sigma & Lamba found for LSDD")
          }
          segments$sigma <- v$LSDDPars$sigma
          segments$lambda <- v$LSDDPars$lambda
          fit <- LSDDbiclustering(data=data, segments=segments, delta=pars$Delta, k=pars$K, progressBar=progressBar)
        }
      }
    )

    # store this information
    v$bifit <- fit

    # update bicluster selector
    K <- fit@Number
    choices <- 1:K
    names(choices) <- paste0("Bicluster #", c(1:K))
    updateSelectInput(session, "biclusterSelector", choices = choices)
    
    # dygraph of series and biclusters
    output$biDygraph <- renderUI({
      get_biDygraph(data = data, fit = fit, aSegStart = aSegStart, aSegEnd = aSegEnd, segOn = segOn)
    })
    dygraph_div <- uiOutput('biDygraph')
    
    # density functions
    output$biDensity <- renderPlot({
      get_biDensity(data = data, fit = fit, aSegStart = aSegStart, aSegEnd = aSegEnd)
    })
    density_div <- plotOutput('biDensity', width ="15%", height = "600px")
    
    
    # organize result
    result_div <- tagAppendChildren(div(), density_div, dygraph_div)
    result_div
    
  })
  


  ###################### Try dygraph + distribution  ###############
  ##################################################################
  get_biDygraph <- function(data, fit, aSegStart, aSegEnd, segOn){
    
    selected <- input$biclusterSelector
    # cat(selected)
    
    Ncol <- ncol(data)
    Nrow <- nrow(data)
    K <- fit@Number
    
    all_dygraph <- div(id="biDygraphAll")

    now_time <- as.numeric(as.POSIXct(Sys.time()))
      
    dygraph_div <- lapply(colnames(data), function(i){
      # bind biclusters into a time series
      X <- seq(from = 1, to = Nrow)
      Bicluster <- rep(NA, Nrow)
      
      #the biclusters selected will be shown
      if(is.null(selected)){
        range <- 1:K
      }
      else{
        range <- as.numeric(selected)
      }

      columns <- 1:Ncol
      for(k in range) {
        aStart <- aSegStart[fit@RowxNumber[,k]]
        aEnd <- aSegEnd[fit@RowxNumber[,k]]
        S <- length(aStart)
        C <- fit@NumberxCol[k,]
        for(ind in 1:S) {
          # the column is included in the biclusters
          if(any(which(i == colnames(data)) == columns[C])){
            # cat('i ',i, 'start', aStart[i], 'end', aEnd[i], 'j ',j, '\n')
            if(segOn){
              Bicluster[aStart[ind]:aEnd[ind]] <- k
            }
            else{
              # when uncheck using segments (only in basaeline biclustering)
              # all shading parts become a single point and thus use event line
              Bicluster[aStart[ind]] <- k
            }
          }
        }
      }
      target <- data.frame(X, data[i], Bicluster)

      # for each variable print time series
      tempName <- paste0("biDygraph_", i, '_', now_time)
      
      output[[tempName]] <- renderDygraph({
        
        # plot series
        g <- dygraph(target, main="", group="biDygraphs") %>%
          dyAxis("x", drawGrid = FALSE, labelWidth=0, labelHeight=0, axisLabelColor="White", axisLabelWidth=0, axisLabelFontSize=0, axisHeight=0) %>%
          dyAxis("y", drawGrid = FALSE, label = i) %>%
          dyAxis("y2", drawGrid = FALSE, label = "", axisLabelWidth=0) %>%
          dyOptions(colors = "black") %>%
          dyLegend(show = "onmouseover") %>%
          dyHighlight(highlightCircleSize = 0, highlightSeriesBackgroundAlpha=0.9) %>%
          dySeries("Bicluster", axis = "y2", strokeWidth=0)

        # the biclusters selected will be shown
        # if(is.null(selected)){
        #   range <- 1:K
        # }
        # else{
        #   range <- as.numeric(selected)
        # }

        # add alpha
        # color <- add.alpha(color, alpha=0.4)
        colorMax <- 8
        # get colors
        # There are only 8 colors in set2
        colors = RColorBrewer::brewer.pal(colorMax, "Set2")
        
        columns <- 1:Ncol
        for(k in range) {
          aStart <- aSegStart[fit@RowxNumber[,k]]
          aEnd <- aSegEnd[fit@RowxNumber[,k]]
          S <- length(aStart)
          C <- fit@NumberxCol[k,]
          for(ind in 1:S) {
            # the column is included in the biclusters
            if(any(which(i == colnames(data)) == columns[C])){
              if(segOn){
                g <- dyShading(g, from = aStart[ind], to = aEnd[ind], color = colors[k %% colorMax + 1])
              }
              else{
                # when uncheck using segments,
                # all shading parts become a single point and thus use event line
                g <- dyEvent(g, x = aStart[ind], color = colors[k %% colorMax + 1])
              }
            }
          }
        }
        
        # only show segment line when seg box checked
        if(segOn){
          for(segline in aSegStart){
            g <- dyEvent(g, segline, labelLoc = "bottom", color = "blue",  strokePattern = "solid")
          }
        }

        g
        
      })
      
      dygraphOutput(tempName, width = "85%", height = paste0(round(600/Ncol),"px"))
      
    })

    all_dygraph <- tagAppendChild(all_dygraph, dygraph_div)
  }
  
  get_biDensity <- function(data, fit, aSegStart, aSegEnd){

    selected <- input$biclusterSelector

    K <- fit@Number
    # the biclusters selected will be shown
    if(is.null(selected)){
      range <- 1:K
    }
    else{
      range <- as.numeric(selected)
    }

    Nrow <- nrow(data)
    Ncol <- ncol(data)

    # par(mfrow=c(Ncol,1), mar=c(2.5,2,1.5,2))
    par(mfrow=c(Ncol,1), mar=c(0.7, 0.2,0,0))
    
    # if one variable is not linked to any bicluster
    # newColIndex[j] should == 0
    # newColIndex <- colSums(fit@NumberxCol)
    
    for(j in 1:Ncol){
      # biclustering result could not contain such infomation
      # then just plot distribution of all data
      # 
      # mainly for  PDF
      if(
           any("newRowIndex" == slotNames(fit))    
      )
      {
        
        newRowIndex <- fit@newRowIndex # the segments
        segsIncluded <- fit@NumberxCol[, j] # whether a bicluster is included for a variable
        segsRow <- newRowIndex[range][segsIncluded[range]] # the index of rows of selected biclusters
        segsRowIndex <- c()
        if(length(segsRow) > 0){
          for(i in 1:length(segsRow)){
            segsRowIndex <- c(segsRowIndex, segsRow[[i]])
          }
        }
        segsRowIndex <- sort(unique(segsRowIndex))
        
        pdfA <- density(data[,j], from=0, to=1, n=2^10)$y
        
        if(!is.null(segsRowIndex)){
          pdfB <- density(data[segsRowIndex,j], from=0, to=1, n=2^10)$y
          aYmax <- max(pdfA, pdfB)
        # if(TRUE || any(fit@NumberxCol == j)) {
          plot(x=seq(from=0, to=1, length.out=2^10), y=pdfA, type="l", xaxt="n", yaxt="n", ann=FALSE, ylab="", main="", col="black", ylim=c(0, aYmax))
          lines(x=seq(from=0, to=1, length.out=2^10), y=pdfB, col="red", type="l")
        }
        # if there are not segments for the selected biclusters
        # only show over all density
        else {
          aYmax <- max(pdfA)
          plot(x=seq(from=0, to=1, length.out=2^10), y=pdfA, type="l", xaxt="n", yaxt="n", ann=FALSE, ylab="", main="", col="black", ylim=c(0, aYmax))
          # lines(x=seq(from=0, to=1, length.out=2^10), y=pdfB, col="grey", type="l")
        }
       
        
      }
      # for other methods
      #
      # Baseline, LSDD
      else{
        # plot(density(data[,j]), main="", axes=FALSE)
        plot(density(data[,j]), main="", xaxt="n", yaxt="n", ann=FALSE)
      }
    }
  }
  
  output$biGraph <- renderUI({
    get_biGraph()
  })

  output$bipars <- renderTable({
    input$biButton
    pars <- v$bipars
    if(is.null(pars)){
      return()
    }
    else{
      data.frame(
        # pars, 
        lapply(pars, function(i){as.character(i)}),
        row.names = v$bifunc)
    }
  })

  
  # output$biSave2 <- downloadHandler(
  #   filename = function() {
  #     paste0('bicluster-', Sys.Date(), '.png')
  #   },
  #   content = function(file) {
  #     # write.csv(js$, file)
  #     png(file)
  #     # data, fit, aSegStart, aSegEnd, segOn
  #     print(get_biDygraph(v$data ,v$bifit, v$segments$segStart, v$segments$segEnd, TRUE))
  #     dev.off()
  #   }
  # )


  #########################################################
  ###################### observe button ###################
  #########################################################
  # disable and re-enable the button
  observeEvent(input$biButton, {

    # to copy previous plot and info to the history
    js$updateBi()
    
    # disable button for 2s to prevent multiple time application
    html("biButton", "Running...")
    disable("biButton")
    
    # it looks like this 2000ms will make sure
    # the button is unclickable until the server is not busy
    delay(2000, {
      html("biButton", 'Start')
      enable("biButton")
    })
  })
  
  observeEvent(input$biPrev, {
    js$prevBi()
  })
  
  observeEvent(input$biNext, {
    js$nextBi()
  })

  observeEvent(input$biSave, {
    js$saveBi()
  })
  
})
