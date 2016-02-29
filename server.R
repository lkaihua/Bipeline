
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

DEBUG_ON = F

AD_GENERAL = 'AD2016_General'


shinyServer(function(input, output, session) {
  
  ###################################################
  ################# General Tab  ####################
  ###################################################
  
  
  # using reactive to dynamically import dataset
  dataInputRaw <- reactive({
    
    # get file from uploading
    if(!DEBUG_ON){
      inFile <- input$file1
  
      if (is.null(inFile))
        return(NULL)
  
      d <- data.frame( read.csv(inFile$datapath, header=input$header, sep=input$sep,
               quote=input$quote) )
    }else{
    # or debug deafult data
      d <- data.frame( read.csv('./test5000.csv'))  
    }
    
    if (input$normalizing){
      d <- normalizeTS(d)
    }
    
    return(d)
  })
  
  # remove all non-numeric columns
  dataInput <- reactive({
    d <- dataInputRaw()
    d <- d[sapply(d, is.numeric)]
    if (is.null(d)){
      return()
    }else{
      return(d)
    }
  })
  

  # data table with navigation tab
  # renderTable will kill the browser when the data is large
  output$table <- DT::renderDataTable({
    d <- dataInputRaw()
    
    if (is.null(d))
      return()
    
    DT::datatable(d, options = list(pageLength = 10))
  })
    
   
  # data summary
  output$summary <- renderPrint({
    d <- dataInputRaw()
    
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
    
    dygraph(target
            # , main = 'Selected variables'
            )
      
    

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
      
      
      tempName <- paste0("mulplot_dygraph_", i)
      output[[tempName]] <- renderDygraph({
        dygraph(target, main = i, group = 'mulplot') %>%
          dyOptions(colors = "#131688")
      })
      dopOut <- dygraphOutput(tempName, width = "100%", height = "300px")
      # dopOut <- tagAppendChild(dopOut, dop)
      # dop <- dygraphOutput(dop, width = "200px", height = "200px")
      # dopOut <- tagAppendChild(dopOut, dop)
      result_div <- tagAppendChild(result_div, dopOut)
      
    
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
      
      output[[tempName]] <- renderPlot({
        ggplot(target, aes_string(x = inputX, y = i)) + 
          geom_point() + 
          geom_smooth(method = "lm", se = F) +
          ggtitle(i)
      })
      
      dop <- plotOutput(tempName, width = "100%", height = "300px")

      result_div <- tagAppendChild(result_div, dop)
      
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
                         tempUni, inline = T
            )
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
    d <- isolate(dataInput())
    
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
  
  
  
  
})
