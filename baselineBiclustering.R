# The function plotKMeans returns a plot of clustered data using k-means
# 
# Author: Ricardo Cachucho
# Edited by kai on Mar 2016
###########################################################################################################
library("biclust")

# Ploting the results of Cheng and Church biclustering on a time series
baselineBiclustering <- function(data, segments=NULL, method=BCCC(), delta=0.01, alpha=1, number=100) {
  
  # if segments are NULL, then skip aggregate part
  if(is.null(segments)){
    AggregateData <- as.matrix(data)
  }
  else{
    # represent the time series in an aggregated form
    aSegStart <- segments$segStart
    aSegEnd <- segments$segEnd
    
    Nrow <- length(aSegStart)
    Ncol <- ncol(data)
    AggregateData <- matrix(data=NA, ncol=Ncol, nrow=Nrow)
    for(j in 1:Ncol){
      AggregateData[,j] <- sapply(seq(1,Nrow,1), function(i) mean(data[aSegStart[i]:aSegEnd[i],j], na.rm=TRUE))
    }
  }
  
  fit <- biclust(AggregateData, method=method, delta=delta, alpha=alpha, number=number)
  
  
  # plot is done in another module
  
  # K <- fit@Number
  # # make plot of original time series
  # N <- nrow(data)
  # par(mfrow=c(1,1))
  # plot(x=1:N,y=data[,1], type="l",xlab="time series index",ylab="", main="Biclustering time series.",ylim=c(min(data,na.rm=TRUE), max(data,na.rm=TRUE)))
  # if(ncol(data) > 1) {
  #   for(c in 2:ncol(data))
  #     lines(x=1:N, y=data[,c], type="l")
  # }
  # # plot with points the biclusters
  # i=1
  # for(i in 1:N) {
  #   if(any(fit@RowxNumber[i,])==TRUE) {
  #     a <- which(fit@RowxNumber[i,] == TRUE)
  #     c <- which(fit@NumberxCol[a,] == TRUE)
  #     temp <- data[i, c]
  #     points(x=rep(i, length(temp)), y=data[i,c], col=a+1)
  #   }
  # }
}
