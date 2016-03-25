LSDDsegment <- function(data, windowSize=100, overlap=0.5, thres=0.9, LSDDparameters=FALSE, univariate=TRUE) {
  if(univariate == TRUE)
    return(LSDD.uniSegment(data=data, windowSize=windowSize, overlap=overlap, thres=thres, LSDDparameters=LSDDparameters))
  else
    return(LSDD.multiSegment(data=data, windowSize=windowSize, overlap=overlap, thres=thres, LSDDparameters=LSDDparameters))
}

LSDD.uniSegment <- function(data, windowSize=100, overlap=0.5, thres=0.9, LSDDparameters=FALSE) {
  # prepare variables for segmentation
  aSegStart <- c()
  aSegEnd <- c()
  sigmas <- vector(mode="numeric", length=ncol(data))
  lambdas <- vector(mode="numeric", length=ncol(data))
  
  # start segmentation process
  Ncol <- ncol(data)
  # par(mfrow=c(Ncol,1), mar=c(0,0,0,0))
  for(j in 1:Ncol) {
    # calculate L2-Distance
    divergence <- streamingLSDD(data[,j], windowSize=windowSize, overlap=overlap, parameters=TRUE)
    sigmas[j] <- divergence$sigma
    lambdas[j] <- divergence$lambda
    
    # find local maximums above a threshold. choice :CDF(LSDD) = 0.9
    threshold <- quantile(divergence$LSDD, probs=thres)
    aMaxIndex <- which(diff(sign(diff(divergence$LSDD)))==-2)+1
    aMaxIndex <- aMaxIndex[which(divergence$LSDD[aMaxIndex] >= threshold)]
    
    # define the beginning and end of a segment
    aSegStartTemp <- c(1, aMaxIndex+1)
    aSegStart <- c(aSegStart, aSegStartTemp)
    aSegEndTemp <- c(aMaxIndex, length(divergence$LSDD))
    aSegEnd <- c(aSegEnd, aSegEndTemp)
    
    # plot the segmented variable
    # plot(x=1:nrow(data), y=data[,j], type="l", xaxt="n", yaxt="n",xlab="time series index", ylab="", main="uni")
    # abline(v = aSegStartTemp, col="blue")
  }
  
  # merge (union) and sort all the segments
  aSegStart <- sort(unique(aSegStart))
  aSegEnd <- sort(unique(aSegEnd))
  
  #output the segments and LSDD paramenters to be used in biclustering
  if(LSDDparameters == TRUE) {
    output <- list(segStart=aSegStart, segEnd=aSegEnd, sigma=sigmas, lambda=lambdas)
    return(output)
  }
  
  #output the segments
  output <- list(segStart=aSegStart, segEnd=aSegEnd)
  return(output)
}


LSDD.multiSegment <- function(data, windowSize=100, overlap=0.5, thres=0.9, LSDDparameters=FALSE) {
  # prepare variables for segmentation
  aSegStart <- c()
  aSegEnd <- c()
  sigmas <- vector(mode="numeric", length=ncol(data))
  lambdas <- vector(mode="numeric", length=ncol(data))
  
  # initiate with first column
  j=1
  divergence <- streamingLSDD(data[,j], windowSize=windowSize, overlap=overlap, parameters=TRUE)
  sigmas[j] <- divergence$sigma
  lambdas[j] <- divergence$lambda
  aggregateDiv <- divergence$LSDD
  
  # start segmentation process
  Ncol <- ncol(data)
  for(j in 2:Ncol) {
    # calculate L2-Distance
    divergence <- streamingLSDD(data[,j], windowSize=windowSize, overlap=overlap, parameters=TRUE)
    sigmas[j] <- divergence$sigma
    lambdas[j] <- divergence$lambda
    
    aggregateDiv <- aggregateDiv + divergence$LSDD
  }
  
  #aggregateDiv <- aggregateDiv/Ncol
  
  # find local maximums above a threshold. choice :CDF(LSDD) = 0.9
  threshold <- quantile(divergence$LSDD, probs=thres)
  aMaxIndex <- which(diff(sign(diff(divergence$LSDD)))==-2)+1
  aMaxIndex <- aMaxIndex[which(divergence$LSDD[aMaxIndex] >= threshold)]
  
  # find local maximums withouth threshold
  #aMaxIndex <- which(diff(sign(diff(aggregateDiv)))==-2)+1
      
  # define the beginning and end of a segment
  aSegStart <- c(1, aMaxIndex+1)
  aSegEnd <- c(aMaxIndex, length(divergence$LSDD))
    
  # plot segmented variables
  # par(mfrow=c(Ncol,1), mar=c(0,0,0,0))
  # for(j in 1:Ncol) {
  #   plot(x=1:nrow(data), y=data[,j], type="l", xaxt="n", yaxt="n",xlab="time series index", ylab="", main="")
  #   abline(v = aSegStart, col="blue")
  # }

  #output the segments and LSDD paramenters to be used in biclustering
  if(LSDDparameters == TRUE) {
    output <- list(segStart=aSegStart, segEnd=aSegEnd, sigma=sigmas, lambda=lambdas)
    return(output)
  }
  
  #output the segments
  output <- list(segStart=aSegStart, segEnd=aSegEnd)
  return(output)
}