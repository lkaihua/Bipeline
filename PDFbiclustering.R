# Author: Ricardo Cachucho
# Edited by Kai on March 2016
#   
# Update: add progressBar option
###########################################################################################################

PDFbiclustering <- function(data, segments, delta, k, progressBar=FALSE) {
  
  # use information from segmented data
  aSegStart <- segments$segStart
  aSegEnd <- segments$segEnd
  
  # initialization
  segIndex <- 1:length(aSegStart)
  colIndex <- 1:ncol(data)
  rowIndex <- 1:nrow(data)
  newSegIndex <- segIndex
  newColIndex <- colIndex
  newRowIndex <- rowIndex
  nClusters <- 1
  scores <- c()
  Biclu <- setClass(
    # Set the name for the class
    "Biclu",
    
    # Define the slots
    slots = c(
      Number = "numeric",
      RowxNumber   = "matrix",
      NumberxCol   = "matrix",
      rRemoved = "list",
      newRowIndex = "list"
    ),
    
    # Set the default values for the slots. (optional)
    prototype=list(
      Number = c(0),
      RowxNumber = matrix(data=FALSE, nrow = length(segIndex), ncol = k),
      NumberxCol = matrix(data=FALSE, nrow = k, ncol = length(colIndex))
    )
  )
  output <- Biclu()
  
  # iterations to find k biclusters
  while(length(newSegIndex) != 0 && nClusters <= k) {
    
    # bookeeping
    sRemoved <- c()
    cRemoved <- c()
    rRemoved <- c()
    
    # estimate PMFs and HD for all segments and variables part of bicluster
    KDEscores <- matrix(data=NA, ncol=ncol(data), nrow=length(aSegStart))
    for(j in newColIndex)
      KDEscores[newSegIndex,j] <- sapply(newSegIndex, function(i) 1/sqrt(2)*sqrt(sum((sqrt(density(data[aSegStart[i]:aSegEnd[i],j], from=0, to=1, n=2^10)$y) - sqrt(density(data[newRowIndex,j], from=0, to=1, n=2^10)$y))^2)))
    # calculate stop policy
    score <- mean(KDEscores[newSegIndex,newColIndex], na.rm=FALSE)
    print(score)
    
    # delta is between 0 and 1
    delta <- delta * score
    
    # an estimation of all work
    if(progressBar){
      allProgress <- min(length(newSegIndex), k)
      nowProgress <- (nClusters - 1)/allProgress
    }
    
    # iterations to remove columns and segments
    while(length(newSegIndex) != 0 && score > delta) {
      # bookeeping scores
      scores <- c(scores, score)
      
      # find the segments or variables with the largest probability distance (REMOVE)
      rMeans <- rowMeans(KDEscores, na.rm = TRUE)
      sIndex <- which(rMeans == max(rMeans, na.rm=TRUE))
      cMeans <- colMeans(KDEscores, na.rm = TRUE)
      cIndex <- which(cMeans == max(cMeans, na.rm=TRUE))
      if(length(newColIndex) < 2 || rMeans[sIndex] >= cMeans[cIndex]) {
        sRemoved <- c(sRemoved, sIndex)
        rRemoved <- c(rRemoved, c(aSegStart[sIndex]:aSegEnd[sIndex]))
        newSegIndex <- newSegIndex[-which(newSegIndex == sIndex)]
        newRowIndex <- newRowIndex[-sapply(c(aSegStart[sIndex]:aSegEnd[sIndex]), function(i) which(newRowIndex == i))]
      } else {
        cRemoved <- c(cRemoved, cIndex)
        newColIndex <- colIndex[-cRemoved]
      }
      
      # estimate PMFs and HD for all segments and variables part of bicluster
      KDEscores[] <- NA
      for(j in newColIndex)
        KDEscores[newSegIndex,j] <- sapply(newSegIndex, function(i) 1/sqrt(2)*sqrt(sum((sqrt(density(data[aSegStart[i]:aSegEnd[i],j], from=0, to=1, n=2^10)$y) - sqrt(density(data[newRowIndex,j], from=0, to=1, n=2^10)$y))^2)))
      # calculate stop policy
      score <- mean(KDEscores[newSegIndex,newColIndex], na.rm=FALSE)
      print(score)
      
      # estimate stop time
      if(progressBar){
        scoreL <- length(scores)
        if(scoreL > 1){
          done <- scores[1] - scores[scoreL]
          distance <- scores[1] - delta
          value <- nowProgress + (1/allProgress)*(done/distance)
          setProgress(value = value, detail = paste(percent(value),"done..."))
        }
      }
    }
    
    # output
    output@RowxNumber[newSegIndex,nClusters] <- TRUE
    output@NumberxCol[nClusters,newColIndex] <- TRUE
    output@rRemoved[[nClusters]] <- rRemoved
    output@newRowIndex[[nClusters]] <- newRowIndex
    
    # book keeping
    print(paste('number of clusters found:', nClusters))
    nClusters <- nClusters + 1
    newSegIndex <- sort(segIndex[unique(sRemoved)])
    print(paste("number of segments remaining to be biclustered:", length(newSegIndex)))
    newRowIndex <- sort(rowIndex[unique(rRemoved)])
    newColIndex <- colIndex
  }
  
  # return
  output@Number <- nClusters - 1
  return(output)
}
