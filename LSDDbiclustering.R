LSDDbiclustering <- function(data, segments, delta, k) {

  # use information from segmented data
  aSegStart <- segments$segStart
  aSegEnd <- segments$segEnd
  LSDDsigma <- segments$sigma
  LSDDlambda <- segments$lambda

  # initialization
  segIndex <- 1:length(aSegStart)
  colIndex <- 1:ncol(data)
  rowIndex <- 1:nrow(data)
  newSegIndex <- segIndex
  newColIndex <- colIndex
  newRowIndex <- rowIndex
  nClusters <- 1
  scores <- c()
  Bicluster <- setClass(
    # Set the name for the class
    "bicluster",
    
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
  output <- Bicluster()

  # first iteration
  LSDDscores <- matrix(data=NA, ncol=ncol(data), nrow=length(aSegStart))
  for(j in colIndex)
    LSDDscores[segIndex,j] <- sapply(segIndex, function(i) LSDDfast(matrix(data[aSegStart[i]:aSegEnd[i],j], nrow=1, ncol=length(aSegStart[i]:aSegEnd[i])), matrix(data[,j], nrow=1, ncol=length(data[,j])), sigma=LSDDsigma[j], lambda=LSDDlambda[j]))

  # check for stop policy
  score <- mean(LSDDscores[segIndex,colIndex], na.rm=FALSE)
  #print(score)

  while(length(newSegIndex) != 0 && nClusters <= k) {
    
    # bookeeping
    sRemoved <- c()
    cRemoved <- c()
    rRemoved <- c()
    
    # calculate LSDD for all segments and variables part of bicluster
    LSDDscores <- matrix(data=NA, ncol=ncol(data), nrow=length(aSegStart))
    for(j in newColIndex)
      LSDDscores[newSegIndex,j] <- sapply(newSegIndex, function(i) LSDDfast(matrix(data[aSegStart[i]:aSegEnd[i],j], nrow=1, ncol=length(aSegStart[i]:aSegEnd[i])), matrix(data[newRowIndex,j], nrow=1, ncol=length(data[newRowIndex,j])), sigma=LSDDsigma[j], lambda=LSDDlambda[j]))
    score <- mean(LSDDscores[newSegIndex,newColIndex], na.rm=FALSE)
    print(score)
    
    # iterations to remove columns and segments
    while(length(newSegIndex) != 0 && score > delta) {
      
      # bookeeping scores
      scores <- c(scores, score)
      
      # find the row or columns with the largest LSDD average
      rMeans <- rowMeans(LSDDscores, na.rm = TRUE)
      sIndex <- which(rMeans == max(rMeans, na.rm=TRUE))
      cMeans <- colMeans(LSDDscores, na.rm = TRUE)
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

      # calculate LSDD for all segments and variables part of bicluster
      LSDDscores[] <- NA
      for(j in newColIndex)
        LSDDscores[newSegIndex,j] <- sapply(newSegIndex, function(i) LSDDfast(matrix(data[aSegStart[i]:aSegEnd[i],j], nrow=1, ncol=length(aSegStart[i]:aSegEnd[i])), matrix(data[newRowIndex,j], nrow=1, ncol=length(data[newRowIndex,j])), sigma=LSDDsigma[j], lambda=LSDDlambda[j]))
      score <- mean(LSDDscores[newSegIndex,newColIndex], na.rm=FALSE)
      print(score)
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
