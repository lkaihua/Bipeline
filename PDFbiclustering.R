source('AUTOmfrow.R')

PDFbiclustering <- function(data, segments, delta, k) {
  
  
  # use information from segmented data
  aSegStart <- segments$segStart
  aSegEnd <- segments$segEnd
  # LSDDsigma <- segments$sigma
  # LSDDlambda <- segments$lambda
  
  # initialization
  segIndex <- 1:length(aSegStart)
  colIndex <- 1:ncol(data)
  rowIndex <- 1:nrow(data)
  newSegIndex <- segIndex
  newColIndex <- colIndex
  newRowIndex <- rowIndex
  nClusters <- 1
  scores <- c()
  

  # iterations to find k biclusters
  while(length(newSegIndex) != 0 && nClusters <= k) {
    
    # bookeeping
    sRemoved <- c()
    cRemoved <- c()
    rRemoved <- c()
    
    # calculate LSDD for all segments and variables part of bicluster
    KDEscores <- matrix(data=NA, ncol=ncol(data), nrow=length(aSegStart))
    for(j in newColIndex)
      KDEscores[newSegIndex,j] <- sapply(newSegIndex, function(i) 1/sqrt(2)*sqrt(sum((sqrt(density(data[aSegStart[i]:aSegEnd[i],j], from=0, to=1, n=2^10)$y) - sqrt(density(data[newRowIndex,j], from=0, to=1, n=2^10)$y))^2)))
    # calculate stop policy
    score <- mean(KDEscores[newSegIndex,newColIndex], na.rm=FALSE)
    print(score)
    
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
        rRemoved <- c(rRemoved, aSegStart[sIndex]:aSegEnd[sIndex])
        newSegIndex <- newSegIndex[-which(newSegIndex == sIndex)]
        newRowIndex <- newRowIndex[-sapply(c(aSegStart[sIndex]:aSegEnd[sIndex]), function(i) which(newRowIndex == i))]
      } else {
        cRemoved <- c(cRemoved, cIndex)
        newColIndex <- colIndex[-cRemoved]
      }
      
      # calculate LSDD for all segments and variables part of bicluster
      KDEscores[] <- NA
      for(j in newColIndex)
        KDEscores[newSegIndex,j] <- sapply(newSegIndex, function(i) 1/sqrt(2)*sqrt(sum((sqrt(density(data[aSegStart[i]:aSegEnd[i],j], from=0, to=1, n=2^10)$y) - sqrt(density(data[newRowIndex,j], from=0, to=1, n=2^10)$y))^2)))
      # calculate stop policy
      score <- mean(KDEscores[newSegIndex,newColIndex], na.rm=FALSE)
      print(score)
      
      # find the removed segments or variables with the smallest probability distance (ADD)
      #KDEscoresToAdd <- matrix(data=NA, ncol=ncol(data), nrow=length(aSegStart))
      #for(j in newColIndex)
      #  KDEscoresToAdd[sRemoved,j] <- sapply(sRemoved, function(i) 1/sqrt(2)*sqrt(sum((sqrt(density(data[aSegStart[i]:aSegEnd[i],j], from=0, to=1, n=2^10)$y) - sqrt(density(data[newRowIndex,j], from=0, to=1, n=2^10)$y))^2)))
      #for(j in cRemoved)
      #  KDEscoresToAdd[newSegIndex,j] <- sapply(newSegIndex, function(i) 1/sqrt(2)*sqrt(sum((sqrt(density(data[aSegStart[i]:aSegEnd[i],j], from=0, to=1, n=2^10)$y) - sqrt(density(data[-rRemoved,j], from=0, to=1, n=2^10)$y))^2)))
      #rMeans <- rowMeans(KDEscoresToAdd[,newColIndex], na.rm = TRUE)
      #rToAdd <- which(sRemoved == which(rMeans <= score))
      #if(length(rToAdd) != 0)
      #  print(paste("Segment to be added:", rToAdd))
        #TODO: add segment (Ricardo)
      #cMeans <- colMeans(KDEscoresToAdd[newSegIndex,], na.rm = TRUE)
      #cToAdd <- which(cRemoved == which(cMeans <= score))
      #if(length(cToAdd) != 0)
      #  print(paste("Variable to be added:", cToAdd))
      #TODO: add the column (Ricardo)
    }
    
    # TODO: plot bicluster and distributions (Kai)
    
    # plot bicluster
    par(mfrow=c(ncol(data),1), mar=c(0,0,0,0))
    for(j in 1:ncol(data)) {
      if(any(newColIndex == j)) {
        plot(x=1:nrow(data), y=data[,j], type="l", xaxt="n", yaxt="n", ylab="", main="")
        for(i in newSegIndex)
          lines(x=aSegStart[i]:aSegEnd[i], y=data[aSegStart[i]:aSegEnd[i],j], col="red", type="l")
      }
      else
        plot(x=1:nrow(data), y=data[,j], type="l", xaxt="n", yaxt="n", ylab="", main="", col="grey")
    }
    
    # plot distributions
    # auto.mfrow(ncol(data))
    par(mfrow=c(ncol(data),1), mar=c(0,0,0,0))
    for(j in 1:ncol(data)) {
      # kai:
      # sometimes rReomoved will be null
      # if null then only pdfB
      if(is.null(rRemoved)){
        pdfA <- seq(from=0,to=0,length.out=2^10) #density(c(0,0), from=0, to=1, n=2^10)$y
      }
      else{
        pdfA <- density(data[rRemoved,j], from=0, to=1, n=2^10)$y
      }

      pdfB <- density(data[newRowIndex,j], from=0, to=1, n=2^10)$y
      aYmax <- max(pdfA, pdfB)
      if(any(newColIndex == j)) {
        plot(x=seq(from=0, to=1, length.out=2^10), y=pdfA, type="l", xaxt="n", yaxt="n", ylab="", main="", ylim=c(0, aYmax))
        lines(x=seq(from=0, to=1, length.out=2^10), y=pdfB, col="red", type="l")
      }
      else {
        plot(x=seq(from=0, to=1, length.out=2^10), y=pdfA, type="l", xaxt="n", yaxt="n", ylab="", main="", col="grey", ylim=c(0, aYmax))
        lines(x=seq(from=0, to=1, length.out=2^10), y=pdfB, col="grey", type="l")
      }
    }
    
    # book keeping
    nClusters <- nClusters + 1
    newSegIndex <- sort(segIndex[unique(sRemoved)])
    newRowIndex <- sort(rowIndex[unique(rRemoved)])
    newColIndex <- colIndex
  }
}
