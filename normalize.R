normalizeTS <- function(data) {
  for(j in 1:ncol(data)) {
    # check numeric
    if(sapply(data[j], is.numeric)){
      minData <- min(data[,j])
      maxData <- max(data[,j])
      if(minData < maxData) {
        data[,j] <- (data[,j]-minData)/(maxData-minData)
      }
      # if minData equals maxData exactly
      else{
        data[j] <- 1
      }
    }
  }
  return(data)
}