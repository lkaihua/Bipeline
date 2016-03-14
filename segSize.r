source('LSDD.R')
source('LSDDsegmentation.R')


segSize <- function(data, segResults, segLSDDPars, throttle = 100){
  
  newSeg <- data.frame()
  
  mergeDirection <- 'left'
  
  for(i in 1:nrow(segResults)){
    # sorry you are too small we have to kill you
    if(segResults[i, "segEnd"] - segResults[i, "segStart"] + 1 <= throttle){
      
      # if it is the first seg or the last seg
      # it only can be merged to one direction 
      # so stop calling the function
      if(i == 1){
        mergeDirection <- 'right'
      }
      else if(i == nrow(segResults)){
        mergeDirection <- 'left'
      } 
      # use LSDD fast function to calculate similarity
      else {
        mergeDirection <- mergeSeg(
          L = segResults[i-1, ],
          G = segResults[i, ],
          R = segResults[i+1, ],
          data = data,
          segLSDDPars = segLSDDPars
        )
      }
      
      if(mergeDirection == "left"){

        # merge to left
        # edit last seg
        newSeg[nrow(newSeg), "segEnd"] <- segResults[i, "segEnd"]
        
      }else{
        # merge to right
        # push to new vector
        newSeg <- rbind(newSeg, 
                            list(
                              "segStart" = segResults[i,"segStart"],
                              "segEnd" = segResults[i+1, "segEnd"]
                            )
        )
        # change next seg to be deal in next loop
        segResults[i+1, "segStart"] = segResults[i, "segStart"]
      }
    }
    # You are long enough to go
    else{
      newSeg <- rbind(newSeg, segResults[i,])
    }
  }
  
  newSeg <- unique(newSeg)
  
}

#### Input
#######################################
# |------------|-----|-----------|
#       L         G        R      
# Compare Gap with two different time sequences
# And return the one with higher similarity
#######################################
# L, G, R, three time sequences to deal with: list("segStart" = , "segEnd" = )
# G is the gap (seg size below the minimum)

# d: dataset
# sigmas: sigma matrix get from segmentation for all variables
# lambdas

#### Output
# "left"/"right", the side to merge

mergeSeg <- function(L, G, R, data, segLSDDPars){
  
  # AD_GENERAL <- "AD2016_GENERAL" # column for all default settings
  sum_LSDD_G2L <- 0
  sum_LSDD_G2R <- 0
  
  # under some extrme conditions
  # the size of G could be 1, which means only 1 data point
  # in this segments,
  # but LSDD requires at least 2 data points to carry on.
  # so just merge G to the shorter side
  if(G$segEnd - G$segStart > 1 
    && L$segEnd - L$segStart > 1   
    && R$segEnd - R$segStart > 1   
  ){

    for(i in 1:ncol(data)){
      # 1 x n matrix
      Gmatrix <- t(matrix(data[G$segStart:G$segEnd, i]))
      Lmatrix <- t(matrix(data[L$segStart:L$segEnd, i]))
      Rmatrix <- t(matrix(data[R$segStart:R$segEnd, i]))
        
      # cat(dim(Gmatrix), dim(Lmatrix), dim(Rmatrix))
      
      sum_LSDD_G2L <- sum(sum_LSDD_G2L, LSDDfast(
          X1 = Gmatrix, 
          X2 = Lmatrix,
          sigma = segLSDDPars["sigma", i],    #sigma
          lambda = segLSDDPars["lambda", i]   #lambda
        )
      )
      
      sum_LSDD_G2R <- sum(sum_LSDD_G2R, LSDDfast(
          X1 = Gmatrix, 
          X2 = Rmatrix,
          sigma = segLSDDPars["sigma", i],    #sigma
          lambda = segLSDDPars["lambda", i]   #lambda
        )
      )
    }
  }
  
  # print(sum_LSDD_G2L)
  # print(sum_LSDD_G2R)
  # cat("Similary to left  is: ", sum_LSDD_G2L, "\n")
  # cat("Similary to right is: ", sum_LSDD_G2R, "\n")
  
  result <- 'left'
  
  if(sum_LSDD_G2R < sum_LSDD_G2L){
    result <- 'right'
  }else if(sum_LSDD_G2L < sum_LSDD_G2R){
    # return('left')
  }else{
    # this is equal, very rare situation
    # merge with shorter segResults
    if(L$segEnd - L$segStart < R$segEnd - R$segStart){
      # return('left')
    }
    else{
      result <- 'right'
    }
    
  }

  # cat("Merge to ", result, "\n")
  result
}

# test

# data <- data.frame( read.csv('./test5000.csv'))  
# data <- data[-1]
#
# segResults <- data.frame( read.csv('./segs5000.csv'))  
# segLSDDPars <- data.frame( read.csv('./pars5000.csv'))  
# 
# segSize(data = data, segResults = segResults, segLSDDPars = segLSDDPars)


