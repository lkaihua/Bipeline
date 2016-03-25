library("pcg")

# Least-Squares Density-Difference Estimation

# Estimating p1(x)-p2(x) from samples {x1_i}_{i=1}^{n1} and {x2_j}_{j=1}^{n2}
# drawn i.i.d. from p1(x) and p2(x), respectively.

# Input:
#    X1: d by n1 training sample matrix
#    X2: d by n2 training sample matrix

# Output:
#    L2dist: estimate of L2-distance obtained from X1 and X2
#    ddh:    estimates of p1(x)-p2(x) at T (OPTIONAL)
LSDD <- function(X1, X2) {
  
  d <- dim(X1)[1]
  n1 <- dim(X1)[2]
  dummy <- dim(X2)[1]
  n2 <- dim(X2)[2]
  X <- cbind(X1,X2)
  rm(X1, X2)
  n <- n1 + n2
  b <- min(300,n) # Number of kernel bases

  center_index_tmp <- sample(n)
  center_index <- center_index_tmp[1:b]
  C <- matrix(X[,center_index], nrow=1, ncol=b)
  XXsum <- matrix(colSums(X^2), nrow=1, ncol=n)
  XC_dist2 <- do.call("rbind", rep(list(XXsum), b)) + do.call("cbind", rep(list(as.matrix(XXsum[,center_index])), n)) - 2*t(C)%*%X
  rm(XXsum, X, C)
  
  X1C_dist2 <- XC_dist2[,1:n1]
  X2C_dist2 <- XC_dist2[,(n1+1):n]
  CC_dist2 <- XC_dist2[,center_index]
  rm(XC_dist2)
  
  fold <- 5
  cv_fold <- c(1:fold)
  cv_split1 <- floor(c(0:(n1-1))*fold/n1)+1
  cv_split2 <- floor(c(0:(n2-1))*fold/n2)+1
  cv_index1 <- cv_split1[sample(n1)]
  cv_index2 <- cv_split2[sample(n2)]
  n1_cv <- vector(mode="numeric", length=fold)
  n2_cv <- vector(mode="numeric", length=fold)
  for(k in cv_fold) {
    n1_cv[k] <- sum(cv_index1==k)
    n2_cv[k] <- sum(cv_index2==k)
  }
  
  sigma_list <- c(0.25, 0.5, 0.75, 1, 1.2, 1.5, 2, 3, 5) # Candidates of Gaussian width
  lambda_list <- 10^seq(-3,1,len=9) # Candidates of regularization parameter
  
  score_cv <- array(data=0, dim=c(length(sigma_list),length(lambda_list),fold))  
  nSigma <- length(sigma_list)
  for(s in 1:nSigma) {
    sigma <- sigma_list[s]
    H <- (sqrt(pi)*sigma)^d*exp(-CC_dist2/(4*sigma^2))
    h1_cv <- matrix(nrow=nrow(X1C_dist2), ncol=fold)
    h2_cv <- matrix(nrow=nrow(X2C_dist2), ncol=fold)
    for(k in cv_fold) {
      h1_cv[,k] <- rowSums(exp(-X1C_dist2[,cv_index1==k]/(2*sigma^2)))
      h2_cv[,k] <- rowSums(exp(-X2C_dist2[,cv_index2==k]/(2*sigma^2)))
    }
    for(k in cv_fold) {
      htrain <- rowSums(h1_cv[,cv_fold!=k],2)/sum(n1_cv[cv_fold!=k]) - rowSums(h2_cv[,cv_fold!=k],2)/sum(n2_cv[cv_fold!=k])
      htest <- rowSums(as.matrix(h1_cv[,cv_fold==k]),2)/sum(n1_cv[cv_fold==k]) - rowSums(as.matrix(h2_cv[,cv_fold==k]),2)/sum(n2_cv[cv_fold==k])
      nLambda <- length(lambda_list)
      for(l in 1:nLambda) {
        lambda <- lambda_list[l]
        #thetah <- solve(H+lambda*diag(b),htrain)
        thetah <- pcg(H+lambda*diag(b),htrain)
        score_cv[s, l, k] <- t(thetah)%*%H%*%thetah - 2*t(thetah)%*%as.matrix(htest)
      }
    }
  }
  
  score_cvMean <- apply(score_cv, c(1,2), mean)
  score_cvMin <- which(score_cvMean == min(score_cvMean), arr.ind = TRUE)
  lambda_index <- score_cvMin[1,2]
  sigma_index <- score_cvMin[1,1]
  lambda=lambda_list[lambda_index]
  sigma=sigma_list[sigma_index]
  #print(paste("lambda:", lambda, "|| sigma:", sigma))
  
  H <- (sqrt(pi)*sigma)^d*exp(-CC_dist2/(4*sigma^2))
  h <- rowMeans(exp(-X1C_dist2/(2*sigma^2))) - rowMeans(exp(-X2C_dist2/(2*sigma^2)))
  #thetah <- solve(H+lambda*diag(b),h)
  thetah <- pcg(H+lambda*diag(b),h)
  L2dist <- 2*t(thetah)%*%h - t(thetah)%*%H%*%thetah
  
  return(L2dist)
}


# Change detection for time series using Least-Squares Density-Difference Estimation

# Input:
#    X1: d by n1 training sample matrix
#    X2: d by n2 training sample matrix

# Output:
#    L2dist: estimate of L2-distance obtained from X1 and X2
#    ddh:    estimates of p1(x)-p2(x) at T (OPTIONAL)
LSDDtimeSeries <- function(data, windowSize=100, overlap=1, makePlot=TRUE) {
  # preparing the way we slide the window
  ifelse(overlap==1, jump<-1, jump<-floor((1-overlap)*windowSize))
  halfWindow <- floor(windowSize/2)
  frames <- seq((halfWindow), (length(data) - halfWindow), jump)
  nIterations <- length(frames)
  print(nIterations)
  
  output <- rep(NA, length(data))
  
  for(i in 2:nIterations) {
    X1 <- matrix(data[(frames[i]-halfWindow+1):(frames[i]+halfWindow)], nrow=1, ncol=windowSize)
    X2 <- matrix(data[(frames[i-1]-halfWindow+1):(frames[i-1]+halfWindow)], nrow=1, ncol=windowSize)
    output[i] <- LSDD(X1, X2)
  }
  if(makePlot == TRUE) {
    par(mfrow=c(2,1), mar = rep(2, 4))
    plot(x=1:length(data),y=data, type="l",xlab="time series index",ylab="", main="Original Univariate Time Series.")
    plot(x=1:length(data),y=output, type="l",xlab="time series index",ylab="", main=paste("TS LSDD using window size:",windowSize))
  }
  else
    return(output)
}


# Change detection for time series using Least-Squares Density-Difference Estimation calculated in a streaming fashion

# Input:
#    X1: 1 by n1 training sample matrix
#    X2: 1 by n2 training sample matrix

# Output:
#    L2dist: estimate of L2-distance obtained from X1 and X2
#    ddh:    estimates of p1(x)-p2(x) at T (OPTIONAL)
streamingLSDD <- function(data, windowSize=100, overlap=1, nIterations=10, makePlot=FALSE, parameters=FALSE, segment=FALSE) {
  # preparing the way we slide the window
  ifelse(overlap==1, jump<-1, jump<-floor((1-overlap)*windowSize))
  halfWindow <- floor(windowSize/2)
  frames <- seq((halfWindow), (length(data) - halfWindow), jump)
  #print(length(frames))
  if(length(frames) < nIterations)
    nIterations <- length(frames)

  # preparing the output
  output <- rep(NA, length(data)) 
  lambdas <- vector(mode="numeric", length=nIterations)
  sigmas <- vector(mode="numeric", length=nIterations)
  # initializing variables
  d <- 1
  n1 <- windowSize
  n2 <- windowSize
  n <- n1 + n2
  b <- min(300,n) # Number of kernel bases
  j <- 1

  # first iterations until convergence of lambda and sigma
  for(i in 2:nIterations) {
    X1 <- matrix(data[(frames[i]-halfWindow+1):(frames[i]+halfWindow)], nrow=1, ncol=windowSize)
    X2 <- matrix(data[(frames[i-1]-halfWindow+1):(frames[i-1]+halfWindow)], nrow=1, ncol=windowSize)
    X <- cbind(X1,X2)
    rm(X1, X2)
    center_index_tmp <- sample(n)
    center_index <- center_index_tmp[1:b]
    C <- matrix(X[,center_index], nrow=1, ncol=b)
    XXsum <- matrix(colSums(X^2), nrow=1, ncol=n)
    XC_dist2 <- do.call("rbind", rep(list(XXsum), b)) + do.call("cbind", rep(list(as.matrix(XXsum[,center_index])), n)) - 2*t(C)%*%X
    rm(XXsum, X, C)
    X1C_dist2 <- XC_dist2[,1:n1]
    X2C_dist2 <- XC_dist2[,(n1+1):n]
    CC_dist2 <- XC_dist2[,center_index]
    rm(XC_dist2)
    fold <- 5
    cv_fold <- c(1:fold)
    cv_split1 <- floor(c(0:(n1-1))*fold/n1)+1
    cv_split2 <- floor(c(0:(n2-1))*fold/n2)+1
    cv_index1 <- cv_split1[sample(n1)]
    cv_index2 <- cv_split2[sample(n2)]
    n1_cv <- vector(mode="numeric", length=fold)
    n2_cv <- vector(mode="numeric", length=fold)
    for(k in cv_fold) {
      n1_cv[k] <- sum(cv_index1==k)
      n2_cv[k] <- sum(cv_index2==k)
    }
    sigma_list <- c(0.25, 0.5, 0.75, 1, 1.2, 1.5, 2, 3, 5) # Candidates of Gaussian width
    lambda_list <- 10^seq(-3,1,len=9) # Candidates of regularization parameter
    score_cv <- array(data=0, dim=c(length(sigma_list),length(lambda_list),fold))  
    nSigma <- length(sigma_list)
    for(s in 1:nSigma) {
      sigma <- sigma_list[s]
      H <- (sqrt(pi)*sigma)^d*exp(-CC_dist2/(4*sigma^2))
      h1_cv <- matrix(nrow=nrow(X1C_dist2), ncol=fold)
      h2_cv <- matrix(nrow=nrow(X2C_dist2), ncol=fold)
      for(k in cv_fold) {
        h1_cv[,k] <- rowSums(exp(-X1C_dist2[,cv_index1==k]/(2*sigma^2)))
        h2_cv[,k] <- rowSums(exp(-X2C_dist2[,cv_index2==k]/(2*sigma^2)))
      }
      for(k in cv_fold) {
        htrain <- rowSums(h1_cv[,cv_fold!=k],2)/sum(n1_cv[cv_fold!=k]) - rowSums(h2_cv[,cv_fold!=k],2)/sum(n2_cv[cv_fold!=k])
        htest <- rowSums(as.matrix(h1_cv[,cv_fold==k]),2)/sum(n1_cv[cv_fold==k]) - rowSums(as.matrix(h2_cv[,cv_fold==k]),2)/sum(n2_cv[cv_fold==k])
        nLambda <- length(lambda_list)
        for(l in 1:nLambda) {
          lambda <- lambda_list[l]
          #thetah <- pcg(H+lambda*diag(b),htrain)
          thetah <- solve(H+lambda*diag(b),htrain)
          score_cv[s, l, k] <- t(thetah)%*%H%*%thetah - 2*t(thetah)%*%as.matrix(htest)
        }
      }
    }
    score_cvMean <- apply(score_cv, c(1,2), mean)
    score_cvMin <- which(score_cvMean == min(score_cvMean), arr.ind = TRUE)
    lambda_index <- score_cvMin[1,2]
    sigma_index <- score_cvMin[1,1]
    lambda=lambda_list[lambda_index]
    sigma=sigma_list[sigma_index]
    H <- (sqrt(pi)*sigma)^d*exp(-CC_dist2/(4*sigma^2))
    h <- rowMeans(exp(-X1C_dist2/(2*sigma^2))) - rowMeans(exp(-X2C_dist2/(2*sigma^2)))
    #thetah <- pcg(H+lambda*diag(b),h)
    thetah <- solve(H+lambda*diag(b),h)
    L2dist <- 2*t(thetah)%*%h - t(thetah)%*%H%*%thetah
    output[frames[i]] <- L2dist
    lambdas[j] <- lambda
    sigmas[j] <- sigma
    j <- j + 1
  }
  
  # choose the lambda and sigma. Update the frames to calculate
  lambda <- median(lambdas)
  sigma <- median(sigmas)
  print(paste("lambda:", lambda, "|| sigma:", sigma))

  # update result
  if(length(frames) > nIterations) {
    frames <- frames[nIterations:length(frames)]
    N <- length(frames)
    for(i in 2:N) {
      X1 <- matrix(data[(frames[i]-halfWindow+1):(frames[i]+halfWindow)], nrow=1, ncol=windowSize)
      X2 <- matrix(data[(frames[i-1]-halfWindow+1):(frames[i-1]+halfWindow)], nrow=1, ncol=windowSize)
      X <- cbind(X1,X2)
      rm(X1, X2)
      center_index_tmp <- sample(n)
      center_index <- center_index_tmp[1:b]
      C <- matrix(X[,center_index], nrow=1, ncol=b)
      XXsum <- matrix(colSums(X^2), nrow=1, ncol=n)
      XC_dist2 <- do.call("rbind", rep(list(XXsum), b)) + do.call("cbind", rep(list(as.matrix(XXsum[,center_index])), n)) - 2*t(C)%*%X
      rm(XXsum, X, C)
      X1C_dist2 <- XC_dist2[,1:n1]
      X2C_dist2 <- XC_dist2[,(n1+1):n]
      CC_dist2 <- XC_dist2[,center_index]
      H <- (sqrt(pi)*sigma)^d*exp(-CC_dist2/(4*sigma^2))
      h <- rowMeans(exp(-X1C_dist2/(2*sigma^2))) - rowMeans(exp(-X2C_dist2/(2*sigma^2)))
      #thetah <- pcg(H+lambda*diag(b),h)
      thetah <- solve(H+lambda*diag(b),h)
      L2dist <- 2*t(thetah)%*%h - t(thetah)%*%H%*%thetah
      output[frames[i]] <- L2dist
    }
  }

  # prepare output
  output <- approx(output, xout=seq_along(output))$y
  output[is.na(output)] <- 0
  
  # return the LSDD score and the segments for external ploting
  if(segment == TRUE) {
    # find local maximums above a threshold. choice :CDF(LSDD) = 0.75
    threshold <- quantile(output, probs=0.75)
    aMaxIndex <- which(diff(sign(diff(output)))==-2)+1
    aMaxIndex <- aMaxIndex[which(output[aMaxIndex] >= threshold)]
    # define the beginning of each segment
    aSegStart <- c(1, aMaxIndex+1)
    
    #for plotting outside
    output <- list(LSDD=output, Segments=c(aSegStart,N))
    return(output)
  }
  
  #return the LSDD score and parameters
  if(parameters == TRUE) {
    output <- list(LSDD=output, sigma=c(sigma), lambda=c(lambda))
    return(output)
  }
  
  # for plotting without segmentation
  if(makePlot == TRUE) {
    N <- length(data)
    par(mfrow=c(2,1), mar = rep(2, 4))
    plot(x=1:N,y=data, type="l",xlab="time series index",ylab="", main="LSDD for Original and Normalized Univariate Time Series.")
    abline(v = c(aSegStart,N), col="blue")
    plot(x=1:N,y=output, type="l",xlab="time series index",ylab="", main=paste("LSDD score. Window size:",windowSize),ylim=c(0, max(output,na.rm=TRUE)))
  }
  else {
    return(output)
  }
}


# Least-Squares Density-Difference Estimation

# Estimating p1(x)-p2(x) from samples {x1_i}_{i=1}^{n1} and {x2_j}_{j=1}^{n2}
# drawn i.i.d. from p1(x) and p2(x), respectively.

# Input:
#    X1: d by n1 training sample matrix
#    X2: d by n2 training sample matrix

# Output:
#    L2dist: estimate of L2-distance obtained from X1 and X2
#    ddh:    estimates of p1(x)-p2(x) at T (OPTIONAL)
LSDDfast <- function(X1, X2, sigma, lambda) {
  
  d <- dim(X1)[1]
  n1 <- dim(X1)[2]
  dummy <- dim(X2)[1]
  n2 <- dim(X2)[2]
  X <- cbind(X1,X2)
  rm(X1, X2)
  n <- n1 + n2
  b <- min(300,n) # Number of kernel bases
  
  center_index_tmp <- sample(n)
  center_index <- center_index_tmp[1:b]
  C <- matrix(X[,center_index], nrow=1, ncol=b)
  XXsum <- matrix(colSums(X^2), nrow=1, ncol=n)
  XC_dist2 <- do.call("rbind", rep(list(XXsum), b)) + do.call("cbind", rep(list(as.matrix(XXsum[,center_index])), n)) - 2*t(C)%*%X
  rm(XXsum, X, C)
  
  X1C_dist2 <- XC_dist2[,1:n1]
  X2C_dist2 <- XC_dist2[,(n1+1):n]
  CC_dist2 <- XC_dist2[,center_index]
  rm(XC_dist2)
  
  H <- (sqrt(pi)*sigma)^d*exp(-CC_dist2/(4*sigma^2))
  h <- rowMeans(exp(-X1C_dist2/(2*sigma^2))) - rowMeans(exp(-X2C_dist2/(2*sigma^2)))
  #thetah <- solve(H+lambda*diag(b),h)
  thetah <- pcg(H+lambda*diag(b),h)
  L2dist <- 2*t(thetah)%*%h - t(thetah)%*%H%*%thetah
  
  return(L2dist)
}