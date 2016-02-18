source('LSDD.R')
source('LSDDsegmentation.R')


d <- data.frame( read.csv('./test100.csv'))[,-1]
nrow(d)
ncol(d)
summary(d)
LSDDsegment(d, windowSize=10, overlap=1, thres=0.5, LSDDparameters=FALSE, univariate=TRUE)


