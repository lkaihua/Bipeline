data <- as.matrix(read.csv("snowboard.csv"))[,c(2,3,5,6,7,8,11,12,13,14,16,17,18,19,20,21,22,23,24,25,26)]
data <- normalizeTS(data)
segments <- LSDDsegment(data, LSDDparameters=TRUE, windowSize=90, overlap=0.33333333333333, thres=0.75, univariate=TRUE)
PDFbiclustering(data, segments=segments, delta=4, k=100)