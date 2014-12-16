log<-logError
#log <- log[log$SEVERITY=="ERROR"|log$SEVERITY=="FATAL",]

#the following attempts to fix an observed gap in EVENT_TIME values: 6 rows are listed as NAs. Check if problem replicates!
#remove otherwise
#problem identified: time corresponds to time change dead hour!
log$EVENT_TIME[14122]<- strptime("2009-03-29-01.57.18.400772", "%Y-%m-%d-%H.%M.%OS")
log$EVENT_TIME[14123]<- strptime("2009-03-29-01.57.18.450630", "%Y-%m-%d-%H.%M.%OS")
log$EVENT_TIME[14124]<- strptime("2009-03-29-01.57.18.593781", "%Y-%m-%d-%H.%M.%OS")
log$EVENT_TIME[14125]<- strptime("2009-03-29-01.57.18.653331", "%Y-%m-%d-%H.%M.%OS")
log$EVENT_TIME[14126]<- strptime("2009-03-29-01.57.18.802415", "%Y-%m-%d-%H.%M.%OS")
log$EVENT_TIME[14127]<- strptime("2009-03-29-01.57.18.864890", "%Y-%m-%d-%H.%M.%OS")




tFilteredIndices <- numeric(dim(log)[1])
tFilteredAggregation <- numeric(dim(log)[1])
#tFilteredAggregation <- data.frame(AGGREGATE=numeric(dim(log)[1]))
windowLength=36000

tFilter <- function(log, indices, windowLength=300)    
{
  j<- 1
  lon<-length(indices)
  tFilteredIndices <- numeric(lon)
  tFilteredAggregation <- numeric(lon)
  while(j <= lon)
  {
    windowStart<-log$EVENT_TIME[indices[j]] 
    windowEnd<- windowStart+windowLength
    timeTmp<-log$EVENT_TIME[indices]
    aggregation <-sum(timeTmp>=windowStart & timeTmp<windowEnd)
    if(is.na(aggregation)){aggregation<-1}
    #tFilteredLog<-rbind(tFilteredLog,tmpLog[j,])
    tFilteredIndices[j]<-indices[j]
    tFilteredAggregation[j] <- aggregation
    j<-j+ aggregation
  }
  #Remove zeros
  tFilteredIndices<-tFilteredIndices[which(tFilteredIndices!=0)]
  tFilteredAggregation<-tFilteredAggregation[which(tFilteredAggregation!=0)]
  return(list(tFilteredIndices,tFilteredAggregation))
} 

k<-1

for(l in levels(log$LOCATION))
{
  indices<- which(log$LOCATION==l)
  x<-tFilter(log,indices)
  x1<-x[[1]]
  x2<- x[[2]]
  tFilteredIndices[k:(k+length(x1)-1)]<-x1
  tFilteredAggregation[k:(k+length(x2)-1)]<-x2
  k<-k+length(x1)
}

tFilteredIndices<-tFilteredIndices[tFilteredIndices>0]
tFilteredAggregation<-tFilteredAggregation[tFilteredAggregation>0]

logTFiltered<-log[tFilteredIndices,]
logTFiltered<-logTFiltered[order(logTFiltered$RECID),]

 
#tFilter(tmpLog)

