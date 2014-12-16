locationTable <- table(log$LOCATION)

spatFilteredLog <- log[0,]
spatFilteredAggregation <- data.frame(AGGREGATE=numeric(0))

windowLength=36000


locationLevels<- levels(log$LOCATION)

for(i in 1:12)
#  i=locationLevels[1]
{
  #tmpLog<- log[log$LOCATION==i,]
  tmpLog<- log[log$LOCATION==locationLevels[i],]
  while(dim(tmpLog)[1]>0)
  {
    windowStart<-tmpLog$EVENT_TIME[1] 
    windowEnd<- windowStart+windowLength
    tmpAggrLog <- tmpLog[tmpLog$EVENT_TIME>=windowStart & tmpLog$EVENT_TIME<windowEnd,]
    tmpLog<-tmpLog[-1:-dim(tmpAggrLog)[1],]
    spatFilteredLog<-rbind(spatFilteredLog,tmpAggrLog[1,])
    spatFilteredAggregation <- rbind(spatFilteredAggregation,dim(tmpAggrLog)[1])
  } 
  
}