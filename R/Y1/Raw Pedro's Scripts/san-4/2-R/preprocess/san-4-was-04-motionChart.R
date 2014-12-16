library(googleVis)
library(reshape)

dataPath <-"C:\\Users\\capelastegui\\workspace\\OFP\\Santander-4\\1-Data\\2-test\\"
fileOut <- paste(dataPath, "san-4-was-motionchart.csv", sep="")

mLog<-log
windowLength <- 3600 #1 hour
firstDate<-mLog$date[1]
firstDate$hour<-0
firstDate$min<-0
firstDate$sec<-0

dateDelta<-difftime(tail(mLog$date,1),firstDate,units="secs")
len<-as.numeric(ceiling(dateDelta/3600))
exLevels<-levels(mLog$Exception)
dupIndices<-which(duplicated(gsub(".*\\.","",exLevels)))



exceptionIndices<-list()
exceptionsInWindow <- list()
eventsInWindow <- numeric(len)

breakStr<-"hour"
#breakStr<-"min"

#efficient implementation for cutting data frame by date
for(i in 1:length(exLevels))
{
  exceptionIndices[[i]]<-which(mLog$Exception==exLevels[i])
  exceptionsInWindow[[i]]<-data.frame(table(cut(mLog$date[exceptionIndices[[i]]],breaks=breakStr)))
  names(exceptionsInWindow[[i]])<-c("date",exLevels[i])
}

names(exceptionIndices)<-exLevels
names(exceptionsInWindow)<-exLevels

eventsInWindow<-data.frame(table(cut(mLog$date,breaks=breakStr)))
names(eventsInWindow)<-c("date","events")

l<-merge_recurse(exceptionsInWindow)
l<-merge(l,eventsInWindow,by="date")
l$date<-as.POSIXlt(l$date)
l<-l[order(l$date),]
l[is.na(l)]<-0
l$dateNum<-as.numeric(difftime(l$date , l$date[1],units="hours")+100)
lm<-melt(l,id=c("date","dateNum","events"))
lm<-lm[order(lm$date),]
names(lm)[names(lm)=="variable"]<-"excType"
levels(lm$excType)<-gsub("Exception", "Exc",levels(lm$excType))
levels(lm$excType)[levels(lm$excType)=="Var.2"]<-"_"



lm2<-(lm[lm$variable %in% exLevels[1:4],])

plot(gvisMotionChart(lm[,-1], idvar="variable", timevar="dateNum",options=list(height=900,width=1600)))
plot(gvisMotionChart(lm[,-1], idvar="variable", timevar="dateNum"))

gsub("Exception", "Exc",levels(lm$excType))


boxplot(l$events ~ l$date$hour)
boxplot(l$events ~ l$date$wday)




write.csv(fileOut)


# for(i in 1:len)
# {
#   end<-firstDate+windowLength*i
#   start<-end-windowLength
#   for(j in 1:length(exLevels))
#   {
#     exceptionsInWindow[[j]][i]<-sum (mLog$date[exceptionIndices[[j]]]>= start &
#                                       mLog$date[exceptionIndices[[j]]]< end)
#   }
#   eventsInWindow[i]<-sum (mLog$date>= start & mLog$date< end)
# }


#calculate events in last observed window for each sample

# for(i in 1:dim(log)[1])
# {
#   failuresInWindow[i]<-sum(failLog$EVENT_TIME>log$EVENT_TIME[i]-windowLength & failLog$EVENT_TIME<log$EVENT_TIME[i])
# }
# 
# hasFailuresInWindow <- failuresInWindow>0
# 
# for(i in 1:dim(log)[1])
# {
#   eventsInWindow[i]<-sum(log$EVENT_TIME>log$EVENT_TIME[i]-windowLength & log$EVENT_TIME<log$EVENT_TIME[i])
# }

#hasEventsInWindow <- eventsInWindow>0



