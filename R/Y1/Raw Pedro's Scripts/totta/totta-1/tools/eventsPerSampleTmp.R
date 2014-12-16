library(plyr)
subTable<-logTable[grep("bvtowpocwp",names(logTable))] # hand-picked, nodes with matching names to failure
subTableCpu<-llply(subTable,function(x){x$CPU[,2:3]})
subTableCpu<-subTableCpu[!laply(subTableCpu,is.null)]
names(subTableCpu)<-gsub(".*/","",names(subTableCpu))
for(n in names(subTableCpu))
{
  names(subTableCpu[[n]])[2]<-n
}

cpuFailTable<-Reduce(function(d1,d2){join(d1,d2,by="date")},subTableCpu)
##cpuMeans<-colMeans(cpuFailTable[,2:5],na.rm=TRUE)
##cpuFailTable

par(mfrow=c(1,1))

plot(cpuFailTable[,c(1,2)],type="l")

for(i in 3:5)
{
  lines(cpuFailTable[,c(1,i)],col=i)
}

#lines(cpuFailTable$date,rowMeans(cpuFailTable[,2:5],na.rm=TRUE),col=i+1)
time1<-cpuFailTable$date[1]+3600*3+3600*24
time2<-cpuFailTable$date[1]+3600*6+3600*24
indices<-which(cpuFailTable$date>time1 & cpuFailTable$date<time2)
cpuTable2<-cpuFailTable[indices,]

par(mfrow=c(1,1))

plot(cpuTable2[,c(1,4)],type="l",col=5)
lines(cpuTable2[,c(1,5)],col=4)
lines(cpuTable2[,c(1,3)],col=3)
lines(cpuTable2[,c(1,2)],col=2)

for(i in 2:4)
{
  lines(cpuFailTable[,c(1,i)],col=i)
}

#llply(subTableCpu,head)

failures<-eventTable[which(eventTable$Severity=="Major"),][,c(2,4)]
failures$Name<-factor(failures$Name)
names(logTable)
#regex to remove file path elements
cutNames<-gsub(".*/","",names(logTable))

cutNames %in% failures$Name

dates

failIndices<-list()
failsInWindow <- list()
#eventsInWindow <- numeric(len)
nameLevels<-levels(failures$Name)

#efficient implementation for cutting data frame by date
for(i in 1:length(nameLevels))
{
  failIndices[[i]]<-which(failures$Name==nameLevels[i])
  failsInWindow[[i]]<-data.frame(table(cut.POSIXt(failures$date[failIndices[[i]]],breaks="5 min")))
  names(failsInWindow[[i]])<-c("date",nameLevels[i])
}

names(failIndices)<-nameLevels
names(failsInWindow)<-nameLevels

eventsInWindow<-data.frame(table(cut(mLog$date,breaks="hour")))
names(eventsInWindow)<-c("date","events")
