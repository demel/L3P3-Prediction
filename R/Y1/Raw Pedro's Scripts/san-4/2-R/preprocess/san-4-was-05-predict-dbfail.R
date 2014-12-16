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

names(l)<-gsub("Exception", "Ex",names(l))
names(l)[names(l)=="Var.2"]<-"noExc"

dbFail<-db[db$LEVEL=="Severe"| db$LEVEL=="Error (OS)",]
dbFail$FUNCTION<-factor(dbFail$FUNCTION)
#remove first level of db$FUNCTION (irrelevant error)
dbSubset<-dbFail[dbFail$FUNCTION!=(levels(dbFail$FUNCTION)[1]),]
dbSubset2<-dbSubset[dbSubset$INSTANCE!="iesmpar2",]
db2<-dbSubset2[,c("DATE","FUNCTION", "MESSAGE")]
names(db2)<- c("date","fun","msg")

db2[,-1]<-lapply(db2[,-1],factor)
levels(db2$fun)<-c("sqlpgOpenLFH", "sqloopenp")
levels(db2$msg)<-c("can't open control file","file sharing violation")
db2$fail<-factor(paste(db2$fun,db2$msg, sep=": "))
db2<-db2[,c(1,4)]
row.names(db2)<-NULL

failTime<- as.POSIXlt(db2$date[1])
l$date

dbFail1<-(failTime > l$date) & (failTime < l$date+3600)
dbFail2<-(failTime > l$date+3600) & (failTime < l$date+7200)
l2<-l[,2:48]
l2$hour<-l$date$hour

a1<-aov(dbFail1 ~ BSComponentEx, l2)
lm1<-lm(dbFail1 ~ BSComponentEx, l2)

plot((l2$BSComponentEx)/50,col="red")
points(dbFail1*1, col="blue",cex=1, pch=10)
lines(lm1$fitted)

plot((l2$BSComponentEx[410:430])/50,col="red")
points(dbFail1[410:430]*1, col="blue",cex=1, pch=10)
lines(lm1$fitted[410:430])

f1.base<- "dbFail1 ~ "

pForm1 <- function(a) 
{
  as.formula(paste(f1.base,a))
}

namesL2<-names(l2)

f1<-sapply(namesL2,pForm1)
lmList1<-lapply(f1,lm,data=l2)
lmSumList1<-lapply(lmList1,summary)

getRsquared<-function(lm)
{
  lm$adj.r.squared
}

rSqList1<-sapply(lmSumList1,getRsquared)

summary(lm(dbFail1 ~ StateNotFoundEx, l2))$adj.r.squared
summary(lm(dbFail2 ~ StateNotFoundEx, l2))$adj.r.squared
lm2<-lm(dbFail1 ~ StateNotFoundEx, l2)

plot((l2$StateNotFoundEx)/2,col="red")
points(dbFail1*1, col="blue",cex=1, pch=10)
lines(lm2$fitted)

lm3<-lm(dbFail1 ~  BSComponentEx+ StateNotFoundEx, l2)

summary(lm(dbFail1 ~ StateNotFoundEx, l2))$adj.r.squared
summary(lm(dbFail2 ~ StateNotFoundEx, l2))$adj.r.squared
lm2<-lm(dbFail1 ~ StateNotFoundEx, l2)

plot((l2$StateNotFoundEx)/2,col="red")
points(dbFail1*1, col="blue",cex=1, pch=10)
lines(lm2$fitted)

lm2<-lm(dbFail1 ~  BSComponentEx+ StateNotFoundEx, l2)
