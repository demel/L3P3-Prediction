#log<-logError
#log<-logFailure
log<-logTFiltered

failLog<-log[log$SEVERITY=="FATAL",]
length<-dim(log)[1]
trainLength<- length%/%3
testRange <- (trainLength+1):(length%/%2)
validationRange <-(length%/%2+1):length 


failureIndices<-which(log$SEVERITY=="FATAL")
nextFailureIndices <- numeric(length)
eventsInPastWindow <- numeric(length)
failuresInWindow <- numeric(length)
windowLength <- 3600 #1 hour

log<-log[order(log$EVENT_TIME, log$RECID),]

#calculate time to next failure for each sample
for(i in 1:dim(log)[1])
{
  nextFailureIndices[i]<-failureIndices[which(failureIndices>i)][1]
}
timeToFailure <-as.numeric(log$EVENT_TIME[nextFailureIndices]- log$EVENT_TIME, units="mins") 

#calculate events in last observed window for each sample
for(i in 1:dim(log)[1])
{
  failuresInWindow[i]<-sum(failLog$EVENT_TIME>log$EVENT_TIME[i]-windowLength & failLog$EVENT_TIME<log$EVENT_TIME[i])
}

hasFailuresInWindow <- failuresInWindow>0

tLog<-log[1:trainLength,]
tLog[c(2:6,8:11)] <- (lapply(tLog[c(2:6,8:11)],factor))
lastAggregation<-c(0,tLog$AGGREGATION[1:trainLength-1])

#time to failure and failures in training set
TTF.train<-timeToFailure[1:trainLength]
tFiW<-failuresInWindow[1:trainLength]

tHFiW<-hasFailuresInWindow[1:trainLength]


#mean time to failure for training set, in minutes
meanTTF.train <- as.numeric((tLog$EVENT_TIME[dim(tLog)[1]]-tLog$EVENT_TIME[1])/length(tLog$SEVERITY[tLog$SEVERITY=="FATAL"]), "mins")
#mean(TTF.train)#[1] 1528.553
#linear regression: prediction of time to failure
MSG_ID<-tLog$MSG_ID
levels(MSG_ID)<- c(levels(MSG_ID), "UNKNOWN")

msgIDnames <- names(table(log$MSG_ID))
msgIDtable<-data.frame(matrix(0,ncol=length(msgIDnames), nrow=length))
names(msgIDtable)<-msgIDnames


logTime<-log$EVENT_TIME
logTimeMinusWindow<-log$EVENT_TIME-windowLength

eventsInWindow <- function(logTime,tmpTimeVector,windowLength)
{
  x<-sum(tmpTimeVector>logTime-windowLength & tmpTimeVector<logTime)   
}

for (i in 1: length(msgIDnames))
{
  #tmpTimeVector<-msgIDtable[,i]
  tmpTimeVector<-log$EVENT_TIME[log$MSG_ID==msgIDnames[i]]
  tmpResult<-sapply(logTime,eventsInWindow,tmpTimeVector,windowLength)
  msgIDtable[,i]<-tmpResult
}




sqrt(sum((lm1$fitted-TTF.train)^2,na.rm=TRUE))
#sqrt(sum((lm(TTF.train ~ tLog$MSG_ID+tHFiW)$fitted-TTF.train)^2,na.rm=TRUE))

#Let's predict!
TTF.test<-timeToFailure[testRange]
testData<-data.frame(log$MSG_ID[testRange], hasFailuresInWindow[testRange])
midt.test <- msgIDtable[testRange,]
testData.b<-data.frame(midt.test, hasFailuresInWindow[testRange])


names(testData)<-c("MSG_ID","tHFiW")
names(testData.b)<-c(msgIDnames,"tHFiW")
#levels(testData$MSG_ID) [which(!levels(testData$MSG_ID) %in% levels(MSG_ID))] <-"UNKNOWN"
testData$MSG_ID [which(!(testData$MSG_ID %in% levels(MSG_ID)))] <-NA

pred1<-predict(lm1,testData)
summary(pred1)
sqrt(sum((pred1-TTF.test)^2,na.rm=TRUE))
plot( timeToFailure[testRange] ~ log$EVENT_TIME[testRange],type="l")
points(pred1 ~ log$EVENT_TIME[testRange], col="blue", pch=2, cex=0.5)
plot(pred1 ~ log$EVENT_TIME[testRange], col="blue")


#And now for some logistic regression
failureIn5min<- timeToFailure<300
failureIn30min<- timeToFailure<1800
failureIn2hours<-timeToFailure<7200

Fi5m.train <- failureIn5min[1:trainLength]
Fi30m.train <- failureIn30min[1:trainLength]
Fi2h.train <- failureIn2hours[1:trainLength]

midt.train <- msgIDtable[1:trainLength,]


aic.list <- numeric(length(msgIDnames))
for(i in 1: length(msgIDnames))
{
  aic.list[i]<-summary(glm(Fi5m.train ~ midt.train[,i], family="binomial"))$aic
}

summary(glm(Fi5m.train~ ., family="binomial", data=midt.train))$aic

binm1<-glm(Fi5m.train ~ MSG_ID + tHFiW, family="binomial")
binm1b<-glm(Fi5m.train ~ ., family="binomial",data=midt.train)
#plot fitting of training set
plot(Fi5m.train[5000:18000] ~ tLog$EVENT_TIME[5000:18000],pch=5,cex=0.5)
points(binm1$fitted[5000:18000] ~ tLog$EVENT_TIME[5000:18000], col="red", pch=6, cex=0.5)

binm2<-glm(Fi30m.train ~ MSG_ID + tHFiW, family="binomial")
#plot fitting of training set
plot(Fi30m.train[5000:18000] ~ tLog$EVENT_TIME[5000:18000],pch=5,cex=0.5)
points(binm2$fitted[5000:18000] ~ tLog$EVENT_TIME[5000:18000], col="red", pch=6, cex=0.5)

binm3<-glm(Fi2h.train ~ MSG_ID + tHFiW, family="binomial")
#plot fitting of training set
plot(Fi2h.train[5000:18000] ~ tLog$EVENT_TIME[5000:18000],pch=5,cex=0.5)
points(binm3$fitted[5000:18000] ~ tLog$EVENT_TIME[5000:18000], col="red", pch=6, cex=0.5)




#Let's predict!

#Fi5m.test <- failureIn5min[testRange]


predB1<-predict(binm1,testData, type="response")
summary(predB1)
summary(Fi5m.test)
plot( Fi5m.test ~ log$EVENT_TIME[testRange],pch=5,cex=0.5)
points(predB1 ~ log$EVENT_TIME[testRange], col="red", pch=6, cex=0.5)

predB1b<-predict(binm1b,midt.test, type="response")
points(predB1b ~ log$EVENT_TIME[testRange], col="green", pch=7, cex=0.5)

Fi30m.test <- failureIn30min[testRange]
predB2<-predict(binm2,testData, type="response")
summary(predB2)
summary(Fi30m.test)
plot( Fi30m.test ~ log$EVENT_TIME[testRange],pch=5,cex=0.5)
points(predB2 ~ log$EVENT_TIME[testRange], col="red", pch=6, cex=0.5)

Fi2h.test <- failureIn2hours[testRange]
predB3<-predict(binm3,testData, type="response")
summary(predB3)
summary(Fi2h.test)
plot( Fi2h.test ~ log$EVENT_TIME[testRange],pch=5,cex=0.5)
points(predB3 ~ log$EVENT_TIME[testRange], col="red", pch=6, cex=0.5)


predB1.roc <- prediction(predB1,Fi5m.test)
predB1b.roc <- prediction(predB1b,Fi5m.test)
perf <- performance(predB1.roc,"tpr","fpr")
plot(perf)
perfb <- performance(predB1b.roc,"tpr","fpr")
plot(perfb,col="red", add=TRUE)
perf1 <- performance(predB1.roc, "prec", "rec")
plot(perf1)
perf1b <- performance(predB1b.roc, "prec", "rec")
plot(perf1b,col="red", add=TRUE)

predB2.roc <- prediction(predB2,Fi30m.test)
perf <- performance(predB2.roc,"tpr","fpr")
plot(perf)
perf1 <- performance(predB2.roc, "prec", "rec")
plot(perf1)

predB3.roc <- prediction(predB3,Fi2h.test)
perf <- performance(predB3.roc,"tpr","fpr")
plot(perf)
perf1 <- performance(predB3.roc, "prec", "rec")
plot(perf1)



#at the end remove last line
