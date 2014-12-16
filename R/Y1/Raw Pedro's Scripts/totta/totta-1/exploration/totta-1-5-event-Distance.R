library(plyr)
library(dplyr)
library(reshape2)
library (ggplot2)
library(lubridate)
library(gridExtra)


#specific date to deal with specific totta dataset
firstDate<-update(floor_date(now(),"day"), year=2013, month=12, day=10)





evTableED<-eventTable %.% 
  filter(severity!="BLANK") %.% 
  select (date,type, node) %.%
  filter(date>firstDate) %.%
  refactor() %.% 
  tbl_df()

#generate window of time


evTableED<-applyGetTypeDistances(evTableED, maxDist=3600,direction="backward", method="anyEvent")



targetEvents<-applyGetTypeDistances(evTableED, maxDist=3600, direction="forward", method="anyEvent", append=FALSE)
# for each column in target, append that column to evTableED with name Y, 
predictionTables<-llply(targetEvents,function(col,table){tmpTable<-table; tmpTable$Y=col; tmpTable},evTableED %.% select(-date,-node))
glmList<-llply(predictionTables,function(t){glm(Y~.,data=t,family=binomial)})

getModels<-function(evTableED, dataWin, predWin)
{
  .data<-applyGetTypeDistances(evTableED, maxDist=dataWin,direction="backward", method="anyEvent")
  targetEvents<-applyGetTypeDistances(.data, maxDist=predWin, direction="forward", method="anyEvent", append=FALSE)
  # for each column in target, append that column to evTableED with name Y, 
  predictionTables<-llply(targetEvents,function(col,table){tmpTable<-table; tmpTable$Y=col; tmpTable},.data %.% select(-date,-node))
  glmList<-llply(predictionTables,function(t){glm(Y~.,data=t,family=binomial)})
  resultList<-list(tables=predictionTables,glms<-glmList)
  resultList
}




.data<-predictionTables[[3]]


fit1<-glm(Y~.,data=.data,family=binomial)
summary(fit1)
fit2<-glm(Y~.^2,data=.data,family=binomial)
summary(fit2)
# for each event, get distance to closest event of each type
#single line

anova(fit1)
exp(fit1$coeff)
exp(confint(fit1))







evTableEd[1,]
evTableED %.% mutate(dist1=as.duration(date[1]%--%date))
evTableED %.% mutate(dist1=as.duration(date[1]%--%date)) %.% group_by(type) %.% summarise(min(dist1))
evTableED %.% mutate(dist1=as.duration(date[1]%--%date)) %.% group_by(type) %.% filter(date>date[1])
evTableED %.% mutate(dist1=as.duration(date[1]%--%date)) %.% group_by(type) %.% filter(date>date[1]) %.% summarise(minDist1=min(dist1))
evTableED %.% mutate(dist1=as.duration(date[1]%--%date)) %.% group_by(type) %.% filter(date!=date[1]) %.% summarise(minDist1=min(dist1))
evTableED %.% mutate(dist1=as.duration(date[10]%--%date)) %.% group_by(type) %.% filter(date!=date[10]) %.% summarise(minDist1=min(dist1))

tmpResult<-evTableED %.% mutate(dist1=as.duration(date[10]%--%date)) %.% group_by(type) %.% filter(date!=date[10]) %.% summarise(minDist1=min(dist1))
tmpResult2<-tmpResult[,2]
names(tmpResult2)<-tmpResult[,1]

tmpResult<-evTableED %.% mutate(dist1=as.duration(date[10]%--%date)) %.% group_by(type)  %.% summarise(minDist1=min(dist1))
tmpResult2<-tmpResult[,2]
names(tmpResult2)<-tmpResult[,1]


getTypeDistances<-function(date1,.data,maxDist=24*3600,forwardAndBackward=TRUE)
{
tmpResult<-.data %.% mutate(dist1=as.duration(date1%--%date)) %.% group_by(type) 

if(forwardAndBackward) 
  {tmpResult<-tmpResult %.% summarise(minDist1=min(abs(dist1),maxDist))} else 
  {#otherwise, only look forward
    tmpResult<-tmpResult %.% filter(dist1>0) 
    if(nrow(tmpResult)>0)
      {tmpResult<-tmpResult%.% summarise(minDist1=min(dist1,maxDist))} else # empty vector, crashes with that summary
      {tmpResult<-tmpResult%.% summarise(minDist1=n())}  
  }

tmpResult2<-as.data.frame(matrix(0,ncol=length(levels(.data$type)),nrow=0))
colnames(tmpResult2)<-levels(.data$type)
tmpResult.T<-as.data.frame(t(tmpResult[,-1]))
colnames(tmpResult.T)<-tmpResult[,1]
rbind.fill(tmpResult2,tmpResult.T)
}


distTable<-ldply(evTableED$date,getTypeDistances,evTableED, forwardAndBackward=FALSE)
distTable$date=evTableED$date
distTable$type=evTableED$type
distTableMolten<-melt(distTable,id=c("date","type"))
p1<-ggplot(distTableMolten)+aes(date,value,color=variable)+geom_line()+geom_point(aes(alpha=0.5))
p2<-ggplot(distTableMolten)+aes(date,fill=type)+geom_bar(stat="bin",colour="darkblue",binwidth=3600*6
grid.arrange(p1,p2,ncol=1)

p1<-ggplot(distTableMolten)+aes(date,value,color=variable)+geom_line()+geom_point(aes(alpha=0.5))+facet_grid(variable~.)
grid.arrange(p1,p2,ncol=1)

distTable<-ldply(evTableED$date,getTypeDistances,evTableED,30*24*3600)
distTable$date=evTableED$date
ggplot(melt(distTable,id="date"))+aes(date,value,color=variable)+geom_point()+scale_y_continuous(limits=c(0,30*24*3600))
ggplot(melt(distTable,id="date"))+aes(date,value,color=variable)+geom_point()+scale_y_continuous(limits=c(0,24*3600))
ggplot(melt(distTable,id="date"))+aes(date,value,color=variable,alpha=0.9)+geom_line()+scale_y_continuous(limits=c(0,24*3600))

evTableED %.% mutate(dist1=as.duration(lag(date)%--%date))

evTableED %.% mutate(nextEv=as.duration(date %--% lead(date)))




evTableED %.% group_by(type)%.%summarise(dist1=as.duration(date[1]%--%first(date)))

evTableED %.% group_by(type)%.%summarise(dist1=date[1])

evTableED %.% group_by(type)%.%summarise(dist1=first(date))
evTableED %.% muta(dist1=first(date))


laply(head(evTableED$date),function(d,w){sum(d %within% w)},head(evTableED$dateWin))#kinda works
#next one fails

llply(head(evTableED$dateWin),function(w,d){(d %within% w)},head(evTableED$date))
llply(head(evTableED$dateWin),function(w,d){(w )},head(evTableED$date)) #somehow, dateWin converted from interval to numeric!
alply(head(evTableED$dateWin),2,function(w,d){(w )},head(evTableED$date)) #somehow, dateWin converted from interval to numeric!
alply(head(evTableED$dateWin),1,function(w,d){(w )},head(evTableED$date)) #somehow, dateWin converted from interval to numeric!

llply(evTableED$dateWin[1:3],function(w){class(w) }) #somehow, dateWin converted from interval to numeric!

llply(head(evTableED$dateWin),function(w,d){(d )},head(evTableED$date))
llply(head(evTableED$dateWin),function(w,d){class(w)},head(evTableED$date)) #crashes



laply(head(evTableED$date),function(d,w){any(d %within% w)&},head(evTableED$dateWin),head(evTableED$type))
# for each event, get distance to closest event of each node


