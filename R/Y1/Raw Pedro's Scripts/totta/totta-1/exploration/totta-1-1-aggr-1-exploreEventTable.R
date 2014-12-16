library(ggplot2)
#Copy event table to work with it.
miniTable<-eventTable
miniTable<-miniTable[-is.na(miniTable$date),]
# Add new variable for faceting data: isFailure
miniTable$isFailure<-miniTable$severity!="BLANK"
# Add new variables: time to clear fail, isCleared
miniTable$clearTime<-as.numeric(as.difftime(miniTable$clearedOn-miniTable$date),units="secs")
miniTable$isCleared<-!is.na(miniTable$clearedOn)
#Number of events
nrow(miniTable)
#base ggplot
lastDay<-tail(miniTable$date[!is.na(miniTable$date)],1)
#use last 3 days from minitable
miniTable<-miniTable[miniTable$date>lastDay-(3600*24*30),]
failTable<-miniTable[miniTable$isFailure,]
debugTable<-miniTable[!miniTable$isFailure,]


p<-ggplot(miniTable)
pf<-ggplot(failTable)
pd<-ggplot(debugTable)
#Plot events per hour
p.date.hist<-p+aes(date)+geom_bar(fill="white",colour="black",binwidth=3600*1)
p.date.hist+facet_grid(severity~.)
p.date.hist+facet_grid(severity~node)
#Plot failure events per hour
pf.date.hist<-pf+aes(date,fill=type)+geom_bar(binwidth=3600*3)
pf.date.hist+facet_grid(node~severity,margins=TRUE)
pf.date.hist+facet_grid(node~severity,scales="free_y",margins=TRUE)
#Plot debug events per hour
pd.date.hist<-pd+aes(date,fill=type)+geom_bar(binwidth=3600*1)
pd.date.hist+facet_grid(node~severity,margins=TRUE)
pd.date.hist+facet_grid(node~severity,scales="free_y",margins=TRUE)

#Plot severity-type-node
pf.type.node<-pf+aes(type,node,colour=severity)+geom_point(position=position_jitter(0.1,0.1))
pf.type.node
pd.type.node<-pd+aes(type,node)+geom_point(position=position_jitter(0.1,0.1))
pd.type.node
p.type.node<-p+aes(type,node,colour=severity)+geom_point(position=position_jitter(0.1,0.1))+
                  facet_grid(isFailure~.,margins=TRUE,labeller = label_both)
p.type.node

#Plot isCleared,clearTime
pf.isCleared<-pf+aes(type,node,colour=isCleared)+geom_point(position=position_jitter(0.1,0.1))
pf.isCleared
pd.isCleared<-pd+aes(type,node,colour=isCleared)+geom_point(position=position_jitter(0.1,0.1))
pd.isCleared

pf.clearTime<-pf+aes(type,clearTime,colour=node)+geom_point(position=position_jitter(0.1,0))
pf.clearTime
#non-failures are never cleared!

#Plot msg,type
pf.msg<-pf+aes(type,msg,colour=isCleared)+geom_point(position=position_jitter(0.1,0.1))
pf.msg
#result: interesting! some failures with same msg are sometimes labeled, sometimes not



hist(miniTable$date[miniTable$Severity!="0"],breaks="hours", freq=TRUE)
#Plot events per day hour
hist(as.POSIXlt(miniTable$date)$hour,breaks=24, freq=TRUE)
#Plot failure events per day hour hour
hist(as.POSIXlt(miniTable$date)$hour[miniTable$Severity!="0"],breaks=24, freq=TRUE)
#Show types of events
sort(table(miniTable$Event.Type))
#Divide table by value of Event Type
miniTable<-dlply(miniTable,.(Event.Type))
#For each subtable, show levels of severity, event message






summarizeMiniTable<-function(mTable)
{
tmp<-mTable[,c("Severity","Event")]
tmp<-llply(tmp,factor)
tmp<-llply(tmp,table)
}
miniTableSummary<-llply(miniTable,summarizeMiniTable)




miniTable<-ddply(miniTable,.variables=("Event.Type"))
t<-table(miniTable$Event)
sort(t)

