#source(eventsPerSample.R)
library(reshape2)
library(dplyr)
#remove if plots are removed
library(ggplot2)
library(gridExtra)
library(lubridate)

evTable<-eventTable %.% select(date,type,node) %.% rename(c(node="type2")) %.% tbl_df()
startTime<-floor_date(evTable$date[1],"day")
day(startTime)<-1
month(startTime)<-12
evTableWindowed<-getEventsInWindow(evTable,startTime=startTime)
#using following line due to bugged dplyr::arrange
#replace with arrange(interval) when fixed
evTableWindowed<-evTableWindowed[with(evTableWindowed, order(interval)),]


evTableByType<- evTableWindowed %.% dcast(interval~type, fun.aggregate=sum) %.% tbl_df()
evTableByNode<- evTableWindowed %.% dcast(interval~type2, fun.aggregate=sum) %.%  tbl_df()

#todo: use regression, train in half of tables, xval in another quarter of tables

#plots
p1<-ggplot(melt(evTableByNode,id="interval"))+aes(interval,value,color=variable)+geom_point()
p2<-ggplot(melt(evTableByType,id="interval"))+aes(interval,value,color=variable)+geom_point()
grid.arrange(p1,p2,ncol=1)