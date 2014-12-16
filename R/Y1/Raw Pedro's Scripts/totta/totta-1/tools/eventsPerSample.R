library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)


#usage
#getEventsInWindow(date,.data,300, "backward")

getEventsInWindow<-function(windowOrigin,data,windowSize=300,direction="forward", offset=0)
{
    # filter: events before or after window origin
  data.filtered<-data.frame()
    # check window origin offset 
  if(is.null(offset) || offset<0){offset<-0}
  if(offset>windowSize){offset<-windowSize}
  if(direction=="forward")
  {data.filtered<-data %.% filter (date>windowOrigin+offset & date <= windowOrigin+windowSize)}else 
  if(direction=="backward")
  {data.filtered<-data %.% filter (date<=windowOrigin-offset & date >= windowOrigin-windowSize)}else 
  if(direction=="both")
  {data.filtered<-data %.% filter (date<=windowOrigin+windowSize & date >= windowOrigin-windowSize)}
    # if empty window, return row marking this date as having no events in window  
  if(nrow(data.filtered)==0) {return(data.frame(date=windowOrigin, type="NO.EVENTS", evInWin=TRUE))}
    #if (countEvents) result <- data.filtered %.% group_by(type) %.%  summarise(minDist1=n())
  result<-data.filtered %.% group_by(type) %.% summarise(evInWin=n()>0) %.% mutate(date=windowOrigin) %.% select(date,type,evInWin)
}


applyGetEventsInWindow<-function(data,windowSize=300,direction="forward", offset=0)
{
  tmp<-ldply(unique(data$date),getEventsInWindow,data, windowSize=windowSize,direction=direction, offset=offset) 
  dcast(tmp, date ~ type, fun.aggregate= any) %.% tbl_df()
}



#plot
#ggplot(data=tmpTable)+aes(x=interval,y=messages,color=type)+geom_point()+facet_wrap(~type,ncol=4,scales="free")

getTypeDistances<-function(index,data,maxDist=24*3600,direction="forward",method="eventDist")
{
  #data<-evTableED
  tmpResult1<-data %.% mutate(dist1=as.numeric(as.duration(date[index]%--%date))) 
  win<-switch(direction,
              forward=(index:nrow(data))[-1], 
              backward=(1:index)[-index], 
              both=-index)
  tmpResult2<- tmpResult1[win,] %.% group_by(type) 
  # table of type levels
  levelTable<-tbl_df(data.frame(type=levels(tmpResult1$type)))  
  # Result to return in case of empty data
  resultEmpty<-levelTable %.% 
    mutate(minDist1=switch(method,
                           anyEvent=FALSE,
                           eventDist=as.numeric(rep(NA,length(type)))
                           )) %.% trans_df() 
  if(nrow(tmpResult2)==0) #empty window, return empty row 
  {return(resultEmpty)}
  
  if(method=="eventDist")
  {
    maxDistInData<-max(abs(tmpResult2$dist1))
    tmpResult3<-tmpResult2 %.% summarise(minDist1=min(abs(dist1),maxDist))
    tmpResult4<-left_join (levelTable,tmpResult3,by="type")
    if (maxDistInData>=maxDist)
    {
      tmpResult4<-tmpResult4 %.% 
        mutate(minDist1=replace(minDist1,is.na(minDist1),maxDist)) 
    }
    tmpResult4<-tmpResult4 %.% trans_df()
  }  
  if(method=="anyEvent")
  {
    tmpResult3<-tmpResult2 %.% filter(abs(dist1)<maxDist) %.% group_by(type)
    if (nrow(tmpResult3)==0) #empty window, return empty row 
    {return(resultEmpty)}
    tmpResult3 <- tmpResult3 %.% summarise(minDist1=n()>0)
    tmpResult4 <- left_join (levelTable,tmpResult3,by="type") %.%
      mutate(minDist1=!is.na(minDist1) & minDist1) %.% 
      trans_df()
  }
  tmpResult4
}

#usage
#getTypeDistances(index,data,maxDist=24*3600,direction="forward",method="eventDist")

applyGetTypeDistances<-function(data,maxDist=24*3600,direction="forward",method="eventDist",append=TRUE)
{
  distTable<-ldply(1:nrow(data),getTypeDistances,data, maxDist=maxDist,direction=direction,method=method) 
  if(append){distTable<-cbind(data,distTable)}
  distTable %.% tbl_df()
}




#usage
#distTable<-applyGetTypeDistances(evTableED)
#evTableED %.% joinorsomethinglikethat(distTable)


#apply factor again to a data frame
refactor<-function(.data)
{colwise(function(col) {if (is.factor(col)) factor(col)  else col }) (.data)
}


#transpose a data frame, assign type - needs homogeneous column types
trans_df<-function(.data,type=NA)
{
  tmpNames<-.data[,1]
  result <- as.data.frame(t(.data[,-1]),stringsAsFactors=FALSE)
  colnames(result) <- tmpNames
  if(!is.na(type)){result <- switch(type,character=result,
                                    factor=colwise(factor)(result),
                                    numeric=colwise(as.numeric)(result),
                                    logical=colwise(as.logical)(result))}
  result
}