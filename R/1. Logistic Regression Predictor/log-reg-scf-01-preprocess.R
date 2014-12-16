require(plyr)
require(dplyr)
require(lubridate)
#source('C:/Users/capelastegui/workspace/OFP/Totta/Totta-1/2-R/tools/eventsPerSample.R')
source("C:/Users/JM/Documents/GitHub/l3p3/R/Y1/Raw Pedro's Scripts/totta/totta-1/tools/eventsPerSample.R")
#source('C:/Users/capelastegui/workspace/OFP/utils/plyr.nested.R')
source("C:/Users/JM/Documents/GitHub/l3p3/R/Y1/Raw Pedro's Scripts/utils/plyr.nested.R")



evTable<-eventTable1 %.% tbl_df() %.% select(date,type)
evTable<-evTable[order(evTable$date),]

applyGetEventsInWindow<-function(data,windowSize=300,direction="forward", offset=0)
{
  tmp<-ldply(unique(data$date),getEventsInWindow,data, windowSize=windowSize,direction=direction, offset=offset) 
  dcast(tmp, date ~ type, fun.aggregate= any) %.% tbl_df()
}

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


#We want to generate the following tables
# allnodes
## backward.5m, backward.30m, backward.2h
## forward.5m, forward.30m, forward.2h
getWinEvTables <-function(data,
                          backward=list(b.5m=5*60,b.30m=30*60),
                          forward=list(f.5m=5*60,f.30m=30*60), 
                          offset.back=list(),
                          offset.fw=list(f.30m=5*60))
{
  back<-llply.parallel.multilist(list.ref=backward,
                                 list.multi=list(windowSize=backward, offset=offset.back),
                                 n=1,
                                 data=data,
                                 .fun=function(sizeOffsetList,data)
                                 {applyGetEventsInWindow(data,windowSize=sizeOffsetList$windowSize, offset=sizeOffsetList$offset,direction="backward")}
  )
  
  attr(back,"windowSize")<-backward
  attr(back,"offset")<-offset.back
  
  fw<-llply.parallel.multilist(list.ref=forward,
                               list.multi=list(windowSize=forward, offset=offset.fw),
                               n=1,
                               data,
                               .fun=function(sizeOffsetList,data)
                               {applyGetEventsInWindow(data,windowSize=sizeOffsetList$windowSize, offset=sizeOffsetList$offset,direction="forward")}
  )
  
  attr(fw,"windowSize")<-forward
  attr(fw,"offset")<-offset.fw
  
  list(back=back,fw=fw)
}


winEvTable <-getWinEvTables(evTable)

# MY INTERVAL
date.trainStart <-dmy("1-4-14")
date.trainEnd   <- dmy_hms("31-5-14 23:59:59")
trainInterval <- date.trainStart %--% date.trainEnd
#


winEvTable.train <- llply(winEvTable,
                          function(tableList)
                          {llply(tableList,
                                 function(table){table %.% filter (date %within% trainInterval)}
                          )         
                          })

winEvTable.test <- llply(winEvTable,
                         function(tableList)
                         {llply(tableList,
                                function(table){table %.% filter (!date %within% trainInterval)}
                         )         
                         })
