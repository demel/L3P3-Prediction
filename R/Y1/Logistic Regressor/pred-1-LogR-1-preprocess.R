require(plyr)
require(dplyr)
require(lubridate)
#source('C:/Users/capelastegui/workspace/OFP/Totta/Totta-1/2-R/tools/eventsPerSample.R')
source("C:/Users/JM/Documents/GitHub/l3p3/R/Y1/Raw Pedro's Scripts/totta/totta-1/tools/eventsPerSample.R")
#source('C:/Users/capelastegui/workspace/OFP/utils/plyr.nested.R')
source("C:/Users/JM/Documents/GitHub/l3p3/R/Y1/Raw Pedro's Scripts/utils/plyr.nested.R")


#events_allnodes<-eventTable %.% filter(severity!="BLANK") %.% select (date,node,type) %.% refactor()

#load data
#baseDataPath <- "C:/Users/capelastegui/workspace/OFP/SCF/SCF-1/1-Data"
baseDataPath <- "C:/Users/JM/Documents/Prueba-Santander/scf"
visualizationFolder <- file.path(baseDataPath,"4-VisualizationTables")
eventsAllnodesFile <- file.path(visualizationFolder,"events-allnodes.csv")
eventTable1<-read.csv(eventsAllnodesFile) %.% mutate(date=ymd_hms(date))

evTable<-eventTable1 %.% tbl_df() %.% select(date,type)



# #We want to generate the following tables
# # allnodes
# ## backward.5m, backward.30m, backward.2h
# ## forward.5m, forward.30m, forward.2h
# getWinEvTables <-function(.data,
#                           backward=list(b.5m=5*60,b.30m=30*60),
#                           forward=list(f.5m=5*60,f.30m=30*60), 
#                           offset.back=list(),
#                           offset.fw=list(f.30m=5*60))
# {
#   back<-llply(backward,
#               function(windowSize,.data)
#               {applyGetEventsInWindow(.data,windowSize=windowSize, direction="backward")},
#               .data)
#   
#   attr(back,"windowSize")<-backward
#   
#   fw<-llply(forward,
#             function(windowSize,.data)
#             {applyGetEventsInWindow(.data,windowSize=windowSize, direction="forward")},
#             .data)
#   
#   attr(fw,"windowSize")<-forward
#   
#   list(back=back,fw=fw)
# }



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

# separation of training and test data
date.trainStart <-  dmy("1-2-14")
date.trainEnd   <-  dmy_hms("2-3-14 23:59:59")
trainInterval <- date.trainStart %--% date.trainEnd

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




#tmp <- winEvTables[[1]][[1]]



#ggplot(data=tmpTable)+aes(x=interval,y=messages,color=type)+geom_point()+facet_grid(type~., scales="free")


#exploration
evTable.train<- evTable %.% filter (date %within% trainInterval)
evTable.test<- evTable %.% filter (!date %within% trainInterval)
