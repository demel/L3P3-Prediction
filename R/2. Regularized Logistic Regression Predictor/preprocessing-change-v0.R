require(lubridate)
require(plyr)
require(dplyr)
require(glmnet)
source('C:/Users/JM/Documents/Github/l3p3/R/Logistic Regression Predictor/utils.R')
source("C:/Users/JM/Documents/GitHub/l3p3/R/Y1/Raw Pedro's Scripts/totta/totta-1/tools/eventsPerSample.R")
source("C:/Users/JM/Documents/GitHub/l3p3/R/Y1/Raw Pedro's Scripts/utils/plyr.nested.R")


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

data_april<-read.table('C:/Users/JM/Documents/experiment-data/parsed/04.Abril-events.csv',sep=",")
names(data_april)<-c("Severity","Created_On","Name","Event_Type","Event","Created_By","Cleared_On","Cleared_By","Model_Type_Name","Event_Precedence")
data_may<-read.table('C:/Users/JM/Documents/experiment-data/parsed/05.Mayo-events.csv',sep=",")
names(data_may)<-c("Severity","Created_On","Name","Event_Type","Event","Created_By","Cleared_On","Cleared_By","Model_Type_Name","Event_Precedence")
data_june<-read.table('C:/Users/JM/Documents/experiment-data/parsed/06.Junio-events.csv',sep=",")
names(data_june)<-c("Severity","Created_On","Name","Event_Type","Event","Created_By","Cleared_On","Cleared_By","Model_Type_Name","Event_Precedence")
data_july<-read.table('C:/Users/JM/Documents/experiment-data/parsed/07.Julio-events.csv',sep=",")
names(data_july)<-c("Severity","Created_On","Name","Event_Type","Event","Created_By","Cleared_On","Cleared_By","Model_Type_Name","Event_Precedence")
data_august<-read.table('C:/Users/JM/Documents/experiment-data/parsed/08.Agosto-events.csv',sep=",")
names(data_august)<-c("Severity","Created_On","Name","Event_Type","Event","Created_By","Cleared_On","Cleared_By","Model_Type_Name","Event_Precedence")
data<-rbind(data_april,data_may,data_june,data_july,data_august)
names(data)<-c("Severity","Created_On","Name","Event_Type","Event","Created_By","Cleared_On","Cleared_By","Model_Type_Name","Event_Precedence")
data<-correctDates(data)
levels(data$Severity)[levels(data$Severity)==""]<-"Blank"
data$Event_Type<-factor(data$Event_Type)
data_filtered<-data.frame("date"=data$Created_On,"node"=droplevels(data$Name),"type"=droplevels(data$Event_Type))
data_filtered$type<-factor(paste(data_filtered$node,data_filtered$type,sep="-"))

evTable<-data_filtered %.% tbl_df() %.% select(date,type)
evTable<-evTable[order(evTable$date),]


get_prediction_table_back<-function(data,back=300,offset_b=0){
  events<-sort(as.character(unique(data$type)))
  dates<-sort(unique(data$date))
  result<-data.frame(matrix(FALSE,ncol=length(events)+1,nrow=length(dates)))
  names(result)<-c("date",as.character(events))
  for (i in 1:length(dates)){
    row<-logical(length=length(events))
    back_f<-data %.% filter (date<=dates[i]-offset_b & date >= dates[i]-back)
    events_b<-as.character(unique(back_f$type))
    indexes<-match(events_b,events)
    row[indexes]<-TRUE
    row<-c(dates[i],row)
    result[i,]<-row
    if (i%%100==0){print(c(i,"/",length(dates)))}
  }
  return(result)
}
tmp<-get_prediction_table_back(evTable)

