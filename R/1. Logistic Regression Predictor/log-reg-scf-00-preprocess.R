require(lubridate)

                                         ###Logistic Regression Experiments###

   #Pedro's work replication#


#General data table#
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


#Pedro's data table#
#data_perico<-data[data$Severity=='Major'|data$Severity=='Critical',]
data_perico<-data
data_perico<-data.frame("date"=data_perico$Created_On,"node"=droplevels(data_perico$Name),"type"=droplevels(data_perico$Event_Type))
event_types<-levels(data_perico$type)
#data_perico<-recodeFactors(data_perico)
  #From here, Pedro's code can be used, turning data_perico into eventTable1
eventTable1<-data_perico
