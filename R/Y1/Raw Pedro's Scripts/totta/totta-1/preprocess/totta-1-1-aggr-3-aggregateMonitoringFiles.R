monitoring<-resultList$logTable
date<-seq(monitoring[[1]]$CPU$date[1],monitoring[[1]]$CPU$date[4912],by=300)
norm_monitoring<-list()
for (j in 1:length(monitoring))
{
  df<-data.frame(date,NA,NA,NA)
  names(df)<-c("date","CPU","MEM","INT")
  date1<-monitoring[[j]]$CPU$date
  df$CPU[which(df$date %in% date1)]<-monitoring[[j]]$CPU$Average[which(date1 %in% df$date)]
  
  date1<-monitoring[[j]]$MEM$date
  df$MEM[which(df$date %in% date1)]<-monitoring[[j]]$MEM$Average[which(date1 %in% df$date)]
  
  date1<-monitoring[[j]]$INT$date
  df$INT[which(df$date %in% date1)]<-monitoring[[j]]$INT$In[which(date1 %in% df$date)]
  
  norm_monitoring[[names(monitoring)[j]]]<-df
  print(c("Completed node ",j))
}