monitoring<-resultList$logTable
date<-seq(monitoring[[1]]$cpu$date[1],monitoring[[1]]$cpu$date[4912],by=300)
norm_monitoring<-list()
for (j in 1:length(monitoring))
{
  df<-data.frame(date,NA,NA,NA)
  names(df)<-c("date","CPU","MEM","INT")
  date1<-monitoring[[j]]$cpu$date
  df$CPU[which(df$date %in% date1)]<-monitoring[[j]]$cpu$Average[which(date1 %in% df$date)]
  
  date1<-monitoring[[j]]$mem$date
  df$MEM[which(df$date %in% date1)]<-monitoring[[j]]$mem$Average[which(date1 %in% df$date)]
  
  date1<-monitoring[[j]]$int$date
  df$INT[which(df$date %in% date1)]<-monitoring[[j]]$int$In[which(date1 %in% df$date)]
  
  norm_monitoring[[names(monitoring)[j]]]<-df
  print(c("Completed node ",j))
}