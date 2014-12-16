data_resources_april<-read.csv('C:/Users/JM/Documents/experiment-data/parsed/04.Abril.csv')
data_resources_may<-read.csv('C:/Users/JM/Documents/experiment-data/parsed/05.Mayo.csv')
data_resources_june<-read.csv('C:/Users/JM/Documents/experiment-data/parsed/06.Junio.csv')
data_resources<-rbind(data_resources_april,data_resources_may,data_resources_june)
data_resources$date<-gsub('Mar','3',data_resources$date)
data_resources$date<-gsub('May','5',data_resources$date)
data_resources$date<-gsub('Apr','4',data_resources$date)
data_resources$date<-gsub('Jun','6',data_resources$date)
data_resources$date<-strptime(data_resources$date,format="%d-%m-%y %I:%M:%S %p",tz="Europe/Madrid")


##Gráficas interesantes
machine_resources<-'ASWPEUID02.sceu.corp'
machine_events<-'aswpeuid02.sceu.corp'

  #events on the system
  ggplot(data_perico[data_perico$node==machine_events,])+geom_point(aes(x=date,y=type,color=type),size=3)
  #certain date range
  minDate<-ymd_hms('2014-05-12 22:00:00',tz="Europe/Madrid")
  maxDate<-ymd_hms('2014-05-13 02:00:00',tz="Europe/Madrid")
  #ggplot(data_perico[data_perico$node==machine_events&data_perico$date>minDate&data_perico$date<maxDate,])+geom_point(aes(x=date,y=type,color=type),size=3)
      #+scale_x_datetime(limits=c(minDate,maxDate))


plot_resource_and_events<-function(machine_resources,machine_events,resource,minDate=ymd(2014-04-01),maxDate=ymd(2014-06-15),limit=FALSE){
  #Resources
  df<-data_resources[data_resources$node==machine_resources,]
  df_cpu<-df[df$resource==resource,]
  df_cpu<-data.frame(date=df_cpu$date,res=df_cpu$value)
    
  #Events
  df_events<-data_perico[data_perico$node==machine_events,]
  
 plot<-ggplot()+geom_line(data=df_cpu,aes(x=date,y=res),color='blue')+ylab(resource)+geom_point(data=df_events,aes(x=date,y=as.numeric(type),color=type))
  if (limit){plot<-plot+scale_x_datetime(limits=c(minDate,maxDate))}
 return(plot)
}

##UNA FUNCION CON TODOS LOS RECURSOS QUE NO SEAN CERO Y LOS EVENTOS EN CADA GRÁFICA
machine_resources<-c('DSWPEUIN01.sceu.corp')
machine_events<-c('dswpeuin01.sceu.corp')
minDate<-ymd_hms('2014-06-01 12:00:00',tz="Europe/Madrid")
maxDate<-ymd_hms('2014-06-02 02:00:00',tz="Europe/Madrid")
index<-1
resource_plot<-list()
df<-data_resources[data_resources$node==machine_resources,]
for (i in which(summary(df$resource)!=0)){
  resource_plot[[index]]<-plot_resource_and_events(machine_resources,machine_events,levels(df$resource)[i],minDate,maxDate,TRUE)
  index<-index+1
}
multiplot(resource_plot[[1]],resource_plot[[2]],resource_plot[[3]],resource_plot[[4]])


##Una función para hacer gráficas iguales variando la máquina

machine_resources<-c('DSWPEUIN01.sceu.corp')
machine_events<-c('dswpeuin01.sceu.corp')
resource<-'Physical Memory Utilization %'
minDate<-ymd_hms('2014-05-13 00:03:00',tz="Europe/Madrid")
maxDate<-ymd_hms('2014-05-13 00:30:00',tz="Europe/Madrid")
index<-1
machines_plot<-list()
for (i in 1:length(machine_resources)){
  machines_plot[[index]]<-plot_resource_and_events(machine_resources[i],machine_events[i],resource,minDate,maxDate,TRUE)
  index<-index+1
}
machines_plot[[1]]

multiplot(machines_plot[[1]],machines_plot[[2]],machines_plot[[3]],machines_plot[[4]])


##Todos los eventos de varias máquinas
machine_events<-c('dswpeuin01.sceu.corp','dswpeuin02.sceu.corp','dswpeuwt01.sceu.corp','dswpeuwt02.sceu.corp')
minDate<-ymd_hms('2014-05-12 12:00:00',tz="Europe/Madrid")
maxDate<-ymd_hms('2014-05-13 02:00:00',tz="Europe/Madrid")
index<-1
machines_plot<-list()
events_dates<-list()
for (i in 1:length(machine_events)){
  #events_dates[[index]]<-c(data_perico$date[data_perico$node==machine_events[i]&data_perico$type==68917],data_perico$date[data_perico$node==machine_events[i]&data_perico$type==69481])
  machines_plot[[index]]<-ggplot(data_perico[data_perico$node==machine_events[i],])+geom_point(aes(x=date,y=as.numeric(type),color=type),size=3)+ggtitle(machine_events[i])+scale_x_datetime(limits=c(minDate,maxDate))
  index<-index+1
}
#events_dates
multiplot(machines_plot[[1]],machines_plot[[2]],machines_plot[[3]],machines_plot[[4]])
#ggplot()+geom_line(data=data_fw_cpu,aes(x=date,y=cpu),color='blue')+scale_x_datetime(limits=c(time[1]-800*60,time[2]+800*60))+geom_point(data=data_events_trim,aes(x=date,y=as.numeric(type)),color='red',size=3)


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
    # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
    numPlots = length(plots)
    # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
    if (numPlots==1) {
    print(plots[[1]])
    } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,layout.pos.col = matchidx$col))
    }
  }
}