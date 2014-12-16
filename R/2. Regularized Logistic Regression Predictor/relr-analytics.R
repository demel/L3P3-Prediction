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
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

###Number of not created models
not_created_models<-function(data){
  return(length(which(summary(data)=="0")))
}
not_created_f5<-not_created_models(tmpb5f5)
not_created_f10<-not_created_models(tmpb5f10)
not_created_f15<-not_created_models(tmpb5f15)
not_created_f20<-not_created_models(tmpb5f20)
not_created_f25<-not_created_models(tmpb5f25)
not_created_f30<-not_created_models(tmpb5f30)

###Number of zero models
zero_models<-function(data){
  j<-0
  for (i in 1:length(data)){
    if (!is.null(data[[i]])){
      if (is.nan(data[[i]]$fscore)|data[[i]]$fscore==0){
        j<-j+1
      }
    }
  }
  return (j)
}
zero_models_f5<-zero_models(tmpb5f5)
zero_models_f10<-zero_models(tmpb5f10)
zero_models_f15<-zero_models(tmpb5f15)
zero_models_f20<-zero_models(tmpb5f20)
zero_models_f25<-zero_models(tmpb5f25)
zero_models_f30<-zero_models(tmpb5f30)

###Average precision for models
extract_precision<-function(data){
  result<-numeric()
  j<-1
  for (i in 1:length(data)){
    if (!is.nan(data[[i]]$fscore)){
      result[j]<-data[[i]]$precision
      j<-j+1
      }
  }
  return(result)
}
precision_f5<-mean(extract_precision(tmpb5f5),na.rm=TRUE)
precision_f10<-mean(extract_precision(tmpb5f10),na.rm=TRUE)
precision_f15<-mean(extract_precision(tmpb5f15),na.rm=TRUE)
precision_f20<-mean(extract_precision(tmpb5f20),na.rm=TRUE)
precision_f25<-mean(extract_precision(tmpb5f25),na.rm=TRUE)
precision_f30<-mean(extract_precision(tmpb5f30),na.rm=TRUE)


###Average recall for models
extract_recall<-function(data){
  result<-numeric()
  j<-1
  for (i in 1:length(data)){
    if (!is.nan(data[[i]]$fscore)){
      result[j]<-data[[i]]$recall
      j<-j+1
    }
  }
  return(result)
}
recall_f5<-mean(extract_recall(tmpb5f5),na.rm=TRUE)
recall_f10<-mean(extract_recall(tmpb5f10),na.rm=TRUE)
recall_f15<-mean(extract_recall(tmpb5f15),na.rm=TRUE)
recall_f20<-mean(extract_recall(tmpb5f20),na.rm=TRUE)
recall_f25<-mean(extract_recall(tmpb5f25),na.rm=TRUE)
recall_f30<-mean(extract_recall(tmpb5f30),na.rm=TRUE)

###Average fscore for models
extract_fscore<-function(data){
  result<-numeric()
  j<-1
  for (i in 1:length(data)){
      if (!is.nan(data[[i]]$fscore)){
        result[j]<-data[[i]]$fscore
        j<-j+1
    }
  }
  return(result)
}
fscore_f5<-mean(extract_fscore(tmpb5f5),na.rm=TRUE)
fscore_f10<-mean(extract_fscore(tmpb5f10),na.rm=TRUE)
fscore_f15<-mean(extract_fscore(tmpb5f15),na.rm=TRUE)
fscore_f20<-mean(extract_fscore(tmpb5f20),na.rm=TRUE)
fscore_f25<-mean(extract_fscore(tmpb5f25),na.rm=TRUE)
fscore_f30<-mean(extract_fscore(tmpb5f30),na.rm=TRUE)

###Extract model complexity
model_complexity<-function(data){
  complexity<-numeric()
  for (i in 1:length(data)){
    if (!is.null(data[[i]])){
      complexity[i]<-length(which(coef(data[[i]]$model)!=0))
    }
    else {complexity[i]<-0}
  }
  return(complexity)
}

###Extract amount of training instances
extract_train_instances<-function(data){
  total_instances<-numeric()
  for (i in 1:length(data)){
    ones_training<-length(which(data[[i]]$target==1))
    total_instances[i]<-floor(ones_training*0.6+3*ones_training*0.6)
  }
  return(total_instances)
}

###Complexity Data frames
complexity_frame_5m<-data.frame("instances"=extract_train_instances(predTables$b.5m$f.5m),"complexity"=model_complexity(tmpb5f5))
complexity_frame_10m<-data.frame("instances"=extract_train_instances(predTables$b.5m$f.10m),"complexity"=model_complexity(tmpb5f10))
complexity_frame_15m<-data.frame("instances"=extract_train_instances(predTables$b.5m$f.15m),"complexity"=model_complexity(tmpb5f15))
complexity_frame_20m<-data.frame("instances"=extract_train_instances(predTables$b.5m$f.20m),"complexity"=model_complexity(tmpb5f20))
complexity_frame_25m<-data.frame("instances"=extract_train_instances(predTables$b.5m$f.25m),"complexity"=model_complexity(tmpb5f25))
complexity_frame_30m<-data.frame("instances"=extract_train_instances(predTables$b.5m$f.30m),"complexity"=model_complexity(tmpb5f30))

comp_p1<-ggplot(complexity_frame_5m)+geom_point(aes(x=complexity,y=instances))+geom_line(aes(x=complexity,y=10*complexity),color="red")+ggtitle("5 minutes")
comp_p2<-ggplot(complexity_frame_10m)+geom_point(aes(x=complexity,y=instances))+geom_line(aes(x=complexity,y=10*complexity),color="red")+ggtitle("10 minutes")
comp_p3<-ggplot(complexity_frame_15m)+geom_point(aes(x=complexity,y=instances))+geom_line(aes(x=complexity,y=10*complexity),color="red")+ggtitle("15 minutes")
comp_p4<-ggplot(complexity_frame_20m)+geom_point(aes(x=complexity,y=instances))+geom_line(aes(x=complexity,y=10*complexity),color="red")+ggtitle("20 minutes")
comp_p5<-ggplot(complexity_frame_25m)+geom_point(aes(x=complexity,y=instances))+geom_line(aes(x=complexity,y=10*complexity),color="red")+ggtitle("25 minutes")
comp_p6<-ggplot(complexity_frame_30m)+geom_point(aes(x=complexity,y=instances))+geom_line(aes(x=complexity,y=10*complexity),color="red")+ggtitle("30 minutes")
multiplot(comp_p1,comp_p2,comp_p3,comp_p4,comp_p5,comp_p6,cols=3)

###Measurements frame
total_models<-c(length(tmpb5f5),length(tmpb5f10),length(tmpb5f15),length(tmpb5f20),length(tmpb5f25),length(tmpb5f30))
failed_models<-c(not_created_f5,not_created_f10,not_created_f15,not_created_f20,not_created_f25,not_created_f30)
zero_models<-c(zero_models_f5,zero_models_f10,zero_models_f15,zero_models_f20,zero_models_f25,zero_models_f30)
precision_average<-c(precision_f5,precision_f10,precision_f15,precision_f20,precision_f25,precision_f30)
recall_average<-c(recall_f5,recall_f10,recall_f15,recall_f20,recall_f25,recall_f30)
fscore_average<-c(fscore_f5,fscore_f10,fscore_f15,fscore_f20,fscore_f25,fscore_f30)
m_frame<-data.frame("total_models"=total_models,"failed_models"=failed_models,"zero_models"=zero_models,"precision"=precision_average,"recall"=recall_average,"fscore"=fscore_average)
rownames(m_frame)<-c("f5m","f10m","f15m","f20m","f25m","f30m")
