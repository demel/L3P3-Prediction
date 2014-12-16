require(ROCR)
require(plyr)
require(dplyr)
require(ggplot2)

# adapting ROCR plots to ggplot standards
# we plot from a 'performance' object, composed of:
## - x.name: label X
## - y.name: label Y
## - alpha.name : color label
## - x.values: list of numeric X values
## - y.values: list of numeric Y values
## - alpha.values: list of numeric alpha values

#We use ... parameter for plot title.

rocrToDf <- function(perf)
{
  alpha<-perf@alpha.values
  if(length(alpha)!=length(perf@x.values)){alpha=1}
  df<-data.frame(x=perf@x.values, y=perf@y.values, alpha=alpha)
  names(df)<-c("x","y","alpha")
  df<- df %.% mutate (y.max=max(y,na.rm=TRUE))
  attr(df,"XLabel")<-perf@x.name
  attr(df,"YLabel")<-perf@y.name
  attr(df,"alphaLabel")<-perf@alpha.name
  df
}

rocrListToDf <- function(perfList)
{
  perfList<-llply.labelname(perfList)
  df<-ldply(perfList, .fun=function(perf){rocrToDf(perf)}  ) %.% tbl_df()
  attr(df,"XLabel")<-perfList[[1]]@x.name
  attr(df,"YLabel")<-perfList[[1]]@y.name
  attr(df,"alphaLabel")<-perfList[[1]]@alpha.name
  df
}



ggROCR.roc <- function (perf, title=NULL)
{
  if(class(perf)=="list"){perf.df<-rocrListToDf(perf)} else {perf.df<-rocrToDf(perf)}
  
  if(is.null(title)) {title<-"ROC"} 
  
  p<-ggplot(perf.df)+aes(x=x,y=y)+geom_line()+geom_point()+
    scale_x_continuous(limits=c(0,1))+
    scale_y_continuous(limits=c(0,1))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    theme(plot.title=element_text(size=12))+
    labs(x=attr(perf.df, "XLabel"), y=attr(perf.df, "YLabel"), title=title)
  if(!is.null(perf.df$.id)){p<-p+aes(color=.id)} else {
    p+geom_abline(slope=1,color="blue",alpha=0.3)}
  p
}

ggROCR.prrc <- function (perf, title=NULL)
{
  if(class(perf)=="list"){perf.df<-rocrListToDf(perf)} else {perf.df<-rocrToDf(perf)}
  
  if(is.null(title)) {title<-"Prec/Recall"} 
  
  p<-ggplot(perf.df)+aes(x=x,y=y)+geom_line()+geom_point()+
    scale_y_continuous(limits=c(0,1))+
    scale_x_continuous(limits=c(0,1))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    theme(plot.title=element_text(size=12))+
    labs(x=attr(perf.df, "XLabel"), y=attr(perf.df, "YLabel"), title=title)
  if(!is.null(perf.df$.id)){p<-p+aes(color=.id)}
  p
}

ggROCR.f <- function (perf, title=NULL)
{
  if(class(perf)=="list"){perf.df<-rocrListToDf(perf)} else {perf.df<-rocrToDf(perf)}
  
  if(is.null(title)) {title<-"F-score"} 
  
  p<-ggplot(perf.df)+aes(x=x,y=y)+geom_line()+geom_point(alpha=0.7)+
    scale_y_continuous(limits=c(0,1))+
    scale_x_continuous(limits=c(0,1))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    theme(plot.title=element_text(size=12))+
    labs(x=attr(perf.df, "XLabel"), y="F-measure", title=title)
    
  if(!is.null(perf.df$.id)){p<-p+aes(color=.id)+geom_hline(aes(yintercept=y.max, color=.id),alpha=0.4)}
  p
}


ggROCR.generic <- function (perf, title=NULL)
{
#   if(class(perf)=="list"){perf.df<-rocrListToDf(perf)}else
#     {perf.df<-rocrToDf(perf)}
  
  if(class(perf)=="list"){perf.df<-rocrListToDf(perf)} else {perf.df<-rocrToDf(perf)}
  
  p<-ggplot(perf.df)+aes(x=x,y=y,color=alpha)+geom_line()+geom_point()+
    labs(x=attr(perf.df, "XLabel"), y=attr(perf.df, "YLabel"), title=title)
  if(!is.null(perf.df$.id)){p<-p+aes(color=.id)}
  p
}

ggROCR.generic.list <- function (perfList, title=NULL)
{
  perf.df<-rocrListToDf(perfList)
  
  ggplot(perf.df)+aes(x=x,y=y,color=.id)+geom_line()+geom_point()+
    labs(x=attr(perf.df, "XLabel"), y=attr(perf.df, "YLabel"), title=title)
}


#####


# pred <- predict (.glm, .glm$model, type="response")
# pred.rocr <- prediction(pred, .glm$model$target)
# perf.roc <-  performance (pred.rocr,"tpr","fpr")

prediction.custom <- function(labels, .fun)
{
  if (is.data.frame(labels)) {
    names(labels) <- c()
    labels <- as.list(labels)
  }
  else if (is.matrix(labels)) {
    labels <- as.list(data.frame(labels))
    names(labels) <- c()
  }
  else if ((is.vector(labels) || is.ordered(labels) || is.factor(labels)) && 
             !is.list(labels)) {
    labels <- list(labels)
  }
  else if (!is.list(labels)) {
    stop("Format of labels is invalid.")
  }
  myfun<-.fun
  predictions<-llply(labels,myfun, .fun=function(l1,myfun){laply(l1,.fun=myfun)})
  prediction(predictions=predictions, labels=labels)
}

prediction.fixed <- function(labels)
{
  prediction.custom(labels,.fun=function(l1){0.5})
}



####
# Takes as input a prediction table, a glm, a model threshold and a window size
# pred.match.df will have as columns: 
## interval (date intervals of size=windowSize), 
## variable - whether a measure corresponds to data labels ("target") or predictions ("pred.glm")
## value - whether a data point has an event (TRUE) or not.
## match - whether prediction=target

getPredMatchDf <- function (predTable, .glm, threshold=0.5, windowSize=300)
{
  pred <- predict (.glm, predTable, type="response")
  pred.match.df <- data.frame(date= predTable$date, pred.glm=pred>=threshold, target=predTable$target)
  breaksTmp<- seq.POSIXt(floor_date(pred.match.df$date[1], "day"), ceiling_date(tail(pred.match.df$date,1),"day"),windowSize)
  breaksTmpLabels<-head(as.POSIXct(breaksTmp),-1)
  pred.match.df <- pred.match.df %.% mutate(match=target==pred.glm) %.%
    mutate(interval=as.POSIXct(cut.POSIXt(date, breaks=breaksTmp, labels=breaksTmpLabels))) %.%
    select(-date) %.% melt(id=c("interval","match")) %.% tbl_df() %.%
    group_by(interval,variable,value,match) %.% summarise(events=n())
  attr(pred.match.df, "threshold")<-threshold
  return(pred.match.df)
}

#usage:
#getPredMatchDf(predTable=get.nested(predTables,c(1,1,1)), .glm=get.nested(glmList,c(1,1,1)), threshold=0.5)

###
getPredMatchPlot <- function(predMatchDf, size=2, vjitter=1, alpha=0.7, colorMatches=TRUE, legend.position="top")
{
threshold<-round (attr(predMatchDf,"threshold") ,2)
pred.match<-ggplot(predMatchDf)+
  aes(x=interval, y=events)+
  geom_point(position=position_jitter(0,vjitter), size=size, alpha= alpha)+
  facet_grid(variable~value)+
  theme(legend.position="top")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title=element_text(size=12))+
  labs(x="Date", y="Event Count", title=paste("Prediction Match; th=",threshold ))
if(colorMatches){pred.match<-pred.match+aes(color=match)+theme(legend.position=legend.position)}
return(pred.match)
}


#TODO: how to make a prediction.COIN with random outcomes?
# how to give it a customizable threshold that is compatible with ROCR threshold plots?


