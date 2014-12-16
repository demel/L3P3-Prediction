require(ROCR)
require(ggplot2)
require(gridExtra)
#source('C:/Users/capelastegui/workspace/OFP/utils/ggROCR.R')
source("C:/Users/JM/Documents/GitHub/l3p3/R/Y1/Raw Pedro's Scripts/utils/ggROCR.R")

# we use 'trainTables' variable from pred-1-LogR-1-preprocess.R

# For each observation window table in trainTables$back,
## for each prediction  window table in trainTables$fw,
### for each column (other than date) in the prediction window table,
### generate a table appending the obs. win. table to the column,
# and store all tables in a tree of lists.
#
# Example: An individual operation from the loop
# predictionTable <- cbind(trainTables$back[[1]],target=trainTables$fw[[1]][,-1][[1]])

# see plyr.nested::llply.cross.ab

#source('C:/Users/capelastegui/workspace/OFP/utils/plyr.nested.R')
source("C:/Users/JM/Documents/GitHub/l3p3/R/Y1/Raw Pedro's Scripts/utils/plyr.nested.R")

getPredTables <- function (back,fw)
{
  llply.cross.ab(back, fw, .fun2=function(back.1,fw.1)
  {
    llply (fw.1 %.% select(-date),back.1,.fun=function(fw.1.col,back.1)
    {cbind(back.1,target=fw.1.col) %.% tbl_df()}
    )}
  )
}

# go over nested list 'predTables.train', and from the prediction tables at depth level 3
## build logistic regression models
## and store results in a nested list with the same structure

getGlmList <- function (predTables, .formula)
{
  glmList <- llply.n(predTables,3, .formula, .fun2=function(.predTable,.formula)
  {glm(.formula, data=.predTable,family=binomial)})
  glmList <- llply.n.labelname(glmList,3, pasteLabels=FALSE)
  return(glmList)
}

# see package ROCR, and my utility functions ggROCR

getPerfs <-function(glmList,predTableList)
{
  perfs<-llply.parallel.ab(glmList ,predTableList, 3,.fun=function(.glm,predTable)
  { 
    #check for cases with only 1 class - would crash ROCR::prediction
    if(.glm$null.deviance==0 | length(table(predTable$target)) <2)
    { return(list())}
    # if there is no test prediction table for this train prediction trable
    if(is.null(predTable)){ return(list())}
    pred <- predict (.glm, predTable, type="response")
    pred.rocr <- prediction(pred, predTable$target)
    pred.rocr.fixed <- prediction.fixed(predTable$target)
    perf.roc <-  performance (pred.rocr,"tpr","fpr")
    perf.roc.fixed <-  performance (pred.rocr.fixed,"tpr","fpr")
    perf.prrc<- performance (pred.rocr,"prec","rec")
    perf.prrc.fixed<- performance (pred.rocr.fixed,"prec","rec")
    perf.f <- performance (pred.rocr,"f")
    perf.f.fixed <- performance (pred.rocr.fixed,"f")
    
    #find threshold of max F-score
    #TODO: THIS SHOULD BE SET AT _TRAINING_ ; ADD ATTRIBUTE THRESHOLD TO GLM
    f<-perf.f@y.values[[1]]
    threshold<-perf.f@x.values[[1]][which.max(f)]
    
    pred.df <- data.frame(date= predTable$date, target=predTable$target, pred.glm=pred>=threshold)
    
    breaksTmp<- seq.POSIXt(floor_date(pred.df$date[1], "day"), ceiling_date(tail(pred.df$date,1),"day"),300)
    breaksTmpLabels<-head(as.POSIXct(breaksTmp),-1)
    
    pred.df <- pred.df %.% mutate(match=target==pred.glm) %.%
      mutate(interval=as.POSIXct(cut.POSIXt(date, breaks=breaksTmp, labels=breaksTmpLabels))) %.%
      select(-date) %.% melt(id=c("interval","match")) %.% tbl_df() %.%
      group_by(interval,variable,value,match) %.% summarise(events=n())
    
    list(roc=list(glm=perf.roc,fixed=perf.roc.fixed),
         prrc=list(glm=perf.prrc,fixed=perf.prrc.fixed),
         f=list(glm=perf.f,fixed=perf.f.fixed),
         match=pred.df)
    
  })
  perfs<-llply.n.labelname(perfs,3)
  return(perfs)
}

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}




getPerfPlotList<-function(perfs)
{ 
  if(length(perfs)<3){return(list())}
  .name<-attr(perfs,".name")
  p.roc<- ggROCR.roc(perfs$roc)+theme(legend.position="bottom", legend.title=element_blank())
  #extract legend
  p.legend<-g_legend(p.roc)
  lheight <- sum(p.legend$height)
  p.heights<-unit.c(unit(1, "npc") - lheight, lheight)
  #calculate plot widths
  p.roc<-p.roc+theme(legend.position="none")
  p.f<- ggROCR.f(perfs$f)+theme(legend.position="none")
  p.prrc<- ggROCR.prrc(perfs$prrc)+theme(legend.position="none")
  pred.match<-ggplot(perfs$match)+aes(x=interval, y=events,color=match, alpha=0.6)+geom_point()+
    facet_grid(variable~value)+ theme(legend.position="top")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # base plots without legends
  plots1<-arrangeGrob(p.roc,p.prrc,p.f, pred.match,nrow=1)
  # add legend on bottom, title
  p.title<-textGrob(.name, gp=gpar(fontsize=12)) 
  plots<-arrangeGrob(plots1,p.legend, nrow=2, heights=p.heights, main=p.title)
  #plots<-arrangeGrob(p.roc,p.prrc,p.f,nrow=1,main=.name,legend=p.legend, widths=p.widths)
  return(plots)
}


savePlots<-function (plots,title="Plot", path="/", prefix="")
{
  llply.n(plots,2, title, path, prefix, .fun=function(perfPlotList1,title, path, prefix)
  {
    .name<-attr(perfPlotList1,".name")
    title<-paste(title,.name,sep="")
    fileName<-file.path(path,paste(prefix,.name,".png"))
    # remove empty list elements
    perfPlotList1<-(llply(perfPlotList1,function(p){if(length(p)>0)return(p)}))
    perfPlotList1<-perfPlotList1[laply(perfPlotList1, function(a){!is.null(a)})]
    tmp<-ggsave(filename=fileName,  width=30, height=18, dpi=90,
                plot= splat(arrangeGrob)(c(perfPlotList1, main=title) ) )
  })
}


getPredictorPerformances <- function(testTables, glmList=NULL, .formula=NULL, save.plots=FALSE, plot.title="Plot", save.path="/", save.prefix="")
{
  predTables<-getPredTables (testTables$back, testTables$fw)
  if(is.null(glmList))
  {
    if(is.null(.formula)) {return(NULL)} else {
      glmList <-getGlmList(predTables,.formula)}
  }
  perfs <- getPerfs(glmList,predTables)
  plots <-llply.n(perfs,3 ,.fun2=getPerfPlotList)
  plots <-llply.n.labelname(plots,3)
  plots <-llply.n.labelname(plots,2)
  if(save.plots) 	{savePlots(plots,title=plot.title, path=save.path, prefix=save.prefix)}
  return ( list(perfs=perfs, plots=plots, glmList=glmList) )
}



#predTables.train<-getPredTables(winEvTable.train$back, winEvTable.train$fw)
#predTables.test<-getPredTables (winEvTable.test$back, winEvTable.test$fw)
#glmList <-getGlmList(as.formula(predTables.train,"target ~ . - date"))

perfs.train<-getPredictorPerformances(winEvTable.train,.formula=as.formula("target ~ . - date"))
glmList <- perfs.train$glmList
perfs.test<-getPredictorPerformances(winEvTable.test,glmList)


baseDataPath <- "C:\\Users\\JM\\Documents\\GitHub\\l3p3\\R\\Logistic Regression Predictor"
file.prefix.train <- "pred-1-logReg-1-Train"
file.prefix.test <- "pred-1-logReg-1-Test"


savePlots(perfs.train$plots,title="Predictor Summary - Training Data - ", path=baseDataPath, prefix=file.prefix.train)
savePlots(perfs.test$plots,title="Predictor Summary - Test Data - ", path=baseDataPath, prefix=file.prefix.test)


#Two variable 
perfs.train<-getPredictorPerformances(winEvTable.train,.formula=as.formula("target ~ . - date"))
glmList <- perfs.train$glmList
perfs.test<-getPredictorPerformances(winEvTable.test,glmList)



