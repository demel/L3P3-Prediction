library(plyr)
library(dplyr)


delta<-function(l)
{
  if (!is.numeric(l)) {return(NA)}
  abs(max(l)-min(l))
}

normalize <- function (df)
{
  df.norm<-colwise  (function(c){if(is.numeric(c)){scale(c)}else{c}})(df)
  getAttrs<-function(c,which)
  { #get attributes, change NULL to NA
   at<-attr(c,which)
   if(is.null(at)){NA}else{at}
  }
  means<-laply(df.norm,getAttrs,"scaled:center")
  sds<-laply(df.norm,getAttrs,"scaled:scale")
  
  df.norm<-colwise (function(c){if(is.matrix(c)){as.numeric(c)}else{c}})(df.norm)
  attr(df.norm,"scaled:center")<-means
  attr(df.norm,"scaled:scale")<-sds
  df.norm
}

denormalize <- function(df)
{
  means<-attr(df,"scaled:center")
  sds<-attr(df,"scaled:scale")
  df.denorm<-llply(1:ncol(df),
                   function(i,df,means,sds)
                     {if(is.numeric(df[,i]))
                         {df[,i]*sds[i]+means[i]}else
                           {df[,i]}},
                   df,means,sds)
  names(df.denorm)<-names(df)
  quickdf(df.denorm)
}


coefDeltaPlot <-function (.lm,remIntercept=TRUE)
{
cTable<-(coefDelta(.lm,addConfint=TRUE,relativeUnits=FALSE))
cTable2<-data.frame(t(cTable)) %.% 
  mutate(names=as.factor(colnames(cTable))) %.%
  mutate(goodCoef=ci03>0|ci97<0) %.%
  mutate(isInterc=names=="X.Intercept.")

if(remIntercept){cTable2<-cTable2%.%filter(!isInterc)}

ggplot(cTable2) +
  geom_hline(size=1,color="gray",linetype="longdash")+
  aes(x=reorder(names,1:length(names)),y=avg,ymin=ci03,ymax=ci97,color=goodCoef) + 
  geom_pointrange()+coord_flip()
}

getGoodCoefs <- function(.lm)
{
  .data<-.lm$model 
  numNames<-names(.data) [laply(.data,is.numeric)]
  facData<-(.data) [laply(.data,is.factor)]
  facNames<-names(facData)
  
  cTable<-(coefDelta(.lm,addConfint=TRUE,relativeUnits=FALSE))
  cTable2<-data.frame(t(cTable)) %.% 
    mutate(names=as.factor(colnames(cTable))) %.%
    mutate(goodCoef=ci03>0|ci97<0) %.%
    filter(goodCoef) %.%
    select(names)
  goodCoefs<-as.character(cTable2$names)
  goodCoefs.num<-intersect(goodCoefs,numNames)
  goodCoefs.fac<-setdiff(goodCoefs,numNames)
  #now we want to get
  # -feature names associated to good factor coefficients
  # note that for factors, coeff name != feature name, since it adds level
  goodCoefs.fac.list<-list()
  for(f in facNames)
  {
    for(l in levels(facData[[f]]))
    {
      s<-paste(f,l,sep="")
      #s = e.g. "DivisionW"
      
      if(any(grepl(s,goodCoefs.fac,fixed=TRUE)))
      {goodCoefs.fac.list$f<-f}
    }
  }
  c(goodCoefs.num,as.character(goodCoefs.fac.list))
}

coefDelta <-function(.lm,relativeUnits=FALSE,addConfint=TRUE)
{
  
  .data<-.lm$model 
  numData<-.data [,laply(.data,is.numeric)]
  facData<-.data [,laply(.data,is.factor)]
  
  coefTable<-data.frame(t(.lm$coefficients)) #%.% select(-X.Intercept.)
  if(addConfint)
  {
    confints<-data.frame(t(confint(.lm))) #%.% select(-X.Intercept.)
    rownames(confints)<-(c("ci03","ci97"))
    coefTable<-rbind(confints[1,],avg=coefTable,confints[2,]) 
  }
  intercTable<-coefTable %.% select(X.Intercept.)
  coefTable<-coefTable %.% select(-X.Intercept.)
  
  deltas<-colwise(delta)(numData) #todo: add conf ints to delta
  ydelta<-deltas [!names(deltas) %in%  names(coefTable)]

  getCoefDelta<-function(n,coefTable,deltas)
  {
    deltaCols<-n
    if(!n %in% names(deltas))
      {
      if(!grepl("\\.",n)){return(coefTable[n])}# no ".", n is not interaction
      n.split<-unlist(strsplit(n,"\\."))
      if(length(setdiff(n.split,names(deltas)))>0) #n is not interaction element
      {return(coefTable[n])} else
      {
        deltaCols<-n.split
      }  
      #otherwise, n is intersection element
      } #if factor variable, do nothing
    #.delta<-prod(deltas[deltaCols])^(1/length(deltas[deltaCols]))
    .delta<-prod(deltas[deltaCols]) #I don't know which model is good, if any
    coef<-coefTable[n]
    .delta*coef
  }
  result<-as.data.frame(llply(names(coefTable),getCoefDelta,coefTable,deltas))
  if(relativeUnits){result<-result/as.numeric(ydelta)}
  cbind(intercTable,result)
}






# 
# 
# 
# coefDelta <-function(.lm,relativeUnits=TRUE)
# {
#   coefs<-.lm$coefficients[names(.lm$coefficients)%in%names(.lm$model)]
#   coefs<-data.frame(t(coefs))
#   deltas<-colwise(delta)(.lm$model[laply(.lm$model,is.numeric)])
#   ydelta<-deltas[!names(deltas) %in% names(coefs)]
#   
#   getCoefDelta<-function(n,coefs,deltas)
#   {
#     .delta<-deltas[names(deltas)==n]
#     coef<-coefs[names(coefs)==n]
#     .delta*coef
#   }
#   result<-as.data.frame(llply(names(coefs),getCoefDelta,coefs,deltas))
#   if(relativeUnits){result<-result/as.numeric(ydelta)}
#   result
# }







# lm1$coefficients[names(lm1$coefficients)=="box.office"]*delta(movies$box.office) / delta(movies$score)