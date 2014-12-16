function(.data,.glm,useExp=TRUE)
{
  tmpNames<-names(.data)
  numIndices<-laply(.data,is.numeric)
  facIndices<-laply(mydata,is.factor)
  if(useExp)
  {
    ci<-exp(confint(.glm))
    coefs<-exp(.glm$coefficients)
  }else
    {
  .confint<-(confint(.glm))
  coefs<-(.glm$coefficients)
  }
  
  numData<-.data %.% select(numIndices)
  facData<-.data %.% select(facIndices)
  
  f1<-function(.xname,.yname,.data,.glm,.confint)
  {
    dataCol<-.data[[.name]]
    qs<-c(min(dataCol),mean(dataCol),max(dataCol))
    deltaX<-(qs[3]-qs[1]) # difference between max and min of feature
    coef<-.glm$coefficients[.name]
    inc<-NA
    if(!is.na(coef))
    {
      tmpCi<-as.data.frame(t(.confint))[[.name]]
      coefs<-c(tmpCi[1],coef,tmpCi[2])
      inc<-coefs*deltaX
    }else{coef<-c(NA,NA,NA)}
    c(qs,coef,inc)
  }
  #get list of min,mean,max for each variable, 
  #and max contribution of coefficient
  numTable<-ldply(names(numData),f1,numData,.glm,.confint)
  rownames(numTable)<-names(numData)
  names(numTable)<-c("Min.","Mean","Max.","Coef5%","CoefMean","Coef95%")
  
  
  
  #TODO: INCLUDE ALSO CONFIDENCE INTERVALS
  
  f2<-function(.name,.data,.glm)
  {
    dataCol<-.data[[.name]]
    levs<-levels(dataCol)
    coef<-.glm$coefficients[.name]
    inc<-NA
    if(!is.na(coef))
    {
      inc<-coef/(qs[3]-qs[1])
    }
    c(qs,coef=coef,inc)
  }
  
  
}

cTable<-(coefDelta(.lm,addConfint=TRUE,relativeUnits=TRUE))
cTable2<-data.frame(t(cTable)) %.% mutate(names=as.factor(colnames(cTable)))
ggplot(cTable2)+aes(x=reorder(names,length(names):1),y=avg,ymin=ci05,ymax=ci97)+geom_pointrange()+coord_flip()
ggplot(cTable2)+aes(x=reorder(names,1:length(names)),y=avg,ymin=ci05,ymax=ci97)+geom_pointrange()+coord_flip()
coefplot(.lm,intercept=FALSE)

#TODO: make it work with factors, with same axes as coefplot!
