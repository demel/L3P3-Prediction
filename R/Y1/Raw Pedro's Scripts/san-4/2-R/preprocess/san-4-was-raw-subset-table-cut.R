dateDiff <-  log$date[2:length(log$date)]- log$date[1:length(log$date)-1] 
#note that dateDiff <0 is not useful due to minor date oscillations
cuts<- which(abs(dateDiff)> 3600)
cuts<-c(1,cuts,length(log$date))
indices<-list(vector())
for (i in 1:(length(cuts)-1))
{
  indices[[i]]<-((cuts[i]+1):cuts[i+1])
  names(indices)[i]<-paste0("set",i)
}

logFields<-c(2,3,4,5,8)
#logFields<-c(2,3)
setLevels<-list(list(vector()))
setLabels<-list(numeric())
#levelsId1<-list(vector())

for (i in 1:length(logFields))
{label=1
  setLevels[[i]]<-list(vector())
  setLabels[[i]]<-numeric()
  
  #setLabels[[i]]
  
  for (j in 1:length(indices))
  {
    setLevels[[i]][[j]]<-levels(factor(log[,logFields[i]][indices[[j]]]))
  }
  for (j in 1: length(setLevels[[i]]))
  {
    if(is.na(setLabels[[i]][j]))
    {
      
      #equalSets<-sapply(setLevels[[i]],setequal,setLevels[[i]][[j]])
      equalSets<-sapply(setLevels[[i]],setequal,setLevels[[i]][[j]])
      setLabels[[i]][equalSets]<- label
      label<-label+1
    }
  }
}
colorsClus<-numeric(length(length(log$date)))
colorsBlo<-numeric(length(length(log$date)))


for(i in 1:length(indices))
{
  colorsClus[indices[[i]]]<-setLabels[[1]][i]
}

for(i in 1:length(indices))
{
  colorsBlo[indices[[i]]]<-i%%30
}

s<-sample(1:length(log$date),600)

