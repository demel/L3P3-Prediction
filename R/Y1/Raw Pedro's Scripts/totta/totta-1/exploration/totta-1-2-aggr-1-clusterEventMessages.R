#msg<-levels(failTable$msg)
library(plyr)

msg<-levels(miniTable$msg)
#get words
wordList<-strsplit(msg," ")
#take first,last words
#wordList<-llply(wordList,function(l){c(head(l),tail(l))})
uniqueWordList<-llply(wordList,unique)

uniqueWordCount<-laply(uniqueWordList,length)

wordDistance1 <-function(wordArray1,wordArray2)
{
  i<-length(intersect(wordArray1,wordArray2))
  l1<-length(wordArray1)
  l2<-length(wordArray2)
  (l1-i)/(2*l1)+(l2-i)/(2*l2)
}

wordDistance2 <-function(wordArray1,wordArray2)
{
  i<-length(intersect(wordArray1,wordArray2))
  l1<-length(wordArray1)
  l2<-length(wordArray2)
  (l1+l2-2*i)/(l1+l2)
}

wordDistance3 <-function(wordArray1,wordArray2)
{
  i<-length(intersect(wordArray1,wordArray2))
  l1<-length(wordArray1)
  l2<-length(wordArray2)
  max((l1-i)/(2*l1),(l2-i)/(2*l2))
}

wordDistance4 <-function(wordArray1,wordArray2)
{
  i<-length(intersect(wordArray1,wordArray2))
  l1<-length(wordArray1)
  l2<-length(wordArray2)
  # percentage1 * percentage 2
  1 - (i^2/max(l1*l2,1))
}

distanceTmp <-function(wordArray1,wordArray2)
{
  i<-length(intersect(wordArray1,wordArray2))
  l1<-length(wordArray1)
  l2<-length(wordArray2)
  # percentage1 * percentage 2
  1 - (i^2/max(l1*l2,1))
}

getWordDistance<-function(wordList,listCrop=NULL,distanceFunction=wordDistance1)
{
  if(!is.null(listCrop)&is.numeric(listCrop))
  {
    wordListTmp<-llply(wordList,function(l){c(head(l,listCrop),tail(l,listCrop))})
  }else {wordListTmp<-wordList}

  uniqueWordList<-llply(wordListTmp,unique)
  wordDistanceApply <-function(wordArray,wordArrayList)
  {laply(wordArrayList,distanceFunction,wordArray)}
  
  as.dist(laply(uniqueWordList,wordDistanceApply,uniqueWordList))
}

getClusterTable<-function(dataList,label, centers=12,nstart=20, listCrop=NULL, distanceFunction=wordDistance1)
{
  df<-getWordDistance(dataList,listCrop=listCrop, distanceFunction=distanceFunction)
  cl<-kmeans(df,centers,nstart=nstart)
  cluster<-data.frame(cluster=cl$cluster,i=1:length(dataList),msg=label)
  
  getClusterLabels<-function(x,v,msg)
  {
    #get index of first element of that level
    i<-which(v==x)[1]
    #use that message as label
    msg[i]
  }
    # for each cluster level, assign a label
  clusterLabels<-laply(1:length(dataList),getClusterLabels,cluster$cluster,label)
  cluster$label<-factor(substr(clusterLabels[cluster$cluster],0,80))
    # return table with event #, cluster #, cluster label, event message
  cluster<-cluster[c("i","cluster","label","msg")]
}

#TODO: ADD a quality indicator attribute, e.g. tot.withins

cluster1<-getClusterTable(wordList,msg,listCrop=20,centers=10,nstart=10,distanceFunction=wordDistance1)
cluster4<-getClusterTable(wordList,msg,listCrop=20,centers=10,nstart=10,distanceFunction=wordDistance4)


checkLevels<-function(cluster)
{#cut cluster table by cluster label, for each partition generate a table of event messages
  dlply(cluster, .(label),.fun=function(df){data.frame(table(factor(df$msg)))})
}
  
tmp<-checkLevels(cluster1)
tmpP<-list(length(tmp))
for(i in 1:length(tmp))
{
  tmpP[[i]]<-ggplot(data=tmp[[i]])+aes(y=Freq,x=Var1)+geom_bar(stat="identity")+
      coord_flip()+ggtitle(names(tmp)[i])+scale_x_discrete(labels=substr(tmp[[i]]$Var1,0,80))
}


ggplot(data=cluster1)+aes(x=cluster,y=label)+geom_point()
ggplot(data=cluster2)+aes(x=cluster,y=label)+geom_point()


p1<-ggplot(melt(as.matrix(d1)), aes(x=Var1,y=Var2,fill=value))
p1+geom_tile()+scale_fill_gradient(low="yellow", high="red")+scale_y_discrete(label=substr(msg,0,80))

  #get binary distance matrix to visualize results. Use with cluster table. If same cluster, d=0, else d=1.
getClusterMatch<-function(list)
{
  binaryDistance <-function(x1,x2)
  {
    if(x1==x2) {distance=0}else{distance=1}
    distance
  }
  distanceApply <-function(array,list)
  {laply(list,binaryDistance,array)}
  
  laply(list,distanceApply,list)
}

d1b<- getClusterMatch(cluster1$cluster)
p1b<-ggplot(melt(as.matrix(d1b)), aes(x=Var1,y=Var2,fill=value))
p1b+geom_tile()+scale_fill_gradient(low="yellow", high="red")+scale_y_discrete(label=substr(msg,0,80))

d2b<- getClusterMatch(cluster2$cluster)
p2b<-ggplot(melt(as.matrix(d1b)), aes(x=Var1,y=Var2,fill=value))
p2b+geom_tile()+scale_fill_gradient(low="yellow", high="red")+scale_y_discrete(label=substr(msg,0,80))


p2<-ggplot(melt(as.matrix(d2)), aes(x=Var1,y=Var2,fill=value))
p2+geom_tile()+scale_fill_gradient(low="yellow", high="red")+scale_y_discrete(label=substr(msg,0,80))




