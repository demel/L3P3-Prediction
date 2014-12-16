library(ggdendro)
library(ggplot2)
library(plyr)
library(cluster)
library(reshape2)

# tmpVector<-read.csv("C:/Users/capelastegui/workspace/OFP/Totta/Totta-1/1-Data/0-test/clusters.txt",header=FALSE)
# tmpLabels<-names(read.csv("C:/Users/capelastegui/workspace/OFP/Totta/Totta-1/1-Data/0-test/labels.txt"))
# tmpDist<-dist(tmpVector,method="binary")

# hCluster1<-hclust(tmpDist,method="single")

plotHClus<-function(hCluster, h=NULL, k=NULL)
{#plots a hierarchical cluster with coloured levels
  hdata<-dendro_data(hCluster,type="rectangle")

  if(is.null(h) & is.null(k))
    {
    phc<-ggplot(hdata$segments)+geom_segment(aes(x=x,y=y,xend=xend,yend=yend))+
      coord_flip()+scale_x_discrete(labels=hdata$labels$label)
    return (phc)
    }
  #Adding colour  
    #cut tree into clusters 
  clusterLevels= cutree(hCluster1, h=h,k=k)
    #indices for mapping each label in hdata to a cluster level
  colIndices<-laply(hdata$labels$label,function(a,b){which(a==b)[1]},names(clusterLevels))
    #use indices to map hdata labels to cluster levels
  hcColours<-as.factor(clusterLevels[colIndices])
    #get colour map that can be used to plot, add to hdata
  hdata$segments$hcColourMap<-hcColours[as.integer(segment(hdata)$x)]
  phc2<-ggplot(hdata$segments)+geom_segment(aes(x=x,y=y,xend=xend,yend=yend))+
    coord_flip()+scale_x_discrete(labels=hdata$labels$label)
  phc2+aes(colour=hcColourMap)
}

#usage
#plotHClus(hCluster1)
#plotHClus(hCluster1,k=15)

#get kmeans clusters
  #plot distance heatmaps

plotDistHMap<-function(distance)
{#Plot heat map for a distance object
  ggplot(melt(as.matrix(distance)), aes(x=Var1,y=Var2,fill=value))+ 
    geom_tile()+  scale_fill_gradient(low="yellow", high="red")
}

# phm1<-plotDistHMap(dist(tmpVector,method="euclidean"))
# phm1
# phm2<-plotDistHMap(dist(tmpVector,method="binary"))
# phm2
#remove unused variables
removedIndices<-which(!laply(llply(tmpVector,unique),length)>1)
tmpVector2<-tmpVector[,-removedIndices]



kCluster1<-kmeans(tmpVector,10,nstart=10, iter.max=50)

pamCluster1<-pam(tmpVector,10)
kCluster2<-pam(tmpVector,10,metric="manhattan" )

getKmClusters<-function(kmCluster,useMelt=FALSE,sortClusters=FALSE, factorVariables=TRUE)
{
  clusters<-kmCluster$cluster
  if(sortClusters){clusters<-sort(clusters)}
  if(!is.null(names(clusters))){clNames<-names(clusters)} else 
    {clNames<-as.character(1:length(clusters))}
  
  clusterMatch <-function(x1,x2)
    {  if(x1==x2) {return(x1)}else{return(NA)}  }
  clusterMatchApply <-function(value,list)
    {laply(list,clusterMatch,value)}
  
  cMatch<-laply(clusters,clusterMatchApply,clusters)
  if(useMelt)
  {
    cMatch<-melt(cMatch,na.rm=TRUE)
    if(factorVariables){cMatch$xlabel<-factor(clNames[cMatch$Var1],levels=clNames)} else
      {cMatch$xlabel=cMatch$Var1}
    if(factorVariables){cMatch$ylabel<-factor(clNames[cMatch$Var2],levels=clNames)} else
      {cMatch$ylabel=cMatch$Var2}
    return(cMatch)
  }
  else
  {
    names(cMatch)<-clNames
    row.names(cMatch)<-clNames
    return(cMatch)
  }
}
#Note: struggles with cluster plots larger than 1000x1000
plotKCluster<- function(kmCluster,useFacet=FALSE, showXLabels=FALSE, sortClusters=TRUE, factorVariables=TRUE)
{
  kClusterMatch1<- getKmClusters(kmCluster,useMelt=TRUE,sortClusters=sortClusters, factorVariables=factorVariables)
  kClusterPlot1<-ggplot(kClusterMatch1)+ geom_tile()+scale_fill_discrete() 
  if(!showXLabels){kClusterPlot1<-kClusterPlot1+ aes(x=Var1,y=ylabel,fill=as.factor(value))}
  else
    {
    kClusterPlot1<-kClusterPlot1+ aes(x=xlabel,y=ylabel,fill=as.factor(value))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
  if(useFacet){kClusterPlot1<-kClusterPlot1+facet_wrap(~value,ncol=3,scales="free")}

  return(kClusterPlot1)
}

p<-plotKCluster(kCluster1)
p

kClusterMatch1<- getKmClusters(kCluster1,useMelt=TRUE,sortClusters=TRUE)
kClusterPlot1<-ggplot(kClusterMatch1, aes(x=Var1,y=ylabel,fill=as.factor(value)))
kClusterPlot1+geom_tile()+scale_fill_discrete()#
kClusterPlot1+geom_tile()+scale_fill_discrete()+facet_wrap(~value,ncol=3,scales="free")
#end nice plots

getPamClusters<-function(pam,useMelt=FALSE,sortClusters=TRUE)
{
  clusters<-pam$clustering
  if(sortClusters){clusters<-sort(clusters)}
  
  clusterMatch <-function(x1,x2)
  {
    if(x1==x2) {match=x1}else{match=NA}
    match
  }
  clusterMatchApply <-function(value,list)
  {laply(list,clusterMatch,value)}
  
  cMatch<-laply(clusters,clusterMatchApply,clusters)
  if(useMelt){cMatch<-melt(as.matrix(cMatch))}
}

reorderFactorByCluster <-function (f, c)
{#given factor f, numeric array c representing assignment of clusters to levels, reorder
  factor(f,levels(f)[as.numeric(names(sort(c)))])
}



pamClusterMatch1<- getPamClusters(pamCluster1,useMelt=TRUE,sortClusters=TRUE)
pamClusterPlot1<-ggplot(pamClusterMatch1, aes(x=Var1,y=ylabel,fill=as.factor(value)))
kClusterPlot1+geom_tile()+scale_fill_discrete()#
kClusterPlot1+geom_tile()+scale_fill_discrete()+facet_wrap(~value,ncol=3,scales="free")



pamClusterPlot1<-ggplot(pamClusterMatch1, aes(x=X1,y=as.factor(X2),fill=as.factor(value)))
pamClusterPlot1+geom_tile()+scale_fill_discrete()
pamClusterPlot1+geom_tile()+scale_fill_discrete()+facet_wrap(~value,ncol=3,scales="free")
  


kClusterPlot<-ggplot(melt(as.matrix(kClusterMatch)), aes(x=X1,y=X2,fill=value))
kClusterPlot+geom_tile()+scale_fill_gradient()+scale_y_discrete(labels=tmpLabels)
kClusterPlot<-ggplot(melt(as.matrix(kClusterMatch)), aes(x=X1,y=X2,fill=as.factor(value)))
kClusterPlot+geom_tile()+scale_fill_discrete()+scale_y_discrete(labels=tmpLabels)

result_unsorted<-kCluster1
result_sorted<-kCluster1
result_sorted$cluster<-sort(result_sorted$cluster,decreasing=TRUE)
kClusterMatch<- getKmClusters(result_sorted$cluster)
kClusterPlot<-ggplot(melt(as.matrix(kClusterMatch)), aes(x=X1,y=X2,fill=value))
kClusterPlot+geom_tile()+scale_fill_gradient()+scale_y_discrete(labels=tmpLabels)
kClusterPlot<-ggplot(melt(as.matrix(kClusterMatch)), aes(x=X1,y=X2,fill=as.factor(value)))
kClusterPlot+geom_tile()+scale_fill_discrete()+scale_y_discrete(labels=tmpLabels)






#not a distance
kCluster1<-kmeans(tmpVector,20,nstart=10, iter.max=100)
pkc<-ggplot(melt(as.matrix(tmpDist)), aes(x=Var1,y=Var2,fill=value))
pkc+geom_tile()+scale_fill_gradient(low="yellow", high="red")
tmpBinDist<- getBinaryDistance(kCluster1$cluster)
tmpBinPlot<-ggplot(melt(as.matrix(tmpBinDist)), aes(x=Var1,y=Var2,fill=value))
tmpBinPlot+geom_tile()+scale_fill_gradient(low="yellow", high="red")

tmpDist<-dist(tmpVector,method="binary")
kCluster1<-kmeans(tmpDist,10,nstart=10, iter.max=100)
pkc<-ggplot(melt(as.matrix(tmpDist)), aes(x=Var1,y=Var2,fill=value))
pkc+geom_tile()+scale_fill_gradient(low="yellow", high="red")
tmpBinDist<- getBinaryDistance(kCluster1$cluster)
tmpBinPlot<-ggplot(melt(as.matrix(tmpBinDist)), aes(x=Var1,y=Var2,fill=value))
tmpBinPlot+geom_tile()+scale_fill_gradient(low="yellow", high="red")


kCluster1<-NbClust(tmpVector, method="kmeans")


