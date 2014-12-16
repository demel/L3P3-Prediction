indices<-1:length(log$date)
normalizedIndices<-scale(as.numeric(indices))
normalizedDate<-scale(as.numeric(log$date))

lin<- lm(normalizedDate ~ normalizedIndices)
x<-lin$residuals

df2<-cbind(normalizedIndices/4,x)
di2<-dist(df2)
cl2<-kmeans(di2,3)
plot(as.numeric(log$date),col=cl2$cluster)

clusterLabel<-factor(cl2$cluster)
levels(clusterLabel)<- c("set2","set1", "set3")


t<-table(log$id1)
t
length(levels(log$id2))
t<-table(log$id2)
t[which(t>5)]

length(levels(log$comp1))
t<-table(log$comp1)
t

length(levels(log$comp2))
t<-table(log$comp2)
t[which(t>5)]
t[which(t<5)]

length(levels(log$msg1))
t<-table(log$msg1)
sort(t[which(t>15)])

length(levels(log$msg2))
t<-table(log$msg2)
sort(t[which(t>15)])

length(levels(log$id3))
t<-table(log$id3)
sort(t[which(t>15)])


tables<-list()
logl<-list()
for(i in 1:length(levels(clusterLabel)))
{
  logl[[i]]<-log[clusterLabel==levels(clusterLabel)[i],]
  logl[[i]][,-1]<-lapply(logl[[i]][,-1],factor)
  tables[[i]]<-list()
  for(j in 2:length(logl[[i]]))
    {
    tables[[i]][[j-1]]<-table(logl[[i]][,j])
    }
}

sapply(lapply(log[,-1],table),length)
sapply(tables[[1]],length)
sapply(tables[[2]],length)
sapply(tables[[3]],length)


t<-tables[[1]][2]
t[which(t>5)]

length(levels(log$comp1))
t<-table(log$comp1)
t

length(levels(log$comp2))
t<-table(log$comp2)
t[which(t>5)]
t[which(t<5)]

length(levels(log$msg1))
t<-table(log$msg1)
sort(t[which(t>15)])

length(levels(log$msg2))
t<-table(log$msg2)
sort(t[which(t>15)])

length(levels(log$id3))
t<-table(log$id3)
sort(t[which(t>15)])