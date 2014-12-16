library(manipulate)

id1Levels<-levels(log$id1)
excLevels<-levels(log$Exception)


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

indicesNode<-list()
for (i in 1:length(id1Levels)) {indicesNode[[i]]<-which(log$id1==id1Levels[i])}
names(indicesNode)<-id1Levels
indicesNode<-c(indicesNode,All=list(1:length(log$date)))

indicesExc<-list()
for (i in 1:length(excLevels)) {indicesExc[[i]]<-which(log$Exception==excLevels[i])}
excLevels[excLevels==""]<-"_"
names(indicesExc)<-substrRight(excLevels,28)

indicesExc<-c(indicesExc,All=list(1:length(log$date)))

id1Breaks<-list("hours","days","weeks","mins")

manipulate(
  hist(log$date[intersect(node,exception)],breaks=fBreaks,freq=TRUE),
  node=picker(indicesNode,initial="All"),
  exception=picker(indicesExc,initial="All"),
  fBreaks=picker(id1Breaks,initial="days")
)

minDateNum<-as.numeric(log$date[1])
maxDateNum<-as.numeric(log$date[length(log$date)])

manipulate(hist(log$date[tail(which(log$date<minDate+1),n=1):which(log$date>maxDate-1)[1]],breaks=fBreaks,freq=TRUE),
           minDate=slider(minDateNum,maxDateNum,initial=minDateNum,step=1800),
           maxDate=slider(minDateNum,maxDateNum,initial=maxDateNum,step=1800),
           fBreaks=picker(id1Breaks,initial="days")
)





