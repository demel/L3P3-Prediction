library(stringr)
library(gsubfn)

dataPath <-"C:\\Users\\capelastegui\\workspace\\OFP\\Santander-4\\1-Data\\2-test\\"
fileIn <- paste(dataPath, "san-4-was-raw-subset.log", sep="")
fileOut <- paste(dataPath, "san-4-was-raw-subset-table.log", sep="")

regex <- "(\".{26}\"),(\".{15}\"),(|\"[^\"]*\"),(|\"[^\"]*\"),(|\"[^\"]*\"),(|\".*\"),(|\"[^\"]*\"),(|\"UNKNOWN\"|\".{17}\")$"
lines <- readLines(fileIn)

indices<-setdiff(1:length(lines),grep(regex,lines))
indicesA<-indices[indices[1:length(indices)-1]- indices[2:length(indices)] == -1]
indicesB<-indices[indices[1:length(indices)-1]- indices[2:length(indices)] == -1]+1
lines[indicesA]<-paste(lines[indicesA], lines[indicesB])
lines<-lines[-indicesB]

log<- as.data.frame(dfFromRegex(lines, regex))
names(log)<-c("date","id1", "id2", "comp1", "comp2", "msg1", "msg2", "id3")

Sys.setlocale("LC_TIME", "english")
date<-as.character(log$date)
log$date <- strptime(date, "\"%Y-%d-%m-%H.%M.%OS")

msg2<-as.character(log$msg2)
log$msg2<-as.factor(substr(msg2,1,120))

id1Sub=c("\"BVSNWRPC201    \"","\"BVSNWRPC216    \"","\"BVSNWRPC301    \"","\"BVSNWRPC316    \"")
logSub<-(log[log$id1 %in% id1Sub,])
logOld<-log
log<-logSub
log[,-1]<-lapply(log[,-1],factor)

for(i in 2:length(log))
{ levels(log[,i])<-substr(levels(log[,i]),2,nchar(levels(log[,i]))-1)}

levels(log$id1)<-str_trim(levels(log$id1))


write.table(log, fileOut)
