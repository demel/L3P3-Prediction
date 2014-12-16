library(stringr)
library(gsubfn)

dataPath <-"C:\\Users\\capelastegui\\workspace\\OFP\\Santander-4\\1-Data\\2-test\\"
fileIn <- file(paste(dataPath, "san-4-was-raw-filtered.log", sep=""), "rb")
fileOut <- paste(dataPath, "san-4-was-table.log", sep="")

regex <- "(\".{26}\"),(\".{15}\"),(|\"[^\"]*\"),(|\"[^\"]*\"),(|\"[^\"]*\"),(|\".*\"),(|\"[^\"]*\"),(|\"UNKNOWN\"|\".{17}\")$"

lines <- readLines(fileIn)
lines<-gsub("\"\"\"\"", "''''", lines)

indicesWrong<-grep(regex,lines,perl=TRUE,invert=TRUE)
lines<-lines[-indicesWrong]

log<- as.data.frame(dfFromRegex(lines, regex))
names(log)<-c("date","id1", "id2", "comp1", "comp2", "msg1", "msg2", "id3")


Sys.setlocale("LC_TIME", "english")
date<-as.character(log$date)
log$date <- strptime(date, "\"%Y-%m-%d-%H.%M.%OS")

# msg2<-as.character(log$msg2)
# log$msg2<-as.factor(substr(msg2,1,120))

id1Sub=c("\"BVSNWRPC201    \"","\"BVSNWRPC216    \"","\"BVSNWRPC301    \"","\"BVSNWRPC316    \"")
logSub<-(log[log$id1 %in% id1Sub,])
logOld<-log
log<-logSub
log[,-1]<-lapply(log[,-1],factor)

for(i in 2:length(log))
{ levels(log[,i])<-substr(levels(log[,i]),2,nchar(levels(log[,i]))-1)}

log<-log[order(log$date),]

levels(log$id1)<-str_trim(levels(log$id1))
levels(log$msg2)<-substr(levels(log$msg2),1,120)

l<-levels(log$msg2)
msg2<-data.frame(a=l[l!=""], stringsAsFactors = F)
regexExp<-"(Exception: |)(.+Exception)(.*)"
regexId<-"(\\[\\w{15}\\])"


msg2b<-data.frame(strapplyc(as.character(msg2$a),regexExp,simplify=rbind),stringsAsFactors = F)
msg2b<-msg2b[,-1]
names(msg2b)<-c("Exception","ExceptionMsg")
msg2c<-as.character(strapplyc(as.character(msg2b$ExceptionMsg),regexId))
msg2c<-gsub("character(0)","",msg2c, fixed=TRUE)
#msg2c<-factor(as.character(strapplyc(as.character(msg2b$ExceptionMsg),regexId)))

msg2b$ExceptionMethod<-msg2c
msg2<-cbind(msg2,msg2b)


getExceptionPars <- function(exceptionField,exceptionLevels)
{
  #msg2[msg2$a==tmpLog[1,]$msg2,]
  #replace variables
  line<-exceptionLevels[exceptionLevels$a==exceptionField,2:4]
}

exception<-t(sapply(log$msg2,getExceptionPars,msg2))
log<-(cbind(log,exception))
#so far, so good
#log<-data.frame(log,exception)
log$Exception<-factor(as.character(log$Exception))
levels(log$Exception)<-(gsub("character(0)","",levels(log$Exception),fixed=TRUE))
levels(log$Exception)[levels(log$Exception)==""]<-"_"
dupIndices<-which(duplicated(gsub(".*\\.","",levels(log$Exception))))
levels(log$Exception)[-dupIndices]<-gsub(".*\\.","",levels(log$Exception)[-dupIndices])
log$ExceptionMsg<-factor(as.character(log$ExceptionMsg))
levels(log$ExceptionMsg)<-(gsub("character(0)","",levels(log$ExceptionMsg),fixed=TRUE))
log$ExceptionMethod<-factor(as.character(log$ExceptionMethod))
levels(log$ExceptionMethod)<-(gsub("character(0)","",levels(log$ExceptionMethod),fixed=TRUE))


write.table(log, fileOut)