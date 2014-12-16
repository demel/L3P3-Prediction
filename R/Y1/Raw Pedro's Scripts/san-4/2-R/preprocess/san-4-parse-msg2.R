levels(log$msg2)<-substr(levels(log$msg2),1,120)

l<-levels(log$msg2)
msg2<-data.frame(a=l[l!=""], stringsAsFactors = F)
regexExp<-"(Exception: |)(.+Exception)(.*)"
regexId<-"(\\[\\w{15}\\])"


msg2b<-data.frame(strapplyc(as.character(msg2$a),regexExp,simplify=rbind),stringsAsFactors = F)
msg2b<-msg2b[,-1]
names(msg2b)<-c("Exception","ExceptionMsg")
msg2c<-as.character(strapplyc(as.character(msg2b$ExceptionMsg),regexId))

msg2c<-gsub("character(0)","",msg2c, fixed=T)
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
logExc<-(cbind(log,exception))
#so far, so good
#logExc<-data.frame(log,exception)
logExc$Exception<-factor(as.character(logExc$Exception))
levels(logExc$Exception)<-(gsub("character(0)","",levels(logExc$Exception),fixed=T))
logExc$ExceptionMsg<-factor(as.character(logExc$ExceptionMsg))
levels(logExc$ExceptionMsg)<-(gsub("character(0)","",levels(logExc$ExceptionMsg),fixed=T))
logExc$ExceptionMethod<-factor(as.character(logExc$ExceptionMethod))
levels(logExc$ExceptionMethod)<-(gsub("character(0)","",levels(logExc$ExceptionMethod),fixed=T))
