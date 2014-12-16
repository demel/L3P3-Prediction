logError <- log[log$SEVERITY=="FATAL"|log$SEVERITY=="FAILURE",]
logError[,c(1:6,8:15)] <- data.frame(lapply(logError[,c(1:6,8:15)],factor))

fileOutTableError <- "C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\1-processed\\intrepid-log-error-table.csv"
write.table(log, file=fileOutTableError, sep="\t", row.names=FALSE, quote=FALSE)

t <- table(logError$LOCATION)
t[which(t>1)]
table(logError$SEVERITY)
table(logError$COMPONENT)
table(logError$SUBCOMPONENT)
table(logError$ERRCODE)
t <- table(logError$ECID)
t[which(t>1)]

t <- table(logError$MESSAGE)
t[which(t>1)]

t<-table(logError$MSG_ID[logError$MESSAGE=="There were 1 proxy processes that did not cleanly at the end of job ...." ])
t[which(t>1)]

t<-table(logError$MSG_ID[logError$MESSAGE=="There were 1 proxy processes that did not cleanly at the end of job ...." ])
t[which(t>1)]

logTmp<-logError[logError$MSG_ID!="KERN_1E23"&logError$MSG_ID!="KERN_2302",]
