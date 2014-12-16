library(plyr)

nullLocationIndex<-which(log2$LOCATION=="")


log2$LOCATION<- as.character(log2$LOCATION)
log2$LOCATION[nullLocationIndex]<- log2$BLOCK[nullLocationIndex]
log2$LOCATION<- factor(log2$LOCATION)
names(log2)[11]<-"MESSAGE"

#lapply(log2,class)


#remove ECID,BLOCK
log0<-rbind.fill(log1[,c(1:10,13)],log2)
log0$MESSAGE<-factor(log0$MESSAGE)

fileOutTable0 <- "C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\1-processed\\log-intrepid-table-csv"
write.table(log0, file=fileOutTable0, sep="\t", row.names=FALSE, quote=FALSE)

logError <- log0[log0$SEVERITY=="ERROR"|log0$SEVERITY=="FATAL",]
logError[c(1:6,8:11)] <- data.frame(lapply(logError[,c(1:6,8:11)],factor))

fileOutTable0 <- "C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\1-processed\\log-intrepid-table-error-csv"
write.table(logError, file=fileOutTable0, sep="\t", row.names=FALSE, quote=FALSE)

logFailure <- logError[logError$SEVERITY=="FATAL",]
logFailure[c(1:6,8:11)] <- data.frame(lapply(logFailure[,c(1:6,8:11)],factor))

fileOutTable0 <- "C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\1-processed\\log-intrepid-table-failure-csv"
write.table(logFailure, file=fileOutTable0, sep="\t", row.names=FALSE, quote=FALSE)