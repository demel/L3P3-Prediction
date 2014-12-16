library(gsubfn)

fixedSplit <- function(x, start, end){
  strings <- substring(x, start, end)            # Fracture string by widths
  strings <- gsub("[[:space:]]+$", "", strings)  # Remove white from right
  strings <- gsub("^[[:space:]]+", "", strings)  # Remove white from left
  strings <- paste(strings)        # Make delimited string
  
  return(strings)
}

fileIn <- "C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\0-raw\\log-intrepid-clean"
fileOutTable1 <- "C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\1-processed\\log-intrepid-table1-csv"
fileOutTable2 <- "C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\1-processed\\log-intrepid-table2-csv"


lines <- readLines(fileIn)


startIndex1=c(1, 13,24,41,62, 103,112,150,174,207,272,292,324)
endIndex1=  c(12,23,40,61,102,111,138,161,206,271,291,323,4000)

startIndex2=c(1 ,14,27,46,69, 112,123,152,156,191,225)
endIndex2=  c(13,26,45,68,111,122,151,155,190,222,4000)


log1<- data.frame(t(sapply(lines[1:137261],fixedSplit,start=startIndex1,end=endIndex1, USE.NAMES=FALSE)))

log2<- data.frame(t(sapply(lines[137262:length(lines)],fixedSplit,start=startIndex2,end=endIndex2, USE.NAMES=FALSE)))

columnNames1<- c("RECID", "MSG_ID", "COMPONENT", "SUBCOMPONENT", "ERRCODE", 
                 "SEVERITY", "EVENT_TIME", "PROCESSOR",  "BLOCK", 
                 "LOCATION", "SERIAL", "ECID","MESSAGE")

names(log1) <- columnNames1
log1 <- log1[c(-1,-2),]
log1[c(1:6,8:10)] <- data.frame(lapply(log1[c(1:6,8:13)],factor))

HEASys.setlocale("LC_TIME", "english")
log1$EVENT_TIME<- strptime(log1$EVENT_TIME, "%Y-%m-%d-%H.%M.%OS")

columnNames2<- c("RECID", "MSG_ID", "COMPONENT", "SUBCOMPONENT", "ERRCODE", 
                 "SEVERITY", "EVENT_TIME", "PROCESSOR",  "BLOCK", 
                 "LOCATION", "SERIAL_ECID_MESSAGE")

names(log2) <- columnNames2


lines_backup <- lines
# At this point, there remain non-valid lines due to linebreaks in MESSAGE
# The following code removes those lines and appends them to the previous MESSAGE
# lines_backup <- lines
logInd <- which(log2$SEVERITY!="FATAL"&log2$SEVERITY!="ERROR"&log2$SEVERITY!="INFO"&log2$SEVERITY!="WARN")
#Add 2 to log1 length because we have removed 2 lines!
linesInd <- logInd+(dim(log1)[1])+2
pastedMsg<-paste(log2$SERIAL_ECID_MESSAGE[logInd-1],lines[linesInd])
message<-as.character(log2$SERIAL_ECID_MESSAGE)
message[logInd-1]<-pastedMsg
log2$SERIAL_ECID_MESSAGE<-message
log2<-log2[-logInd,] 

log2[c(1:6,8:10)] <- data.frame(lapply(log2[c(1:6,8:10)],factor))

Sys.setlocale("LC_TIME", "english")
log2$EVENT_TIME<- strptime(log2$EVENT_TIME, "%Y-%m-%d-%H.%M.%OS")


write.table(log1, file=fileOutTable1, sep="\t", row.names=FALSE, quote=FALSE)
write.table(log2, file=fileOutTable2, sep="\t", row.names=FALSE, quote=FALSE)
