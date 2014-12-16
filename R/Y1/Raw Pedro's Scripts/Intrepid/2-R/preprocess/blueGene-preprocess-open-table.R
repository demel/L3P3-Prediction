library(gsubfn)

fixedSplit <- function(x, start, end){
  strings <- substring(x, start, end)            # Fracture string by widths
  strings <- gsub("[[:space:]]+$", "", strings)  # Remove white from right
  strings <- gsub("^[[:space:]]+", "", strings)  # Remove white from left
  strings <- paste(strings)        # Make delimited string
  
  return(strings)
}

fileIn <- "C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\0-raw\\Intrepid_RAS_0901_0908_scrubbed"
fileOut <- "C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\1-processed\\log-intrepid-clean.csv"
fileOutTable <- "C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\1-processed\\log-intrepid-table"


#lines <- readLines(fileIn, 1000000)
#lines <- readLines(fileIn)

sReadLines <- function(fnam) {
  f <- file(fnam, "rb")
  res <- readLines(f)
  close(f)
  res
}

lines <- sReadLines(fileIn)
lines<-lines[which(lines!="")]
linesInd <- which(lapply(lines, nchar) <283)
for(i in linesInd)
{
  lines[i-1]<-paste(lines[i-1], lines[i])
}
lines<-lines[-linesInd]

system.time(lines<-gsub("\32%", " ", lines))

startIndex1=c(1, 13,24,41,62, 103,112,150,174,207,272,292,324)
endIndex1=  c(12,23,40,61,102,111,138,161,206,271,291,323,4000)

startIndex2=c(1 ,14,27,46,69, 112,123,152,156,191,225)
endIndex2=  c(13,26,45,68,111,122,151,155,190,257,4000)


log1<- data.frame(t(sapply(lines[1:274524],fixedSplit,start=startIndex1,end=endIndex1, USE.NAMES=FALSE)))

log2<- data.frame(t(sapply(lines[274525:length(lines)],fixedSplit,start=startIndex2,end=endIndex2, USE.NAMES=FALSE)))
#system.time(log<- data.frame(t(sapply(lines,fixedSplit,start=startIndex,end=endIndex, USE.NAMES=FALSE))))


namesLine <- lines[3]
#columnNames <- fixedSplit(namesLine,startIndex, endIndex)
columnNames<- c("RECID", "MSG_ID", "COMPONENT", "SUBCOMPONENT", "ERRCODE", 
  "SEVERITY", "EVENT_TIME", "PROCESSOR",  "BLOCK", 
  "LOCATION", "SERIAL", "ECID","MESSAGE")
names(log) <- columnNames
log <- log[log$RECID!="",]
log <- log[c(-1,-2),]
#log <- log[log$SEVERITY=="ERROR"|log$SEVERITY=="FATAL",]
log <- data.frame(lapply(log,factor))

Sys.setlocale("LC_TIME", "english")
log$EVENT_TIME<- strptime(log$EVENT_TIME, "%Y-%m-%d-%H.%M.%OS")


#write.table(log, file=fileOut, sep="\t", row.names=FALSE, quote=FALSE)
#write.table(log, file=fileOutTable, sep="\t", row.names=FALSE, quote=FALSE)
