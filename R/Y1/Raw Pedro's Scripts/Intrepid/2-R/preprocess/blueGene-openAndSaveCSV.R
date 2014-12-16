library(gsubfn)

fixedSplit <- function(x, start, end){
  strings <- substring(x, start, end)            # Fracture string by widths
  strings <- gsub("[[:space:]]+$", "", strings)  # Remove white from right
  strings <- gsub("^[[:space:]]+", "", strings)  # Remove white from left
  strings <- paste(strings)        # Make delimited string
  
  return(strings)
}

fileIn <- "C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\0-raw\\Intrepid_RAS_0901_0908_scrubbed"
fileOut <- "C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\1-processed\\log-large.csv"


lines <- readLines(fileIn, 1000000)


startIndex=c(1,13,24,41,62,103,112,139,150,162,174,207,272,292,324)
endIndex=c(i[-1]-1,4000)

system.time(log<- data.frame(t(sapply(lines,fixedSplit,start=startIndex,end=endIndex, USE.NAMES=FALSE))))

namesLine <- lines[3]
columnNames <- fixedSplit(namesLine,startIndex, endIndex)
names(log) <- columnNames
log <- log[log$RECID!="",]
log<- log[c(-1,-2),]
log<- log[log$SEVERITY=="ERROR"|log$SEVERITY=="FATAL"|log$SEVERITY=="INFO"|log$SEVERITY=="WARN",]



write.table(log, file=fileOut, sep="\t", row.names=FALSE, quote=FALSE)
