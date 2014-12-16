fileIn <- "C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\1-processed\\intrepid-log-table"
fileInError <- "C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\1-processed\\intrepid-log-error-table.csv"


  f <- file(fileInError, "rb")
  logError2 <- read.csv(f,sep="\t")

#system.time(miLog<- read.table(f, sep="\t"))
#system.time(miLog<- read.table(fileIn, sep="\t"))
#system.time(lines<-gsub("\32%", " ", lines))
system.time(miLog<-data.frame(lines))
#mylines <- readLines(fileIn)