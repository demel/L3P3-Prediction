dataPath <-"C:\\Users\\capelastegui\\workspace\\OFP\\Santander-4\\1-Data\\2-test\\"
fileName <- "DEADIC.LOGTEC_PART.del"
#fileIn <- paste(dataPath, fileName, sep="")
fileIn <- file(paste(dataPath, fileName, sep=""), "rb")
lines <- readLines(fileIn, 1000)
fileNameOut <- "DEADIC.LOGTEC_PART-subset.del"
write(lines, paste(dataPath, fileNameOut, sep=""))