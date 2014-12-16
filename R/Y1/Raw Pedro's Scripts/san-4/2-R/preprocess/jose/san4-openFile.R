dataPath <-"C:\\Users\\capelastegui\\workspace\\OFP\\Santander-4\\1-Data\\2-test\\"
fileName <- "DEADIC.LOGTEC_PART.del"
fileIn <- paste(dataPath, fileName, sep="")
lines <- readLines(fileIn, 10000)
fileName <- "san-4-was-raw-subset.log"
write(lines, paste(dataPath, fileName, sep=""))