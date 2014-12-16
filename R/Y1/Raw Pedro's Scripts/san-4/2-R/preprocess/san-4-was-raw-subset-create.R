dataPath <-"C:\\Users\\capelastegui\\workspace\\OFP\\Santander-4\\1-Data\\2-test\\"
fileIn <- paste(dataPath, "DEADIC.LOGTEC_PART.del", sep="")
fileOut <- paste(dataPath, "san-4-was-raw-subset.log", sep="")

f <- file(fileIn , "rb")
lines <- readLines(f, 50000)
lines<- gsub("\32", " ", lines)
write(lines, fileOut)
