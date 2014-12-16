library(stringr)
library(gsubfn)

dataPath <-"C:\\Users\\capelastegui\\workspace\\OFP\\Santander-4\\1-Data\\2-test\\"
fileIn <- file(paste(dataPath, "DEADIC.LOGTEC_PART.del", sep=""), "rb")
fileOut <- paste(dataPath, "san-4-was-raw-filtered.log", sep="")

regex <- "(\".{26}\"),(\".{15}\"),(|\"[^\"]*\"),(|\"[^\"]*\"),(|\"[^\"]*\"),(|\".*\"),(|\"[^\"]*\"),(|\"UNKNOWN\"|\".{17}\")$"
lines <- readLines(fileIn)
# remove incorrect line breaks
indices<-setdiff(1:length(lines),grep(regex,lines,perl=TRUE))
indicesA<-indices[indices[1:length(indices)-1]- indices[2:length(indices)] == -1]
indicesB<-indices[indices[1:length(indices)-1]- indices[2:length(indices)] == -1]+1
lines[indicesA]<-paste(lines[indicesA], lines[indicesB])
lines<-lines[-indicesB]

# remove lines without
regex2<- "BVSNWRPC201|BVSNWRPC301|BVSNWRPC216|BVSNWRPC316"
lines<-lines[grep(regex2,lines,perl=TRUE)]

write(lines, fileOutFiltered)
