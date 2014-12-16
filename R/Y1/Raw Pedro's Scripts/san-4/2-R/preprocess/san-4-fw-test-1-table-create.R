dataPath <-"C:\\Users\\capelastegui\\workspace\\OFP\\Santander-4\\1-Data\\2-test\\"
fileIn <- paste(dataPath, "traffic-drop.log", sep="")
fileOut <- paste(dataPath, "traffic-drop-table.log", sep="")


#Replace file, regEx file as required
#file <- "..."
regex <- "(.{15}) (\\S+) (\\S+) ([^:]+):\\s+time=(.{24})\\s+action=(\\w*) orig=(\\S*) i/f_dir=(\\S*) (.+)" 
#Switch lines to read full log or summary
lines <- readLines(fileIn, 1000)
#lines <- readLines(fileIn)
log<- dfFromRegex(lines, regex)

write.table(log, fileOut)
