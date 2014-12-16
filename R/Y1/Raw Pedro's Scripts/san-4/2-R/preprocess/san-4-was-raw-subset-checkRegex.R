fileOut <- paste(dataPath, "san-4-was-raw-subset-wronglines.log", sep="")
indices<-setdiff(1:length(lines),grep(regex,lines))
write(lines[indices], fileOut)