library(gsubfn)
fileTmp <- "C:\\Users\\capelastegui\\workspace\\OFP\\tmp\\lines-tmp.txt"
fileTmpRegex <- "C:\\Users\\capelastegui\\workspace\\OFP\\tmp\\regex-tmp.txt"

checkRegex <- function(file=fileTmp, regexFile=fileTmpRegex)
{
  tmpregex <- readLines(regexFile,1)
  tmplines <- readLines(file, 1)
  cat ("Regex: ",tmpregex, "\n")
  cat("Text: ", tmplines, "\n")
  strapplyc(tmplines, tmpregex, simplify = "rbind")}

checkRegex()