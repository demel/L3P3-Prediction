library(gsubfn)


fileIn <- "C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\0-raw\\Intrepid_RAS_0901_0908_scrubbed"
fileOut <- "C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\1-processed\\log-intrepid.txt"


lines <- readLines(fileIn)

write(lines, file=fileOut)
