fileIn <- "C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\0-raw\\Intrepid_RAS_0901_0908_scrubbed"
fileOut <- "C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\0-raw\\log-intrepid-clean"

sReadLines <- function(fnam) {
  f <- file(fnam, "rb")
  res <- readLines(f)
  close(f)
  res
}

lines <- sReadLines(fileIn)
lines<- gsub("\32", " ", lines)
#lines_backup <- lines
lines<-lines[which(lines!="")]
linesInd <- which(nchar(lines)<283)
linesIndMinusOne <- linesInd-1

for(i in linesInd)
{
  lines[i-1]<-paste(lines[i-1], lines[i])
}
lines<-lines[-linesInd]


write(lines, fileOut)


# write(lines[1:1000000],file=(con<-file("C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\0-raw\\log-intrepid-raw-00", "w", encoding = "UTF-8"))); close(con)
# write(lines[1000001:2000000],file=(con<-file("C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\0-raw\\log-intrepid-raw-01", "w", encoding = "UTF-8"))); close(con)
# write(lines[2000001:3000000],file=(con<-file(,"C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\0-raw\\log-intrepid-raw-02", "w", encoding = "UTF-8"))); close(con)
# write(lines[3000001:4000000],file=(con<-file("C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\0-raw\\log-intrepid-raw-03", "w", encoding = "UTF-8"))); close(con)
# write(lines[4000001:5000000],file=(con<-file("C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\0-raw\\log-intrepid-raw-04", "w", encoding = "UTF-8"))); close(con)
# write(lines[5000001:6000000],file=(con<-file("C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\0-raw\\log-intrepid-raw-05", "w", encoding = "UTF-8"))); close(con)
# write(lines[6000001:7000000],file=(con<-file("C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\0-raw\\log-intrepid-raw-06", "w", encoding = "UTF-8"))); close(con)
# write(lines[7000001:8000000],file=(con<-file("C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\0-raw\\log-intrepid-raw-07", "w", encoding = "UTF-8"))); close(con)
# write(lines[8000001:9000000],file=(con<-file("C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\0-raw\\log-intrepid-raw-08", "w", encoding = "UTF-8"))); close(con)
# write(lines[9000001:10000000],file=(con<-file("C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\0-raw\\log-intrepid-raw-09", "w", encoding = "UTF-8"))); close(con)
# write(lines[10000001:11000000],file = (con <- file("C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\0-raw\\log-intrepid-raw-10", "w", encoding = "UTF-8"))); close(con)
# write(lines[11000001:12000000],file = (con <- file("C:\\Users\\capelastegui\\workspace\\OFP\\Other\\Public OFP Data\\Intrepid\\1-Data\\0-raw\\log-intrepid-raw-11", "w", encoding = "UTF-8"))); close(con)
                                    
                                    