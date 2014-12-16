#Replace file, xmlFile as required
file <- "C:\\Users\\capelastegui\\workspace\\OFP\\Santander-2\\1-Data\\0-raw\\OPERPAR1_SystemOut_13.05.20_09.04.50.log"


library(gsubfn)
#Switch lines to read full log or summary
#lines <- readLines(file, 20)
lines <- readLines(file)

#Identify starts of headers
startEnvIndex <- grep(" Start Display Current Environment ",lines)

#Each header is 11 lines long
#Get index of all header lines
myF <- function(x)
{x:(x+10)}
envIndex <- c(sapply(startEnvIndex,myF, simplify="array"))

#Skip the following types of lines
#	- Lines starting with tab
#	- Lines starting with space
#	- Header lines
notread <- sort(c(grep("^\t",lines),grep("^ ",lines), envIndex))
lines2 <- lines[setdiff(1:length(lines),notread)]

# Regex for this table
#regex <- "(.{28}) (.{8}) (\S*)\s*(\w)\s*(.*)\r"
regex <- "^(.{28}) (.{8}) (\\S*)\\s*(\\w)\\s*(.*)"


#Create data frame, apply regex
log<- data.frame(strapplyc(lines2, regex, simplify = "rbind"))
names(log) <- c("date","code","source","severity","message")

#levels(factors) <- c("audit","error", "info", "sysOut", "warning")
levels(log$severity) <- c("2","4", "1", "0", "3")
log$sevNum <- as.numeric(as.character(log$severity))

date2 <- gsub("(.{18}):","\\1.", log$date)
date3 <- strptime(date2, "[%d/%m/%y %H:%M:%OS")
log$dateNum <- date3

log <- log[c("dateNum", "code", "source", "sevNum", "message")]
#plot(log2dateNum,log$sevNum)
