#Replace file, xmlFile as required
#file <- "C:\\Users\\capelastegui\\workspace\\OFP\\Santander-1\\1-Data\\Logs\\LogicalEntity\\WEB-IHS\\SANPARTICULARES\\bvsalwparp10-adm.wt.santander.corp-02\\access_1.log"

xmlFile <- "C:\\Users\\capelastegui\\workspace\\OFP\\Santander-1\\1-Data\\Logs\\0-log-tables\\tables\\table-checkpointfw1-san_internet_ms-traffic-drop.xml"

library(XML)
library(gsubfn)
x <-  xmlTreeParse(xmlFile)
r <- xmlRoot(x)
l<- xmlSApply(r[["columns"]], xmlAttrs)
#regex <- "(.{15}) (\\S+) (\\S+) ([^:]+):\\s+()time=(.{24})\\s+action=(\\w*) orig=(\\S*) i/f_dir=(\\S*) i/f_name=(\\S*) has_accounting=(\\S+) (?:product=(.{18})(?:.{152}))rule=(\\S+)\\s\\S*\\ssrc=(?:\\S+_)?(\\d+\\.\\d+\\.\\d+\\.\\d+)\\s(?:s_port=(\\S+)\\s)?dst=(?:\\S+_)?(\\d+\\.\\d+\\.\\d+\\.\\d+)\\s(?:service=(\\S+)\\s)?proto=(\\S+)\\s?"

regex <- "(.{15}) (\\S+) (\\S+) ([^:]+):\\s+(time=(.{24})\\s+action=(\\w*) orig=(\\S*) i/f_dir=(\\S*) i/f_name=(\\S*) has_accounting=(\\S+) (?:(?:product=(.{18})(?:.{152}))rule=(\\S+)\\s\\S*\\ssrc=(?:\\S+_)?(\\d+\\.\\d+\\.\\d+\\.\\d+)\\ss_port=(\\S+)\\sdst=(?:\\S+_)?(\\d+\\.\\d+\\.\\d+\\.\\d+)\\sservice=(\\S+)\\sproto=(\\S+)\\s?))"


#Switch lines to read full log or summary
#lines <- readLines(file, 10)
lines <- readLines(file, 10000) #takes 1.5 minutes with 10k lines!
#lines <- readLines(file)
log<- data.frame(strapplyc(lines, regex, simplify = "rbind"))


myF <- function(x, name="name")
{x[[name]]}
vapply(l,myF, "", USE.NAMES=FALSE)

myF <- function(x, name="name") {if(name%in%names(x)) {x[[name]]} else "NA"}

#get names, groups, froms, types
froms <- vapply(l,myF, "",  "from", USE.NAMES=FALSE)
groups <- vapply(l,myF, "",  "group", USE.NAMES=FALSE)
names <- vapply(l,myF, "", "name", USE.NAMES=FALSE)
types <- vapply(l,myF, "", "parser", USE.NAMES=FALSE)

indices <- as.numeric(groups[froms=="log"])
indicesB <- indices[indices>0]
line1 <- log[1,]


line1a <- line1[indicesB]
names(line1a) <- names[froms=="log"][indices>0]

log <- log[indicesB]
names(log) <- names[froms=="log"][indices>0]

#Column procesing
#log$http_response_size <- as.numeric(log$http_response_size)
names(log)[1] <- "generated_date"
#Date
Sys.setlocale("LC_TIME", "english")
event_date2 <- strptime(log$event_date, "%b %d %H:%M:%S")
generated_date2 <- strptime(log$generated_date, "%a %b %d %H:%M:%S %Y")

log$event_date <- event_date2
log$generated_date <- generated_date2
library(plyr)
log$program<-revalue(log$program,c("Santander.Santander.SecuritySystem.Firewall.production.CheckpointFW1.SAN_Internet_MS.traffic"="FW.SAN_Internet"))
log <- log[,-2]