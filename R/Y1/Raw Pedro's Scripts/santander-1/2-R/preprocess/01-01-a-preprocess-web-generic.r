#Replace file, xmlFile as required
#file <- "C:\\Users\\capelastegui\\workspace\\OFP\\Santander-1\\1-Data\\Logs\\LogicalEntity\\WEB-IHS\\SANPARTICULARES\\bvsalwparp10-adm.wt.santander.corp-02\\access_1.log"

xmlFile <- "C:\\Users\\capelastegui\\workspace\\OFP\\Santander-1\\1-Data\\Logs\\0-log-tables\\tables\\table-ihs-santander-sanparticulares-access.xml"

library(XML)
library(gsubfn)
x <-  xmlTreeParse(xmlFile)
r <- xmlRoot(x)
l<- xmlSApply(r[["columns"]], xmlAttrs)
regex <- xmlValue(r[["log"]])
#Switch lines to read full log or summary
#lines <- readLines(file, 10)
lines <- readLines(file)
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

logA <- log[indicesB]
names(logA) <- names[froms=="log"][indices>0]

#Column procesing
logA$http_response_size <- as.numeric(logA$http_response_size)
#Date
Sys.setlocale("LC_TIME", "english")
event_date2 <- strptime(logA$event_date, "%b %d %H:%M:%S")
generated_date2 <- strptime(logA$generated_date, "%d/%b/%Y:%H:%M:%S")

logA$event_date <- event_date2
logA$generated_date <- generated_date2
