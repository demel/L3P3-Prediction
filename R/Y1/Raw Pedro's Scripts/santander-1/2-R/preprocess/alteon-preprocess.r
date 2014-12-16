#Replace file, xmlFile as required
file <- "C:\\Users\\capelastegui\\workspace\\OFP\\Santander\\Logs_2\\CommunicationSystems\\LoadBalancer\\172.21.211.6-01\\alteon.log"
xmlFile <- "C:\\Users\\capelastegui\\workspace\\OFP\\Santander\\tables\\tables\\table-loadbalancer-alteon-santander.xml"

library(gsubfn)
x <-  xmlTreeParse(xmlFile)
r <- xmlRoot(x)
l<- xmlSApply(r[["columns"]], xmlAttrs)
regex <- xmlValue(r[["log"]])
lines <- readLines(file, 10)
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
