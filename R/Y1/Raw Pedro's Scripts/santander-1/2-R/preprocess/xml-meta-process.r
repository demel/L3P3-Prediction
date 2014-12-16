xmlFile <- "C:\\Users\\capelastegui\\workspace\\OFP\\Santander\\tables\\schemas\\statistics.xml"

library(gsubfn)
library(xml)
x <-  xmlTreeParse(xmlFile)
r <- xmlRoot(x)

myF <- function (x)
{
vapply(xmlChildren(x),xmlValue, c(""), USE.NAMES="FALSE")
}

tableNames <- vapply(xmlChildren(r[[4]][[1]][[1]][[1]]), xmlValue, c(""),USE.NAMES="FALSE")
tableSource <- unlist(xmlSApply(r[[3]][[9]][[1]], myF), use.names="FALSE")
columnNames <- unlist(xmlSApply(r[[4]][[7]][[1]], myF), use.names="FALSE")
columnNamesAttrs <- (xmlSApply(r[[4]][[7]][[1]], xmlAttrs))
columnNamesTotal<- vapply(columnNamesAttrs,function(x, name="name")
{x[[name]]}, "","total", USE.NAMES=FALSE)

columnFrom <- unlist(xmlSApply(r[[4]][[5]][[1]][[1]]), xmlValue, c(""),USE.NAMES="FALSE")

categorySchema <- unlist(xmlSApply(r[[4]][[3]][[1]][[1]]), xmlValue, c(""),USE.NAMES="FALSE")

categoryName <- unlist(xmlSApply(r[[4]][[2]][[1]][[1]]), xmlValue, c(""),USE.NAMES="FALSE")







columnNames <- vapply(xmlChildren(r[[4]][[7]][[1]][[1]]), xmlValue, c(""),USE.NAMES="FALSE")

columnNamesTotal <- vapply(xmlChildren(r[[4]][[7]][[1]][[1]]), xmlAttr, c(""), "total", USE.NAMES="FALSE")

columnFrom <- vapply(xmlChildren(r[[4]][[5]][[1]][[1]]), xmlValue, c(""),USE.NAMES="FALSE")

categorySchema <- vapply(xmlChildren(r[[4]][[3]][[1]][[1]]), xmlValue, c(""),USE.NAMES="FALSE")

categoryName <- vapply(xmlChildren(r[[4]][[2]][[1]][[1]]), xmlValue, c(""),USE.NAMES="FALSE")