library(stringr)
library(gsubfn)
library(plyr)


csvToDf <- function (csvLines, skip=0)
{
  #regex "([^"]*)" identifies blocks delimited by "
  regex <- "\"([^\\\"]*)\""
  if(skip>0)csvLines<-csvLines[-skip]
  csvLines<-csvLines[csvLines!=""]
  if(length(csvLines)<1) {return(data.frame())}
  df<-data.frame(t(strapplyc(csvLines,regex,simplify=TRUE)))
  names(df)<-laply(df[1,],as.character)
  df<-df[-1,]
  return(df)
}

resourceParser  <- function(resource)
{
  if(is.null(resource)|length(resource)<2){return(data.frame())}
  
  breaks<-cut(1:length(resource),which(nchar(resource)==0))
  splitResource<-split(resource,breaks)
  dfList<-llply(splitResource, csvToDf, skip=1)
  dfList<-dfList[-1]
  #check dfList includes data frames, filter empty ones
  dfList<-dfList[laply(dfList,is.data.frame)]
  dfListDims<-laply(dfList,dim)
  if(!is.null(dim(dfListDims))) 
  {dfList<-dfList[dfListDims[,1]!=0]}   else  
  {return (data.frame())}
  #{dfList<-dfList[0]}
  
  #llply(dfList,head)
  
  #for partition column, remove all but last word from column name
  prevNameList<-list(0)
  for(i in 1:length(dfList))
  {
    #name<-tail(unlist(strsplit(names(dfList[[i]])," ")),1)
    nameList<-laply(strsplit(names(dfList[[i]][-1])," "),tail,n=1)
    for(name in nameList)
    {
      if(any(prevNameList==name)){name<-paste(name,i,sep="_")}
      prevNameList[[name]]<-name
    }
    names(dfList[[i]])[-1]<-name
    names(dfList[[i]])[1]<-"date"
  }
  
  #join- plyr function to merge 2 dataframes
  #Reduce - function to apply binary function (join) to vector
  df<-Reduce(function(d1,d2){join(d1,d2,by="date")},dfList)
  
  #df$date<-strptime(df$date,format="%m/%d/%Y %H:%M")
  df$date <- mdy_hm(df$date)
  for(i in 2:length(df))
  {
    fac<-df[,i]
    levels(fac)<-gsub(" ","",levels(fac))
    levels(fac)<-gsub("Percent","",levels(fac))
    levels(fac)<-gsub("<","",levels(fac))
    df[,i]<-as.numeric(as.character(fac))
  }
  return(df)
}

# Parses a totta event log file. Returns a table with non-factored character vectors.
eventParser  <- function(file_name)
{
  #events<-read.csv(file_name,stringsAsFactors=FALSE)
  events<-read.table(file_name, header=TRUE, sep=",", colClasses="character")
  #If not enough columns, return (should give error)
  ##TODO: find a way to send an error here!
  #if(ncol(events)<4){return(NULL)}
  if(ncol(events)<4){return(data.frame(parseError=file_name))}
  if(nrow(events)<1){return(data.frame(emptyLog=file_name))}
  Sys.setlocale("LC_TIME","Spanish_Spain.1252")
  #date<-as.POSIXct(strptime(events$Created.On,format="%d-%b-%Y %H:%M:%S"))
  #dateClear<-as.POSIXct(strptime(events$Cleared.On,format="%d-%b-%Y %H:%M:%S"))
  date<-ymd_hms(events$Created.On)
  dateClear<-ymd_hms(events$Cleared.On)

  #check if valid dates
  if(length(date)==nrow(events) || any(is.na(date))==FALSE)
    {events$Created.On<-date} else
    {return(data.frame(parseError=file_name))}
  if(length(dateClear)==nrow(events)){events$Cleared.On<-dateClear}
  if(!is.null(events$Event.Type))  {events$Event.Type<-  paste("t.",(events$Event.Type),sep="")}
  return(events)
}
