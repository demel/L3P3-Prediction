#This script aggregates all different date folders for data into a single one
# In addition, it performs the following operations not needed for the totta dataset
# - Multi-line events:
## Event files use LF instead of CRLF for line feed
## Also, messages can include line feeds and unescaped quotes

library(plyr)
library(gsubfn)
library(doParallel)
library(foreach)
library(lubridate)
library(dplyr)
library(resourceParser)

#baseRPath <- "C:/Users/capelastegui/workspace/OFP/SCF/SCF-1/2-R/preprocess" 
baseRPath  <- "C:/Users/JM/Documents/GitHub/l3p3/Parsing/Totta"
resourceParserPath <- file.path(baseRPath,"totta-1-0-raw-2-resourceParser.R")

# File path variables. Modify as needed.   
baseDataPath <- "C:/Users/JM/Documents/Prueba-Santander/scf"
rawFolder <- file.path(baseDataPath,"1-Raw/1-Current")
aggrDateFolder <- file.path(baseDataPath,"2-AggrDateFolder")
aggrDateFile <- file.path(baseDataPath,"3-AggrDateFile")
eventTableFile <- file.path(aggrDateFile,"eventTable.txt")
eventRawFile <- file.path(aggrDateFile,"eventRaw.txt")

source(resourceParserPath)
# For some reason related to scope, we need to redeclare this here, or parallel calls from plyr will crash

# Function to take totta log/event files from a single folder tree, parse them, and save the aggregated results
aggregateDataFiles <- function(doLogs=FALSE, doEvents=TRUE, saveToFile=FALSE)
{

  
  
  # Generate filesTable, a dataframe listing log files, and their associated nodes and resources
  files<-list.files(aggrDateFolder,recursive=TRUE, include.dirs=FALSE,full.names=TRUE)
  filesCropped<-gsub(aggrDateFolder, "", files)
  # regex to split node, resource type, and date component of file name (has form node_type_ddmmyy.csv)
  regex <- "(.*)_([^_]*)_[^_]*$"

  filesTable<-data.frame(strapplyc(filesCropped,regex, simplify = "rbind"))
  names(filesTable)<-c("node","resource")
  filesTable$file<-files[grep(regex,files)]
  
  #for debugging purposes, list of files with names not matching expected pattern
  wrongNameFiles<-files[-grep(regex,files)]
  
  # Generate rawDataList, eventDataList
  ## rawDataList: list tree with raw data from all log files
  ## eventDataList: list tree with raw data from all event files
  
  # function to apply readlines over an array of file names
  # return list with file contents; names of list elements are file names
  applyReadLines<-function(l)
  {
    myReadLines<-function(file)
    {
      lines<-readLines(file)
      #remove content if single, empty line
      #if(length(lines)==1&nchar(lines[1]==0)){lines<-NULL}
      #lines
    }
    tmpList<-alply(l,1,.fun=myReadLines)
    names(tmpList)<-l
    tmpList
  }
  # function to cut data frame by resource type, get fileName columns, apply applyReadLines over them
  #return list with one sublist per resource type; 
  ## each sublist element has name=fileName, value=fileContent
  getNodeDataFiles<-function(df){dlply(df,.(resource),.fun=function(df){applyReadLines(df$file)})}
  
  # cut data frame by node name, apply getNodeDataFiles for each node
  #  return list with one sublist per node
  ##  each sublist element is a subsublist with name=resource, value=(list of file contents)
  rawDataList <- dlply(filesTable,.(node),.fun=getNodeDataFiles)
  #remove event data from rawDataList, add to eventDataList
  #DELETEME
  #quickStr(rawDataList)
  eventDataList <- llply(rawDataList,.fun=function(l){l[names(l)=="ev"]})
  rawDataList <- llply(rawDataList,.fun=function(l){l[names(l)!="ev"]})
  
  #Elements that fail from rawDataList when applying resourceParser: 29, 30,31. We take them out temporarily
  rawDataList<-c(rawDataList[1:28],rawDataList[32:44])
  
  
  #some nodes can have empty event lists, which make eventParser crash
  #remove those empty event lists
  # structure of eventDataList
  ## node:list()
  ## - ev:list()
  ## --file: char(n)
  #remove files with null file content
  # eventDataList<-laply(eventDataList,function(l)
  #   {llply(l,function(l2)
  #     {l2[nchar(l2)>0]})})
  #remove nodes without events from eventDataList
  eventDataList <-eventDataList[laply(eventDataList,length)>0]
  
  #SCF: code to deal with multi-line event messages
  
  #
    
  # Generate logTable and eventTable, dataframes with parsed data from log/event files
    
  logTable<-list()
  eventTable<-list()
  eventRawTable<-character(0)
  parseErrorTable<-list()
  emptyLogTable<-list()
  debugTable<-list()
  
  if(doLogs)
  {# apply resource parser over log file groups for each (node, resource) pair. Loop parallelized for performance.
    #set up parallel runtime configuration
    cl<-makeCluster(8)
    registerDoParallel(cl)
    #parOptions<-list(.packages = c("plyr","gsubfn","stringr"),.export = c("resourceParser","csvToDf", "tmpEventParser") )
    #exporting package resourceparser may fail if totta.resourceparser is different from scf.resp
    parOptions<-list(.packages = c("plyr","gsubfn","stringr", "resourceParser","lubridate"))
    
    #csvToDf<-csvToDf
    logTable<-llply (rawDataList,
                     .fun=function(node) 
                     {llply(node,function(resource)  
                     {ldply(resource,resourceParser)})},
                     .parallel=TRUE,.paropts=parOptions)
    
    stopCluster(cl)  
  }
  
  if(doEvents)
  {
    # load eventParser function into scope of this function
    
    # function to apply eventParser over all files of a given node.
#     applyEventParser <- function(node)
#     { 
#       tmpNames<-laply(node,names)
#       nonEmptyIndices<-laply(node,function(l){nchar(l)>0})
#       tmpEventSubtable<-ldply(tmpNames[nonEmptyIndices],eventParser)
#     }
    
    applyEventParser <- function(node)
    { 
      node1<-node[[1]]
      nonEmptyIndices<-nchar(node1)>0
      node1<-node1[nonEmptyIndices]
      if(length(node1)<1)(return(data.frame()))
      ldply(node1,eventParser)
    }
    
    applyGetRawEvents<- function(node)
    { 
      node1<-node[[1]]
      tmpNames<-names(node1)
      nonEmptyIndices<-nchar(node1)>0
      node1<-node1[nonEmptyIndices]
      tmpRawEvents<-character(0)
      if(length(node1)<1)(return(tmpRawEvents))
      for (i in 1:length(node1))
      {tmpRawEvents<-c(tmpRawEvents,names(node1)[i],node1[[i]])}
      tmpRawEvents
    }
    # apply event parser over all event files for each node. Loop parallelized for performance.
    #eventTableList<-llply (eventDataList ,.fun=applyEventParser,.parallel=TRUE,.paropts=parOptions )
    eventTableList<-llply (eventDataList ,.fun=applyEventParser)
    eventRawTable<-llply (eventDataList ,.fun=applyGetRawEvents)
    eventRawTable<-c(eventRawTable,recursive=TRUE)
    # remove empty list elements, or the following Reduce+merge fails (merge won't work with empty dataframes)
    emptyEventFileList<-eventTableList[laply(eventTableList,nrow)<=0]
    eventTableList<-eventTableList[laply(eventTableList,nrow)>0]
    # combine list into single dataframe
    eventTable<-Reduce( function(d1,d2) {merge(d1,d2, all.x=TRUE, all.y=TRUE)}, eventTableList)
    if(!is.null(eventTable$parseError))
    {parseErrorTable<-eventTable$parseError[!is.na(eventTable$parseError)]
     eventTable<-eventTable[is.na(eventTable$parseError),]
     eventTable$parseError<-NULL}
    if(!is.null(eventTable$emptyLog))
    {emptyLogTable<-eventTable$emptyLog[!is.na(eventTable$emptyLog)]
     eventTable<-eventTable[is.na(eventTable$emptyLog),]
     eventTable$emptyLog<-NULL}
    eventTable$.id<-NULL
    # function to factor a character vector, converting ""'s and NAs into "BLANK" levels
    factorNoBlanks <- function (f)
    {
      f[f==""|is.na(f)]<-"BLANK"
      factor(f)
    }
    eventColumnClasses<-laply(eventTable,function(column){class(column)[1]})
    eventTable[eventColumnClasses=="character"]<-llply(eventTable[eventColumnClasses=="character"],factorNoBlanks)
    # rename table labels
    eventTable<-rename(eventTable, c("Created.On"="date", "Name"="node","Event"="msg","Event.Type"="type",
                                     "Severity"="severity", "Created.By"="createdBy","Cleared.On"="clearedOn",
                                     "Cleared.By"="clearedBy"),warn_missing = FALSE)
    # reorder by event date
    #eventTable<-eventTable[order(eventTable$date),]
    eventTable<-arrange(eventTable,date)
    # rearrange table fields
    knownFields<-c("date", "severity", "type", "node", "createdBy","clearedOn","clearedBy","msg")
    unknownFields<-names(eventTable)[!names(eventTable) %in% knownFields]
    eventTable<-eventTable[c(knownFields,unknownFields)]
    #for file writing, remove CRLF from event messages
    eventTable$msg <-gsub("\n", " CRLF ", eventTable$msg)
  } # end if(doEvents)
  
  # to browse logTable, try str(logTable,3,vec.len=3,list.len=3)
  
  #all file writing here
  if(saveToFile){
    #create folder tree in target path
    folderList<-list.dirs(rawFolder, recursive=FALSE)
    folders<-character(0)
    for(f in folderList)
    {folders<- unique(c(folders,list.dirs(f,recursive=TRUE,full.names=FALSE)))}
    folders<-file.path(aggrDateFile,folders)
    tmp<-lapply(folders,dir.create,showWarnings=FALSE)
    
    #write data to files
    
    for (node in names(rawDataList))
    {
      cpuFile<-file.path(aggrDateFile,paste(node,"_cpu.csv",sep=""))
      memFile<-file.path(aggrDateFile,paste(node,"_mem.csv",sep=""))
      hdFile<-file.path(aggrDateFile,paste(node,"_hd.csv",sep=""))
      intFile<-file.path(aggrDateFile,paste(node,"_int.csv",sep=""))
      evFile<-file.path(aggrDateFile,paste(node,"_ev.csv",sep=""))
      
      cpu<-logTable[[node]]$cpu
      mem<-logTable[[node]]$mem
      hd <-logTable[[node]]$hd
      int<-logTable[[node]]$int
      ev <-eventTable[[node]]$ev
      
      #debugTable collects write results
      debugTable[[node]]<-list()
      if(!is.null(cpu)&&dim(cpu)[2]>0) {write.table(cpu,cpuFile)} else
      {debugTable[[node]]$cpu<-NA}
      if(!is.null(mem)&&dim(mem)[2]>0) {write.table(mem,memFile)} else
      {debugTable[[node]]$mem<-NA}
      if(!is.null(hd)&&dim(hd)[2]>0) {write.table(hd,hdFile)} else
      {debugTable[[node]]$hd<-NA}
      if(!is.null(int)&&dim(int)[2]>0) {write.table(int,intFile)} else
      {debugTable[[node]]$int<-NA}
    }
    
    write.table(eventTable,eventTableFile)
    writeLines(eventRawTable,eventRawFile)
  }
  #debugTable$wrongNameFiles<-wrongNameFiles
  #debugTable$parseError<-parseErrorTable
  resultList<-list(logTable=logTable,eventTable=eventTable,debugTable=debugTable,emptyLog=emptyLogTable,wrongNameFiles=wrongNameFiles,parseError=parseErrorTable)
} # end of function aggregateDataFiles

#resultList<-aggregateDataFiles(TRUE,TRUE,TRUE)