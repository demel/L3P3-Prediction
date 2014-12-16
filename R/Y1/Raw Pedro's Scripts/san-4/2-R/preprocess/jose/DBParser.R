dbparser <- function(dbfile){
  library(stringr)
  
  file<- dbfile
  lines<- readLines(file)
  
  #rm(database)
  database<-list()
  end <-FALSE
  vectorLength <- length(lines)/10
  
  DATE<-character(vectorLength)
  UNKNOWN<-character(vectorLength)
  LEVEL<-character(vectorLength)
  PID<-character(vectorLength)
  TID<-character(vectorLength)
  PROC<-character(vectorLength)
  INSTANCE<-character(vectorLength)
  NODE<-character(vectorLength)
  DB<-character(vectorLength)
  APPHDL<-character(vectorLength)
  APPID<-character(vectorLength)
  AUTHID<-character(vectorLength)
  EDUID<-character(vectorLength)
  EDUNAME<-character(vectorLength)
  FUNCTION<-character(vectorLength)
  MESSAGE <- character(vectorLength)
  
  
  for (j in 1:round(length(lines)/10,digits=0))
    #for (j in 1:400)  
    
  {
    i<-(j*10)-9
    #print(c("ronda: ",j," linea: ",i,substr(lines[i],1,20),"archivo: ",substr(dbfile,60,nchar(dbfile))))
    while(substr(lines[i],1,4)!="2013")
    {
      i<-i+1
      #print(c("ronda: ",j," linea: ",i,substr(lines[i],1,20),"archivo: ",substr(dbfile,60,nchar(dbfile))))
      if (i+6>length(lines))
      {
        end <-TRUE
        break       
      }
    }
    if (end == TRUE) break
    #EVENT PROCESSING
    DATE[j]    <-substr(lines[i],1,19)
    UNKNOWN[j] <-substr(lines[i],32,50)
    LEVEL[j]   <-substr(lines[i],58,nchar(lines[i]))
    PID[j]     <-substr(lines[i+1],11,31)
    TID[j]     <-substr(lines[i+1],39,50)
    PROC[j]    <-substr(lines[i+1],58,nchar(lines[i+1]))
    INSTANCE[j]<-substr(lines[i+2],11,31)
    NODE [j]   <-substr(lines[i+2],39,50)
    
    if (substr(lines[i+2],51,52)=="DB")
      DB[j]      <-substr(lines[i+2],58,nchar(lines[i+2]))
    else
      DB[j]      <-""
    
    if (substr(lines[i+3],1,6)=="APPHDL")
    {
      APPHDL[j]  <-substr(lines[i+3],11,31)
      APPID [j]  <-substr(lines[i+3],39,nchar(lines[i+3]))
      AUTHID[j]  <-substr(lines[i+4],11,nchar(lines[i+4]))
      EDUID [j]  <-substr(lines[i+5],11,31)
      EDUNAME[j] <-substr(lines[i+5],41,nchar(lines[i+5]))
      FUNCTION[j]<-substr(lines[i+6],11,nchar(lines[i+6]))
      
      MESSAGE[j] <-""
    }
    else if (substr(lines[i+3],1,5)=="EDUID")
    {
      EDUID [j]  <-substr(lines[i+3],11,11)
      FUNCTION[j]<-substr(lines[i+4],11,nchar(lines[i+4]))
      tempMessage <- substr(lines[i+5],11,nchar(lines[i+5]))
      i <- i+1
      while(substr(lines[i+5],1,1)==" ")
      {
        tempMessage <- paste(tempMessage,substr(lines[i+5],11,nchar(lines[i+5])),sep="")
        i<-i+1
      }
      MESSAGE[j]<-str_trim(tempMessage, side="both")
    }
   #END OF EVENT PROCESSING 
  }
  
  
  
  DATE   <-strptime(DATE,"%Y-%m-%d-%H.%M.%S")
  UNKNOWN <-factor(str_trim(UNKNOWN,side="right"))
  LEVEL  <-factor(LEVEL)
  PID<- as.numeric(str_trim(PID,side="right"))
  TID<- as.numeric(str_trim(TID,side="right"))
  PROC <-factor(str_trim(PROC,side="right"))
  INSTANCE <-factor(str_trim(INSTANCE,side="right"))
  NODE <-factor(str_trim(NODE,side="right"))
  DB <-factor(str_trim(DB,side="right"))
  APPHDL <-factor(str_trim(APPHDL,side="right"))
  APPID <-factor(str_trim(APPID,side="right"))
  AUTHID <-factor(str_trim(AUTHID,side="right"))
  EDUID<- as.numeric(str_trim(EDUID,side="right"))
  EDUNAME <-factor(str_trim(EDUNAME,side="right"))
  FUNCTION <-factor(str_trim(FUNCTION,side="right"))
  MESSAGE <- factor(str_trim(MESSAGE,side="right"))
  
  
  database <-data.frame(DATE,UNKNOWN,LEVEL,PID,TID,PROC,INSTANCE,NODE,DB,APPHDL,APPID,AUTHID,EDUID,EDUNAME,FUNCTION,MESSAGE)
}


getdb <-function()
{
  fileNames <- c('2013-08_iesmpar1_db2diag.log','2013-08_iesmpar2_db2diag.log','2013-08_iesmpar3_db2diag.log','2013-08_iesspar1_db2diag.log','2013-08_iesspar2_db2diag.log')
  filePath <- 'C:\\Datos Santander\\Datos 2013-02-10\\Datos Particulares\\'
  
  db1 <-dbparser(paste(filePath,fileNames[1],sep=''))
  db2 <-dbparser(paste(filePath,fileNames[2],sep=''))
  db3 <-dbparser(paste(filePath,fileNames[3],sep=''))
  db4 <-dbparser(paste(filePath,fileNames[4],sep=''))
  db5 <-dbparser(paste(filePath,fileNames[5],sep=''))
  
  db <-rbind(db1,db2)
  db <-rbind(db,db3)
  db <-rbind(db,db4)
  db <-rbind(db,db5)
  
  rm(db1)
  rm(db2)
  rm(db3)
  rm(db4)
  rm(db5)
  
  db <- db[order(db$DATE),]
}


db <- getdb()