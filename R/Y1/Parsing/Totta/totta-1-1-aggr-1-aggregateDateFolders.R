#baseDataPath <- "C:/Users/capelastegui/workspace/OFP/Totta/Totta-1/1-Data"
#baseDataPath <- "C:/Users/capelastegui/workspace/OFP/SCF/1-Data"
baseDataPath <-  "C:/Users/JM/Documents/Prueba-Santander"

rawFolder <- file.path(baseDataPath,"1-Raw/1-Current")
aggrDateFolder <- file.path(baseDataPath,"2-AggrDateFolder")
aggrDateFile <- file.path(baseDataPath,"3-AggrDateFile") 

folderList<-list.dirs(rawFolder, recursive=FALSE)
folders<-character(0)
files<-character(0)
destFiles<-character(0)
subFolders<-character(0)

for(f in folderList)
{
  folders<- unique(c(folders,list.dirs(f,recursive=TRUE,full.names=FALSE)))
  files<-c(files,list.files(f,recursive=TRUE, include.dirs=FALSE,full.names=TRUE))
  destFiles<-tolower(c(destFiles,list.files(f,recursive=TRUE, include.dirs=FALSE,full.names=FALSE)))
  subFolders<-c(subFolders,list.files(f,recursive=TRUE, include.dirs=FALSE,full.names=FALSE))
}
#TODO: ADD LINE TO REMOVE SPACES FROM DESTFILES - apply regex
folders1<-file.path(aggrDateFolder,tolower(folders))
tmp<-lapply(folders1,dir.create,showWarnings=FALSE)
#regex to remove file name, use only file path
#ignore this line
##destFolders<-file.path(aggrDateFolder,tolower(gsub("/[^/]*$","",subFolders)))
tmp<-mapply(file.copy,files,file.path(aggrDateFolder,destFiles), overwrite=FALSE)


folders2<-file.path(aggrDateFile,folders)
tmp<-lapply(folders2,dir.create,showWarnings=FALSE)