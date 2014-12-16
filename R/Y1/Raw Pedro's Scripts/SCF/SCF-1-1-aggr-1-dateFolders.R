#This script aggregates all different date folders for data into a single one
# In addition, it performs the following operations not needed for the totta dataset
# - XLSX removal: Sometimes event files are in the form of empty, 8829bytes XLSX files
## This can also include XLSX files with csv extension
## Solution: Identify these files, replace them with empty .csv files



baseDataPath <- "C:/Users/capelastegui/workspace/OFP/SCF/SCF-1/1-Data"

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

#create folders to copy
folders1<-file.path(aggrDateFolder,tolower(folders))
tmp<-lapply(folders1,dir.create,showWarnings=FALSE)
folders2<-file.path(aggrDateFile,folders)
tmp<-lapply(folders2,dir.create,showWarnings=FALSE)

#code to address empty xlsx files
emptyXlsxIndices<-which(file.info(files)$size==8829)
dotXlsxIndices<-which(grepl(".xlsx",files))
nonEmptyXlsxInd<-which(!dotXlsxIndices %in% emptyXlsxIndices) #should be integer(0)
destFiles<-gsub(".xlsx",".csv", destFiles)
  #create temporary empty file
emptyFileName<-file.path(aggrDateFolder,"emptyFile.txt")
writeLines("",emptyFileName)
  #replace names of empty xlsx files with emptyfilename
files[emptyXlsxIndices]<-emptyFileName

#copy all files to aggregated dir
tmp<-mapply(file.copy,files,file.path(aggrDateFolder,destFiles), overwrite=FALSE)


