#rm(list=ls())
library("shiny")
library("datasets")

#file_path <- "C:\\Users\\Jos� Manuel\\Google Drive\\COM\\C�digo\\R\\totta\\"
app_file_path <- "C:\\Users\\capelastegui\\workspace\\OFP\\Totta\\Totta-1\\2-R\\preprocess\\"


source(paste(app_file_path,"2013-11-18 HD Parser.R", sep=""))
source(paste(app_file_path,"2013-11-18 CPU Parser.R", sep=""))
source(paste(app_file_path,"2013-11-18 MEM Parser.R", sep=""))

runApp(app_file_path)