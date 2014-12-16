##### Elastic Net Local Data v2 (locality in prediction) with 5-minute observation and prediction window#####
require(lubridate)
library(plyr)
library(dplyr)
require(glmnet)
source('C:/Users/JM/Documents/Github/l3p3/R/Logistic Regression Predictor/utils.R')
source("C:/Users/JM/Documents/GitHub/l3p3/R/Y1/Raw Pedro's Scripts/totta/totta-1/tools/eventsPerSample.R")
source("C:/Users/JM/Documents/GitHub/l3p3/R/Y1/Raw Pedro's Scripts/utils/plyr.nested.R")
#source("C:/Users/JM/Documents/GitHub/l3p3/R/Regularized Logistic Regression Predictor/rlr-utils.R")

correctDates<-function(df){
  df$Created_On<-gsub("abr","04",substring(as.character(df$Created_On),0,20))
  df$Created_On<-gsub("may","05",substring(as.character(df$Created_On),0,20))
  df$Created_On<-gsub("jun","06",substring(as.character(df$Created_On),0,20))
  df$Created_On<-gsub("jul","07",substring(as.character(df$Created_On),0,20))
  df$Created_On<-gsub("ago","08",substring(as.character(df$Created_On),0,20))
  df$Created_On<-gsub("sep","09",substring(as.character(df$Created_On),0,20))
  df$Created_On<-gsub("oct","10",substring(as.character(df$Created_On),0,20))
  df$Created_On<-dmy_hms(df$Created_On,tz="Europe/Madrid")
  
  return(df)
}

getEventsInWindow<-function(windowOrigin,data,windowSize=300,direction="forward", offset=0)
{
  data.filtered<-data.frame()
  if(is.null(offset) || offset<0){offset<-0}
  if(offset>windowSize){offset<-windowSize}
  if(direction=="forward")
  {data.filtered<-data %.% filter (date>windowOrigin+offset & date <= windowOrigin+windowSize)}else 
    if(direction=="backward")
    {data.filtered<-data %.% filter (date<=windowOrigin-offset & date >= windowOrigin-windowSize)}else 
      if(direction=="both")
      {data.filtered<-data %.% filter (date<=windowOrigin+windowSize & date >= windowOrigin-windowSize)}  
  if(nrow(data.filtered)==0) {return(data.frame(date=windowOrigin, type="NO.EVENTS", evInWin=TRUE))}
  result<-data.filtered %.% group_by(type) %.% summarise(evInWin=n()>0) %.% mutate(date=windowOrigin) %.% select(date,type,evInWin)
}

rbind.all.columns <- function(x, y) {
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  x[, c(as.character(y.diff))] <- FALSE
  y[, c(as.character(x.diff))] <- FALSE
  return(rbind(x, y))
}

correctDuplicates<-function(df){
  duplicates<-which(duplicated(df$date))
  if(length(duplicates)==0){return (df)}
  else
  {
    for (i in 1:length(duplicates)){
      df[duplicates[i],1]<-df$date[duplicates[i]]
      df[duplicates[i],2:ncol(df)]<-df[duplicates[i],2:ncol(df)]|df[duplicates[i]-1,2:ncol(df)]
      df<-df[-(duplicates[i]-1),]
      duplicates-1
    }
    return(df)
  }
}

load_data<-function(simple=TRUE){
  data_april<-read.table('C:/Users/JM/Documents/experiment-data/parsed/04.Abril-events.csv',sep=",")
  names(data_april)<-c("Severity","Created_On","Name","Event_Type","Event","Created_By","Cleared_On","Cleared_By","Model_Type_Name","Event_Precedence")
  data_may<-read.table('C:/Users/JM/Documents/experiment-data/parsed/05.Mayo-events.csv',sep=",")
  names(data_may)<-c("Severity","Created_On","Name","Event_Type","Event","Created_By","Cleared_On","Cleared_By","Model_Type_Name","Event_Precedence")
  data_june<-read.table('C:/Users/JM/Documents/experiment-data/parsed/06.Junio-events.csv',sep=",")
  names(data_june)<-c("Severity","Created_On","Name","Event_Type","Event","Created_By","Cleared_On","Cleared_By","Model_Type_Name","Event_Precedence")
  data_july<-read.table('C:/Users/JM/Documents/experiment-data/parsed/07.Julio-events.csv',sep=",")
  names(data_july)<-c("Severity","Created_On","Name","Event_Type","Event","Created_By","Cleared_On","Cleared_By","Model_Type_Name","Event_Precedence")
  data_august<-read.table('C:/Users/JM/Documents/experiment-data/parsed/08.Agosto-events.csv',sep=",")
  names(data_august)<-c("Severity","Created_On","Name","Event_Type","Event","Created_By","Cleared_On","Cleared_By","Model_Type_Name","Event_Precedence")
  data<-rbind(data_april,data_may,data_june,data_july,data_august)
  names(data)<-c("Severity","Created_On","Name","Event_Type","Event","Created_By","Cleared_On","Cleared_By","Model_Type_Name","Event_Precedence")
  data<-correctDates(data)
  levels(data$Severity)[levels(data$Severity)==""]<-"Blank"
  data$Event_Type<-factor(data$Event_Type)
  data<-data.frame("date"=data$Created_On,"node"=droplevels(data$Name),"type"=droplevels(data$Event_Type))
  if (!simple){data$type<-factor(paste(data$node,data$type,sep="-"))}
  data<-data %.% tbl_df() %.% select(date,type)
  data<-data[order(data$date),]
  return(data)
}

###Actual Elastic Net Functions

find_best_model_trycatch<-function(df){
  result<-tryCatch(
{
  return(find_best_model(df))
},
error=function(error){return(list("best_alpha"=NaN,"confusion_matrix"=NaN,"precision"=NaN,"recall"=NaN,"fscore"=NaN,"model"=NULL))})
return(result)
}

find_best_model<-function(df,measure="auc"){
  
  complete_data<-three_fold_data(df)
  
  input<-complete_data$training[,2:(length(complete_data$training)-1)]
  output<-complete_data$training[,length(complete_data$training)]
  list_of_models<-list()
  alpha_list<-c(1,0.98,0.8,0.6,0.4,0.2,0.05,0)
  
  #Training Loop
  for (i in 1:length(alpha_list)){
    input_matrix<-matrix(nrow=dim(input)[1],ncol=dim(input)[2])
    input_matrix[which(input==TRUE)]<-1
    input_matrix[which(input==FALSE)]<-0
    output_matrix<-matrix(nrow=length(output),ncol=1)
    output_matrix[which(output==TRUE)]<-1
    output_matrix[which(output==FALSE)]<-0
    tryCatch({cvmodel<-cv.glmnet(x=input_matrix,y=output_matrix,family="binomial",type.measure=measure,alpha=alpha_list[i])})
    print(paste(c("model created with alpha",alpha_list[i])))
    list_of_models[[i]]<-cvmodel
  }
  
  #Validation loop
  input<- complete_data$validation[,2:(length(complete_data$validation)-1)]
  output<-complete_data$validation[,length(complete_data$validation)]
  
  input_matrix<-matrix(nrow=dim(input)[1],ncol=dim(input)[2])
  input_matrix[which(input==TRUE)]<-1
  input_matrix[which(input==FALSE)]<-0
  
  output[which(output==TRUE)]<-1
  output[which(output==FALSE)]<-0
  output<-factor(output)
  predictions_frame<-data.frame(matrix(nrow=length(output), ncol=length(alpha_list)+1))
  names(predictions_frame)<-c(as.character(alpha_list),"real")
  predictions_frame$real<-output
  list_of_fscores<-numeric()
  for (i in 1:length(alpha_list)){
    predictions_frame[,i]<-factor(as.numeric(predict(list_of_models[[i]],newx=input_matrix,type="class",s="lambda.min")))
    print("prediction made!")
    test_table<-data.frame("pred"=predictions_frame[,i],"real"=output)
    
    confusion_matrix<-matrix(nrow=2,ncol=2)
    confusion_matrix[1,1]<-length(which(test_table[,1]==1&test_table[,2]==1))
    confusion_matrix[2,1]<-length(which(test_table[,1]==0&test_table[,2]==1))
    confusion_matrix[1,2]<-length(which(test_table[,1]==1&test_table[,2]==0))
    confusion_matrix[2,2]<-length(which(test_table[,1]==0&test_table[,2]==0))
    
    if (sum(confusion_matrix[1,])==0){
      precision<-0
    }else{
      precision<-confusion_matrix[1,1]/sum(confusion_matrix[1,])}
    
    if (sum(confusion_matrix[1,])==0){
      recall<-0
    }else{recall<-confusion_matrix[1,1]/sum(confusion_matrix[,1])}
    if(precision==0|recall==0){list_of_fscores[i]<-0}
    else{list_of_fscores[i]<-2*precision*recall/(precision+recall)}
  }
  best_alpha_index<-which(list_of_fscores==max(list_of_fscores))[1]
  best_alpha<-alpha_list[best_alpha_index]
  
  
  #Test phase
  input<- complete_data$test[,2:(length(complete_data$test)-1)]
  output<-complete_data$test[,length(complete_data$test)]
  
  input_matrix<-matrix(nrow=dim(input)[1],ncol=dim(input)[2])
  input_matrix[which(input==TRUE)]<-1
  input_matrix[which(input==FALSE)]<-0
  
  output[which(output==TRUE)]<-1
  output[which(output==FALSE)]<-0
  output<-factor(output)
  
  alpha<-best_alpha
  
  prediction<-factor(as.numeric(predict(list_of_models[[best_alpha_index]],newx=input_matrix,type="class",s="lambda.min")))
  print("prediction made!")
  test_table<-data.frame(pred=prediction,real=output)
  
  confusion_matrix<-matrix(nrow=2,ncol=2)
  confusion_matrix[1,1]<-length(which(test_table[,1]==1&test_table[,2]==1))
  confusion_matrix[2,1]<-length(which(test_table[,1]==0&test_table[,2]==1))
  confusion_matrix[1,2]<-length(which(test_table[,1]==1&test_table[,2]==0))
  confusion_matrix[2,2]<-length(which(test_table[,1]==0&test_table[,2]==0))
  
  if (sum(confusion_matrix[1,])==0){
    precision<-0
  }else{
    precision<-confusion_matrix[1,1]/sum(confusion_matrix[1,])}
  
  if (sum(confusion_matrix[1,])==0){
    recall<-0
  }else{recall<-confusion_matrix[1,1]/sum(confusion_matrix[,1])}
  fscore<-2*precision*recall/(precision+recall)
  things_to_return<-list("best_alpha"=best_alpha,"confusion_matrix"=confusion_matrix,"precision"=precision,"recall"=recall,"fscore"=fscore,"model"=list_of_models[[best_alpha_index]])
  return(things_to_return)  
}

three_fold_data<-function(input,zero_proportion=3){
  zeros_frame<-input[input$target==FALSE,]
  zeros_frame<-zeros_frame[sample(nrow(zeros_frame)),]
  
  ones_frame<-input[input$target==TRUE,]
  ones_frame<-ones_frame[sample(nrow(ones_frame)),]
  
  if (dim(zeros_frame)[1]<(zero_proportion*dim(ones_frame)[1])){
    training_data<-rbind(ones_frame[1:(3*nrow(ones_frame)/5),],zeros_frame[sample(nrow(zeros_frame),size=length(1:(3*nrow(ones_frame)/5))*zero_proportion,replace=TRUE),])
    validate_data<-rbind(ones_frame[floor(((3*nrow(ones_frame)/5)+1)):floor((4*nrow(ones_frame)/5)),],zeros_frame[sample(nrow(zeros_frame),size=length(floor(((zero_proportion*3*nrow(ones_frame)/5)+1)):floor((4*nrow(ones_frame)/5)))*zero_proportion,replace=TRUE),])
    test_data<-rbind(ones_frame[floor(((4*nrow(ones_frame)/5)+1)):floor((5*nrow(ones_frame)/5)),],zeros_frame[sample(nrow(zeros_frame),size=length(floor(((zero_proportion*4*nrow(ones_frame)/5)+1)):floor((zero_proportion*5*nrow(ones_frame)/5))),replace=TRUE),])
  }
  else{
    training_data<-rbind(ones_frame[1:(3*nrow(ones_frame)/5),],zeros_frame[1:floor((zero_proportion*(3*nrow(ones_frame)/5))),])
    validate_data<-rbind(ones_frame[floor(((3*nrow(ones_frame)/5)+1)):floor((4*nrow(ones_frame)/5)),],zeros_frame[floor(((zero_proportion*(3*nrow(ones_frame)/5))+1)):floor(((zero_proportion*(4*nrow(ones_frame)/5)))),])
    test_data<-rbind(ones_frame[floor(((4*nrow(ones_frame)/5)+1)):floor((5*nrow(ones_frame)/5)),],zeros_frame[floor(((zero_proportion*(4*nrow(ones_frame)/5))+1)):floor(((zero_proportion*(5*nrow(ones_frame)/5)))),])
  }
  result<-list("training"=training_data,"validation"=validate_data,"test"=test_data)
  return(result)
  
}

getTables<-function(df_simple,df_complex,window_back=300,window_fw=300){
  tables_back<-list()
  tables_fw<-list()
  for (i in 1:5){
    tmp_simple<-df_simple[floor(((((dim(df_simple)[1])*(i-1))/5)+1)):floor(((((dim(df_simple)[1])*(i))/5))),]
    tmp_complex<-df_complex[floor(((((dim(df_complex)[1])*(i-1))/5)+1)):floor(((((dim(df_complex)[1])*(i))/5))),]
    
    tmp_back<-ldply(unique(tmp_simple$date),getEventsInWindow,tmp_simple, windowSize=window_back,direction="backward", offset=0)%.% tbl_df() %.% arrange(date)
    tmp_back<-dcast(tmp_back, date ~ type, fun.aggregate= any) %.% tbl_df()
    tables_back[[i]]<-tmp_back
    
    tmp_fw<-ldply(unique(tmp_complex$date),getEventsInWindow,tmp_complex, windowSize=window_fw,direction="forward", offset=0)%.% tbl_df() %.% arrange(date)
    tmp_fw<-dcast(tmp_fw, date ~ type, fun.aggregate= any) %.% tbl_df()
    tables_fw[[i]]<-tmp_fw
  }
  back<-rbind.all.columns(tables_back[[1]],tables_back[[2]])
  back<-rbind.all.columns(back,tables_back[[3]])
  back<-rbind.all.columns(back,tables_back[[4]])
  back<-rbind.all.columns(back,tables_back[[5]])
  back<-correctDuplicates(back)
  fw<-rbind.all.columns(tables_fw[[1]],tables_fw[[2]])
  fw<-rbind.all.columns(fw,tables_fw[[3]])
  fw<-rbind.all.columns(fw,tables_fw[[4]])
  fw<-rbind.all.columns(fw,tables_fw[[5]])
  fw<-correctDuplicates(fw)
  return(list("back"=back,"fw"=fw))
}

data_simple<-load_data(simple=TRUE)
data_complex<-load_data(simple=FALSE)
tables<-getTables(data_simple,data_complex)
models_sc<-vector("list",ncol(tables$fw)-1)
names(models_sc)<-names(tables$fw)[2:ncol(tables$fw)]
#for (i in 1:(ncol(tables$fw)-1)){
for (i in 1:(ncol(tables$fw)-1)){
  predTable<-cbind(tables$back,tables$fw[,i+1])
  names(predTable)[length(names(predTable))]<-"target"
  print(paste(c("Starting event ",names(tables$fw[i+1]))))
  print("____________")
  models_sc[[i]]<-find_best_model_trycatch(predTable)
}
