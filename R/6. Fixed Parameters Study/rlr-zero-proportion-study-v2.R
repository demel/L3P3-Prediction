##### This experiment measures the performance varying the zero proportion added to each ones dataset#####
require(lubridate)
require(plyr)
require(dplyr)
require(glmnet)
#source('C:/Users/JM/Documents/Github/l3p3/R/Logistic Regression Predictor/utils.R')
source("C:/Users/JM/Documents/GitHub/l3p3/R/Y1/Raw Pedro's Scripts/totta/totta-1/tools/eventsPerSample.R")
source("C:/Users/JM/Documents/GitHub/l3p3/R/Y1/Raw Pedro's Scripts/utils/plyr.nested.R")

##Functions defined in log-reg-scf-01-preprocess###
applyGetEventsInWindow<-function(data,windowSize=300,direction="forward", offset=0)
{
  tmp<-ldply(unique(data$date),getEventsInWindow,data, windowSize=windowSize,direction=direction, offset=offset) 
  dcast(tmp, date ~ type, fun.aggregate= any) %.% tbl_df()
}

getEventsInWindow<-function(windowOrigin,data,windowSize=300,direction="forward", offset=0)
{
  # filter: events before or after window origin
  data.filtered<-data.frame()
  # check window origin offset 
  if(is.null(offset) || offset<0){offset<-0}
  if(offset>windowSize){offset<-windowSize}
  if(direction=="forward")
  {data.filtered<-data %.% filter (date>windowOrigin+offset & date <= windowOrigin+windowSize)}else 
    if(direction=="backward")
    {data.filtered<-data %.% filter (date<=windowOrigin-offset & date >= windowOrigin-windowSize)}else 
      if(direction=="both")
      {data.filtered<-data %.% filter (date<=windowOrigin+windowSize & date >= windowOrigin-windowSize)}
  # if empty window, return row marking this date as having no events in window  
  if(nrow(data.filtered)==0) {return(data.frame(date=windowOrigin, type="NO.EVENTS", evInWin=TRUE))}
  #if (countEvents) result <- data.filtered %.% group_by(type) %.%  summarise(minDist1=n())
  result<-data.filtered %.% group_by(type) %.% summarise(evInWin=n()>0) %.% mutate(date=windowOrigin) %.% select(date,type,evInWin)
}


getWinEvTables <-function(data,
                          backward=list(b.5m=5*60),
                          forward=list(f.5m=5*60), 
                          offset.back=list(),
                          offset.fw=list())
{
  back<-llply.parallel.multilist(list.ref=backward,
                                 list.multi=list(windowSize=backward, offset=offset.back),
                                 n=1,
                                 data=data,
                                 .fun=function(sizeOffsetList,data)
                                 {applyGetEventsInWindow(data,windowSize=sizeOffsetList$windowSize, offset=sizeOffsetList$offset,direction="backward")}
  )
  
  attr(back,"windowSize")<-backward
  attr(back,"offset")<-offset.back
  
  fw<-llply.parallel.multilist(list.ref=forward,
                               list.multi=list(windowSize=forward, offset=offset.fw),
                               n=1,
                               data,
                               .fun=function(sizeOffsetList,data)
                               {applyGetEventsInWindow(data,windowSize=sizeOffsetList$windowSize, offset=sizeOffsetList$offset,direction="forward")}
  )
  
  attr(fw,"windowSize")<-forward
  attr(fw,"offset")<-offset.fw
  
  list(back=back,fw=fw)
}

getPredTables <- function (back,fw)
{
  llply.cross.ab(back, fw, .fun2=function(back.1,fw.1)
  {
    llply (fw.1 %.% select(-date),back.1,.fun=function(fw.1.col,back.1)
    {cbind(back.1,target=fw.1.col) %.% tbl_df()}
    )}
  )
}
###function to create winEvTables

load_data<-function(){
  ###Code from log-reg-scf-00-preprocess###
  
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
  #data<-correctDates(data)
  data$Created_On<-dmy_hms(data$Created_On)
  levels(data$Severity)[levels(data$Severity)==""]<-"Blank"
  data$Event_Type<-factor(data$Event_Type)
  #data_perico<-data[data$Severity=='Major'|data$Severity=='Critical',]
  data_perico<-data
  data_perico<-data.frame("date"=data_perico$Created_On,"node"=droplevels(data_perico$Name),"type"=droplevels(data_perico$Event_Type))
  event_types<-levels(data_perico$type)
  #data_perico<-recodeFactors(data_perico)
  #From here, Pedro's code can be used, turning data_perico into eventTable1
  eventTable1<-data_perico
  
  ###Code from log-reg-scf-01-preprocess###
  
  evTable<-eventTable1 %.% tbl_df() %.% select(date,type)
  evTable<-evTable[order(evTable$date),]
  winEvTable <-getWinEvTables(evTable)
  return(winEvTable)
}

###Actual Elastic Net Functions

find_best_model_trycatch<-function(i,target,zero_proportion=3){
  result<-tryCatch(
{
  return(find_best_model(i,target=target,zero_proportion=zero_proportion))
},
error=function(error){return(list("best_alpha"=NaN,"confusion_matrix"=NaN,"precision"=NaN,"recall"=NaN,"fscore"=NaN,"model"=NULL))})
return(result)
}

cross_validation_for_alpha<-function(target,zero_proportion=3){ 
  list_of_models<-vector("list",length(predTables$b.5m[[target]]))         
  names(list_of_models)<-names(predTables$b.5m[[target]])
  for (i in 1:length(predTables$b.5m[[target]])){                          
    print(paste(c("Starting event ",names(predTables$b.5m[[target]])[i]))) 
    print("____________")                                                  
    list_of_models[[i]]<-find_best_model_trycatch(i,target=target,zero_proportion=zero_proportion)      
  }
  return(list_of_models)
}


find_best_model<-function(target_variable,measure="auc",target,zero_proportion=3){
  if(!exists("predTables")){predTables<-getPredTables(winEvTable$back, winEvTable$fw)}
  tmp_data<-predTables$b.5m[[target]][[target_variable]]
  
  complete_data<-three_fold_data(tmp_data,zero_proportion)
  
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
  #zeros_frame<-zeros_frame[sample(nrow(zeros_frame)),]
  
  ones_frame<-input[input$target==TRUE,]
  #ones_frame<-ones_frame[sample(nrow(ones_frame)),]
  
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
  training_data<-training_data[sample(nrow(training_data)),]
  validate_data<-validate_data[sample(nrow(validate_data)),]
  test_data<-test_data[sample(nrow(test_data)),]
  result<-list("training"=training_data,"validation"=validate_data,"test"=test_data)
  return(result)
}

split_data<-function(lst){
  test<-list()
  for (i in 1:length(lst$b.5m$f.5m)){
    tmp<-split_df(lst$b.5m$f.5m[[i]])
    lst$b.5m$f.5m[[i]]<-tmp$training
    test[[i]]<-tmp$test
  }
  result<-list("predTables"=lst,"test"=test)
}

split_df<-function(df){
  zeros_frame<-df[df$target==FALSE,]
  zeros_frame<-zeros_frame[sample(nrow(zeros_frame)),]
  
  ones_frame<-df[df$target==TRUE,]
  ones_frame<-ones_frame[sample(nrow(ones_frame)),]
  
  training<-rbind(zeros_frame[1:(floor(dim(zeros_frame)[1]*0.8)),],ones_frame[1:(floor(length(ones_frame)*0.8)),])
  training<-training[sample(nrow(training)),]
  
  test<-rbind(zeros_frame[floor(dim(zeros_frame)[1]*0.8+1):dim(zeros_frame)[1],],ones_frame[floor(dim(ones_frame)[1]*0.8+1):dim(ones_frame)[1],])
  test<-test[sample(nrow(test)),]
  
  result<-list("training"=training,"test"=test)
  return(result)
}

winEvTable<-load_data()
predTables<-getPredTables(winEvTable$back,winEvTable$fw)
#NEW: data split
splitted<-split_data(predTables)
predTables<-splitted$predTables
test<-splitted$test
#NEW
zero_study_b5f5<-list()
time_vec<-list()
time_vec[[1]]<-system.time({zero_study_b5f5[[1]]<-cross_validation_for_alpha(target=1,zero_proportion=1)})
time_vec[[2]]<-system.time({zero_study_b5f5[[2]]<-cross_validation_for_alpha(target=1,zero_proportion=2)})
time_vec[[3]]<-system.time({zero_study_b5f5[[3]]<-cross_validation_for_alpha(target=1,zero_proportion=3)})
time_vec[[4]]<-system.time({zero_study_b5f5[[4]]<-cross_validation_for_alpha(target=1,zero_proportion=4)})
time_vec[[5]]<-system.time({zero_study_b5f5[[5]]<-cross_validation_for_alpha(target=1,zero_proportion=5)})
time_vec[[6]]<-system.time({zero_study_b5f5[[6]]<-cross_validation_for_alpha(target=1,zero_proportion=6)})
time_vec[[7]]<-system.time({zero_study_b5f5[[7]]<-cross_validation_for_alpha(target=1,zero_proportion=7)})
time_vec[[8]]<-system.time({zero_study_b5f5[[8]]<-cross_validation_for_alpha(target=1,zero_proportion=8)})
time_vec[[9]]<-system.time({zero_study_b5f5[[9]]<-cross_validation_for_alpha(target=1,zero_proportion=9)})
time_vec[[10]]<-system.time({zero_study_b5f5[[10]]<-cross_validation_for_alpha(target=1,zero_proportion=10)})
time_vec[[11]]<-system.time({zero_study_b5f5[[11]]<-cross_validation_for_alpha(target=1,zero_proportion=11)})
time_vec[[12]]<-system.time({zero_study_b5f5[[12]]<-cross_validation_for_alpha(target=1,zero_proportion=12)})
time_vec[[13]]<-system.time({zero_study_b5f5[[13]]<-cross_validation_for_alpha(target=1,zero_proportion=13)})
time_vec[[14]]<-system.time({zero_study_b5f5[[14]]<-cross_validation_for_alpha(target=1,zero_proportion=14)})
time_vec[[15]]<-system.time({zero_study_b5f5[[15]]<-cross_validation_for_alpha(target=1,zero_proportion=15)})



######PONER EL MODELO SIN PROPORCION DE ZEROS OTRA VEZ Y ANALIZAR A PARTIR DE AQUI
models<-list("no_zeros_models"=no_zeros_models,"zeros_models"=zero_study_b5f5,"times"=list(time_no_zeros,time_vec),"test_data"=test)

###NOW THE ANALYSIS WILL CONTINUE IN rlr-zero-proportion-analysis TOGETHER WITH THE DATA FROM rlr-zero-proportion-no-zeros####

#NEW: model testing
results<-list()
for (i in 1:length(zero_study_b5f5)){
  print (i)
  results[[i]]<-list()
  for (j in 1:length(test)){
  model<-zero_study_b5f5[[i]][[j]]$model
  if (!is.null(model)){
    print (j)
    input<- test[[j]][,2:(length(test[[j]])-1)]
    output<-test[[j]][,length(test[[j]])]
    
    input_matrix<-matrix(nrow=dim(input)[1],ncol=dim(input)[2])
    input_matrix[which(input==TRUE)]<-1
    input_matrix[which(input==FALSE)]<-0
    
    output[which(output==TRUE)]<-1
    output[which(output==FALSE)]<-0
    output<-factor(output)
    
    prediction<-factor(as.numeric(predict(model,newx=input_matrix,type="class",s="lambda.min")))
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
    results[[i]][[j]]<-list("confusion_matrix"=confusion_matrix,"precision"=precision,"recall"=recall,"fscore"=fscore)
    }
  }
  #names(results[[i]])<-names(zero_study_b5f5[[i]])
}
#names(results)<-c("1","2","3","4","5")
#NEW

#no_zeros_models testing
results_no_zeros<-list()
for (j in 1:length(test)){
  model<-no_zeros_models[[j]]$model
  if (!is.null(model)){
    print (j)
    input<- test[[j]][,2:(length(test[[j]])-1)]
    output<-test[[j]][,length(test[[j]])]
    
    input_matrix<-matrix(nrow=dim(input)[1],ncol=dim(input)[2])
    input_matrix[which(input==TRUE)]<-1
    input_matrix[which(input==FALSE)]<-0
    
    output[which(output==TRUE)]<-1
    output[which(output==FALSE)]<-0
    output<-factor(output)
    
    prediction<-factor(as.numeric(predict(model,newx=input_matrix,type="class",s="lambda.min")))
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
    results_no_zeros[[j]]<-list("confusion_matrix"=confusion_matrix,"precision"=precision,"recall"=recall,"fscore"=fscore)
  }
}
#


##Information retrieval

find_appearances_list<-function(lst,value){
  count<-0
  for (i in 1:length(lst)){
    if (value %in% lst[[i]]){count<-count+1}
  }
  return(count)
}

extract_precisions<-function(lst){
  res<-vector()
  for (i in 1:length(lst)){
    if (!is.null(lst[[i]])){
    res[i]<-lst[[i]]$precision
    }
  }
  return(res)
}

extract_recalls<-function(lst){
  res<-vector()
  for (i in 1:length(lst)){
    if (!is.null(lst[[i]])){
    res[i]<-lst[[i]]$recall
    }
  }
  return(res)
}

extract_fscores<-function(lst){
  res<-vector()
  for (i in 1:length(lst)){
    if (!is.null(lst[[i]])){
    res[i]<-lst[[i]]$fscore
    }
  }
  return(res)
}

prec_df<-data.frame("0"=extract_precisions(results_no_zeros),"1"=extract_precisions(results[[1]]),"2"=extract_precisions(results[[2]]),"3"=extract_precisions(results[[3]]),"4"=extract_precisions(results[[4]]),"5"=extract_precisions(results[[5]]),"6"=extract_precisions(results[[6]]),"7"=extract_precisions(results[[7]]),"8"=extract_precisions(results[[8]]),"9"=extract_precisions(results[[9]]),"10"=extract_precisions(results[[10]]),"11"=extract_precisions(results[[11]]),"12"=extract_precisions(results[[12]]),"13"=extract_precisions(results[[13]]),"14"=extract_precisions(results[[14]]),"15"=extract_precisions(results[[15]]))
rec_df<-data.frame("0"=extract_recalls(results_no_zeros),"1"=extract_recalls(results[[1]]),"2"=extract_recalls(results[[2]]),"3"=extract_recalls(results[[3]]),"4"=extract_recalls(results[[4]]),"5"=extract_recalls(results[[5]]),"6"=extract_recalls(results[[6]]),"7"=extract_recalls(results[[7]]),"8"=extract_recalls(results[[8]]),"9"=extract_recalls(results[[9]]),"10"=extract_recalls(results[[10]]),"11"=extract_recalls(results[[11]]),"12"=extract_recalls(results[[12]]),"13"=extract_recalls(results[[13]]),"14"=extract_recalls(results[[14]]),"15"=extract_recalls(results[[15]]))                                                                                                                                   
fsc_df<-data.frame("0"=extract_fscores(results_no_zeros),"1"=extract_fscores(results[[1]]),"2"=extract_fscores(results[[2]]),"3"=extract_fscores(results[[3]]),"4"=extract_fscores(results[[4]]),"5"=extract_fscores(results[[5]]),"6"=extract_fscores(results[[6]]),"7"=extract_fscores(results[[7]]),"8"=extract_fscores(results[[8]]),"9"=extract_fscores(results[[9]]),"10"=extract_fscores(results[[10]]),"11"=extract_fscores(results[[11]]),"12"=extract_fscores(results[[12]]),"13"=extract_fscores(results[[13]]),"14"=extract_fscores(results[[14]]),"15"=extract_fscores(results[[15]]))

prec<-list()
prec[[1]]<-prec_df
prec[[2]]<-c(mean(prec_df[,1],na.rm=TRUE),mean(prec_df[,2],na.rm=TRUE),mean(prec_df[,3],na.rm=TRUE),mean(prec_df[,4],na.rm=TRUE),mean(prec_df[,5],na.rm=TRUE),mean(prec_df[,6],na.rm=TRUE),mean(prec_df[,7],na.rm=TRUE),mean(prec_df[,8],na.rm=TRUE),mean(prec_df[,9],na.rm=TRUE),mean(prec_df[,10],na.rm=TRUE),mean(prec_df[,11],na.rm=TRUE),mean(prec_df[,12],na.rm=TRUE),mean(prec_df[,13],na.rm=TRUE),mean(prec_df[,14],na.rm=TRUE),mean(prec_df[,15],na.rm=TRUE),mean(prec_df[,16],na.rm=TRUE))
tmp<-apply(prec_df,1,function(x) {which(x==max(x))})
prec[[3]]<-c(find_appearances_list(tmp,value=1),find_appearances_list(tmp,value=2),find_appearances_list(tmp,value=3),find_appearances_list(tmp,value=4),find_appearances_list(tmp,value=5),find_appearances_list(tmp,value=6),find_appearances_list(tmp,value=7),find_appearances_list(tmp,value=8),find_appearances_list(tmp,value=9),find_appearances_list(tmp,value=10),find_appearances_list(tmp,value=11),find_appearances_list(tmp,value=12),find_appearances_list(tmp,value=13),find_appearances_list(tmp,value=14),find_appearances_list(tmp,value=15),find_appearances_list(tmp,value=16))
names(prec)<-c("df","means","maximum_appearance")

rec<-list()
rec[[1]]<-rec_df
rec[[2]]<-c(mean(rec_df[,1],na.rm=TRUE),mean(rec_df[,2],na.rm=TRUE),mean(rec_df[,3],na.rm=TRUE),mean(rec_df[,4],na.rm=TRUE),mean(rec_df[,5],na.rm=TRUE),mean(rec_df[,6],na.rm=TRUE),mean(rec_df[,7],na.rm=TRUE),mean(rec_df[,8],na.rm=TRUE),mean(rec_df[,9],na.rm=TRUE),mean(rec_df[,10],na.rm=TRUE),mean(rec_df[,11],na.rm=TRUE),mean(rec_df[,12],na.rm=TRUE),mean(rec_df[,13],na.rm=TRUE),mean(rec_df[,14],na.rm=TRUE),mean(rec_df[,15],na.rm=TRUE),mean(rec_df[,16],na.rm=TRUE))
tmp<-apply(rec_df,1,function(x) {which(x==max(x))})
rec[[3]]<-c(find_appearances_list(tmp,value=1),find_appearances_list(tmp,value=2),find_appearances_list(tmp,value=3),find_appearances_list(tmp,value=4),find_appearances_list(tmp,value=5),find_appearances_list(tmp,value=6),find_appearances_list(tmp,value=7),find_appearances_list(tmp,value=8),find_appearances_list(tmp,value=9),find_appearances_list(tmp,value=10),find_appearances_list(tmp,value=11),find_appearances_list(tmp,value=12),find_appearances_list(tmp,value=13),find_appearances_list(tmp,value=14),find_appearances_list(tmp,value=15),find_appearances_list(tmp,value=16))
names(rec)<-c("df","means","maximum_appearance")

fsc<-list()
fsc[[1]]<-fsc_df
fsc[[2]]<-c(mean(fsc_df[,1],na.rm=TRUE),mean(fsc_df[,2],na.rm=TRUE),mean(fsc_df[,3],na.rm=TRUE),mean(fsc_df[,4],na.rm=TRUE),mean(fsc_df[,5],na.rm=TRUE),mean(fsc_df[,6],na.rm=TRUE),mean(fsc_df[,7],na.rm=TRUE),mean(fsc_df[,8],na.rm=TRUE),mean(fsc_df[,9],na.rm=TRUE),mean(fsc_df[,10],na.rm=TRUE),mean(fsc_df[,11],na.rm=TRUE),mean(fsc_df[,12],na.rm=TRUE),mean(fsc_df[,13],na.rm=TRUE),mean(fsc_df[,14],na.rm=TRUE),mean(fsc_df[,15],na.rm=TRUE),mean(fsc_df[,16],na.rm=TRUE))
tmp<-apply(fsc_df,1,function(x) {which(x==max(x))})
fsc[[3]]<-c(find_appearances_list(tmp,value=1),find_appearances_list(tmp,value=2),find_appearances_list(tmp,value=3),find_appearances_list(tmp,value=4),find_appearances_list(tmp,value=5),find_appearances_list(tmp,value=6),find_appearances_list(tmp,value=7),find_appearances_list(tmp,value=8),find_appearances_list(tmp,value=9),find_appearances_list(tmp,value=10),find_appearances_list(tmp,value=11),find_appearances_list(tmp,value=12),find_appearances_list(tmp,value=13),find_appearances_list(tmp,value=14),find_appearances_list(tmp,value=15),find_appearances_list(tmp,value=16))
fsc[[4]]<-c(length(which(!is.na(fsc_df[,1]))),length(which(!is.na(fsc_df[,2]))),length(which(!is.na(fsc_df[,3]))),length(which(!is.na(fsc_df[,4]))),length(which(!is.na(fsc_df[,5]))),length(which(!is.na(fsc_df[,6]))),length(which(!is.na(fsc_df[,7]))),length(which(!is.na(fsc_df[,8]))),length(which(!is.na(fsc_df[,9]))),length(which(!is.na(fsc_df[,10]))),length(which(!is.na(fsc_df[,11]))),length(which(!is.na(fsc_df[,12]))),length(which(!is.na(fsc_df[,13]))),length(which(!is.na(fsc_df[,14]))),length(which(!is.na(fsc_df[,15]))),length(which(!is.na(fsc_df[,16]))))
fsc[[5]]<-apply(fsc_df,2,function(x){return(sum(x,na.rm=TRUE))})
names(fsc)<-c("df","means","maximum_appearance","non_zero_models","score_sum")

result<-list(results_no_zeros,zero_study_b5f5,prec,rec,fsc,c(time_no_zeros,time_vec))
names(result)<-c("no_proportion_model","models","precision","recall","fscore","time")
save(result,file="data.Rdata")
