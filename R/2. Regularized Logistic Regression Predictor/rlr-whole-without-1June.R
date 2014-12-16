##### Elastic Net Temporal Window Cross Validation Experiment Taking out 1st of June Frequent Events #####
###RESULT: Models are not better than using the whole amount of events####


require(lubridate)
require(plyr)
require(dplyr)
require(glmnet)
source('C:/Users/JM/Documents/Github/l3p3/R/Logistic Regression Predictor/utils.R')
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
                          backward=list(b.5m=5*60,b.30m=30*60),
                          forward=list(f.5m=5*60,f.30m=30*60), 
                          offset.back=list(),
                          offset.fw=list(f.30m=0))
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
  data<-correctDates(data)
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
  evTable<-evTable[evTable$type!="69448"&evTable$type!="69447"&evTable$type!="69450"&evTable$type!="69449",]
  winEvTable <-getWinEvTables(evTable)
  return(winEvTable)
}

###Actual Elastic Net Functions

find_best_model_trycatch<-function(i,target){
  result<-tryCatch(
{
  return(find_best_model(i,target=target))
},
error=function(error){return(list("best_alpha"=NaN,"confusion_matrix"=NaN,"precision"=NaN,"recall"=NaN,"fscore"=NaN,"model"=NULL))})
return(result)
}

cross_validation_for_alpha<-function(target){ #Voy apuntando los que no:     f5m: c(1,3:15,17:46,49:58,60,61,64:71,73:length(predTables$b.5m[[target]]))
  list_of_models<-vector("list",length(predTables$b.5m[[target]]))         #f10m: c(1,2,4:9,11:17,19:49,51,53:61,63:length(predTables$b.5m[[target]]))
  names(list_of_models)<-names(predTables$b.5m[[target]])
  for (i in 1:length(predTables$b.5m[[target]])){                          #f15m: c(1,3:18,20:50,52,54:62,64:length(predTables$b.5m[[target]]))
    print(paste(c("Starting event ",names(predTables$b.5m[[target]])[i]))) #f20m: c(1:18,20:50,52,54:length(predTables$b.5m[[target]]))
    print("____________")                                                  #f25m: c(1:18,20:50,52,54:length(predTables$b.5m[[target]]))
    list_of_models[[i]]<-find_best_model_trycatch(i,target=target)      
  }
  return(list_of_models)
}


find_best_model<-function(target_variable,measure="auc",target){
  if(!exists("predTables")){predTables<-getPredTables(winEvTable$back, winEvTable$fw)}
  tmp_data<-predTables$b.5m[[target]][[target_variable]]
  
  complete_data<-three_fold_data(tmp_data)
  
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

winEvTable<-load_data()
predTables<-getPredTables(winEvTable$back,winEvTable$fw)
tmpb5f5<-cross_validation_for_alpha(1)
#tmpb5f10<-cross_validation_for_alpha(2)
#tmpb5f15<-cross_validation_for_alpha(3)
#tmpb5f20<-cross_validation_for_alpha(4)
#tmpb5f25<-cross_validation_for_alpha(5)
#tmpb5f30<-cross_validation_for_alpha(2)
#tmpb30f5<-cross_validation_for_alpha(1)
#tmpb30f30<-cross_validation_for_alpha(2)


# tmpb5f5_1<-cross_validation_for_alpha(1)
# tmpb5f5_2<-cross_validation_for_alpha(1)
# tmpb5f5_3<-cross_validation_for_alpha(1)
# tmpb5f5_4<-cross_validation_for_alpha(1)
# tmpb5f5_5<-cross_validation_for_alpha(1)
# tmpb5f5_6<-cross_validation_for_alpha(1)
# tmpb5f5_7<-cross_validation_for_alpha(1)
# tmpb5f5_8<-cross_validation_for_alpha(1)
# tmpb5f5_9<-cross_validation_for_alpha(1)
# tmpb5f5_10<-cross_validation_for_alpha(1)
# tmpb5f5_11<-cross_validation_for_alpha(1)
# tmpb5f5_12<-cross_validation_for_alpha(1)
# tmpb5f5_13<-cross_validation_for_alpha(1)
# tmpb5f5_14<-cross_validation_for_alpha(1)
# tmpb5f5_15<-cross_validation_for_alpha(1)
# tmpb5f5_16<-cross_validation_for_alpha(1)
# tmpb5f5_17<-cross_validation_for_alpha(1)
# tmpb5f5_18<-cross_validation_for_alpha(1)
# tmpb5f5_19<-cross_validation_for_alpha(1)
# tmpb5f5_20<-cross_validation_for_alpha(1)
