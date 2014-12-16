#This file depends on files log-reg-scf-00-preprocess.R and log-reg-scf-01-preprocess.R

## Using glmnet
require(glmnet)
require(dplyr)
require(plyr)

#### Function definitions ####

prune_data<-function(data,zero_proportion=4){
  number_of_1s<-length(data$target[data$target==TRUE])
  number_of_0s<-number_of_1s*zero_proportion
  zeros_frame<-data[data$target==FALSE,]
  replacement<-FALSE
  if (number_of_0s>dim(zeros_frame)[2]){return(data)}
  zeros_frame<-zeros_frame[sample(nrow(zeros_frame),number_of_0s,replace=replacement),]
  result<-rbind(data[data$target==TRUE,],zeros_frame)
  result
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


regularized_linear_model<-function(alpha,target_variable,measure,use_train,show_output=FALSE,prune=FALSE,zero_proportion=4){
    if (use_train){predTables<-getPredTables(winEvTable.train$back, winEvTable.train$fw)
    }else {predTables<-getPredTables(winEvTable$back, winEvTable$fw)}
    
    tmp_data<-predTables$b.5m$f.5m[[target_variable]]
    if(prune){
      tmp_data<-prune_data(tmp_data,zero_proportion)
    }
    input<-tmp_data[,2:(length(tmp_data)-1)]
    output<-tmp_data[,length(tmp_data)]
    
    
    
    input_matrix<-matrix(nrow=dim(input)[1],ncol=dim(input)[2])
    input_matrix[which(input==TRUE)]<-1
    input_matrix[which(input==FALSE)]<-0
    output_matrix<-matrix(nrow=length(output),ncol=1)
    output_matrix[which(output==TRUE)]<-1
    output_matrix[which(output==FALSE)]<-0
    #model<-glmnet(x=input_matrix,y=output_matrix,family="binomial"
    if (show_output){print("data created!")}
    tryCatch({cvmodel<-cv.glmnet(x=input_matrix,y=output_matrix,family="binomial",type.measure=measure,alpha=alpha)
    if (show_output){print("model created!")}
    
    #plot(cvmodel)
    
    
    ##Model testing
    
    if (use_train){
      new_x<-winEvTable.test$back$b.5m[,2:dim(winEvTable.test$back$b.5m)[2]]
      test_data<-winEvTable.test$fw$f.5m[[target_variable+1]]
    }else{
      new_x<-winEvTable$back$b.5m[,2:dim(winEvTable$back$b.5m)[2]]
      test_data<-winEvTable$fw$f.5m[[target_variable+1]]
    }
    new_x_matrix<-matrix(nrow=dim(new_x)[1],ncol=dim(new_x)[2])
    new_x_matrix[which(new_x==TRUE)]<-1
    new_x_matrix[which(new_x==FALSE)]<-0
    
    test_data[which(test_data==TRUE)]<-1
    test_data[which(test_data==FALSE)]<-0
    test_data<-factor(test_data)
    
    prediction<-factor(as.numeric(predict(cvmodel,newx=new_x_matrix,type="class",s="lambda.min")))
    if (show_output){print("prediction made!")}
    test_table<-data.frame(pred=prediction,real=test_data)
    
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
    #print(confusion_matrix)
    #if (precision>0.5){
      predictor<-list()
      predictor[[1]]<-names(predTables$b.5m$f.5m)[target_variable]
      predictor[[2]]<-coefficients(cvmodel,s='lambda.min')
      predictor[[3]]<-precision
      predictor[[4]]<-recall
      predictor[[5]]<-confusion_matrix
      test_ones<-sum(as.numeric(as.character(test_data)))
      predictor[[6]]<-test_ones
    if (show_output){print("Finished!")}
      return (predictor)
    #}},
    },error=function(e){
      if (show_output){
        print(paste('Model',i,' failed'))}
            })
}


create_models<-function(alpha,measure,use_train,show_output,prune=FALSE,zero_proportion=4){
    rm(models)
    models<-list()
    if (use_train){predTables<-getPredTables(winEvTable.train$back, winEvTable.train$fw)
    }else {predTables<-getPredTables(winEvTable$back, winEvTable$fw)}
    sequence<-seq(1,length(names(predTables$b.5m$f.30m)))
    for (i in sequence){
        predictor<-list()
        models[[i]]<-regularized_linear_model(alpha,target_variable=i,measure,use_train,show_output,prune,zero_proportion)
        if (models[[i]]=="Model failed :("){
          models[[i]][1]<-names(predTables$b.5m$f.5m)[i]
          #print(paste('Model',i,' failed'))
        }
        else{
          print(paste("Finished model: ",i))
          print(models[[i]][5])}
    }
    
    model_table<-data.frame(matrix(nrow=length(models),ncol=4))
    names(model_table)<-c('event','precision','recall','test_ones')
    for (i in 1:length(models)){
      tryCatch({model_table[i,1]<-models[[i]][1]
      model_table[i,2]<-models[[i]][3]
      model_table[i,3]<-models[[i]][4]
      model_table[i,4]<-models[[i]][6]},error=function(e){})
    }
    result<-list("models"=models,"table"=model_table)
    return (result)
}

##Same function as regularized_linear_model but with three sets cross validation for selecting the best value of alpha
find_best_model<-function(target_variable,measure="auc"){
  if(!exists("predTables")){predTables<-getPredTables(winEvTable$back, winEvTable$fw)}
  tmp_data<-predTables$b.5m$f.5m[[target_variable]]
  
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


### Actual model creations #####

mega_function<-function(alpha,zero_proportion){

#Models without training test separation
models1<-list()
model_table1<-list()
result<-create_models(1,"auc",FALSE,TRUE,FALSE,zero_proportion)
models1<-result$models
model_table1<-result$table

#Models with training test separation
models2<-list()
model_table2<-list()
result<-create_models(1,"auc",TRUE,TRUE,FALSE,zero_proportion)
models2<-result$models
model_table2<-result$table

comparation_table<-data.frame("event"=model_table1$event,"precision_wo_test"=model_table1$precision,"precision_w_test"=model_table2$precision,"recall_wo_test"=model_table1$recall,"recall_w_test"=model_table2$recall,"ones_wo_test"=model_table1$test_ones,"ones_w_test"=model_table2$test_ones)

#Applying rare events prediction techniques: taking out the zeros, until the proportion of them is

#Models without training test separation
models1<-list()
model_table1<-list()
result<-create_models(1,"auc",FALSE,TRUE,TRUE,zero_proportion)
models1<-result$models
model_table1<-result$table

#Models with training test separation
models2<-list()
model_table2<-list()
result<-create_models(1,"auc",TRUE,TRUE,TRUE,zero_proportion)
models2<-result$models
model_table2<-result$table

comparation_table2<-data.frame("event"=model_table1$event,"precision_wo_test"=model_table1$precision,"precision_w_test"=model_table2$precision,"recall_wo_test"=model_table1$recall,"recall_w_test"=model_table2$recall,"ones_wo_test"=model_table1$test_ones,"ones_w_test"=model_table2$test_ones)

precision_table<-data.frame("event"=model_table1$event,"p_wo"=comparation_table$precision_wo_test,"p_w"=comparation_table$precision_w_test,"p_wo_r"=comparation_table2$precision_wo_test,"p_w_r"=comparation_table2$precision_w_test,"o_wo"=model_table1$test_ones,"o_w"=model_table2$test_ones)
precision_table[,2]<-as.numeric(as.character(precision_table[,2]))
precision_table[,3]<-as.numeric(as.character(precision_table[,3]))
precision_table[,4]<-as.numeric(as.character(precision_table[,4]))
precision_table[,5]<-as.numeric(as.character(precision_table[,5]))
precision_table[is.na(precision_table)]<-0
recall_table<-data.frame("event"=model_table1$event,"r_wo"=comparation_table$recall_wo_test,"r_w"=comparation_table$recall_w_test,"r_wo_r"=comparation_table2$recall_wo_test,"r_w_r"=comparation_table2$recall_w_test,"o_wo"=model_table1$test_ones,"o_w"=model_table2$test_ones)
recall_table[,2]<-as.numeric(as.character(recall_table[,2]))
recall_table[,3]<-as.numeric(as.character(recall_table[,3]))
recall_table[,4]<-as.numeric(as.character(recall_table[,4]))
recall_table[,5]<-as.numeric(as.character(recall_table[,5]))
recall_table[is.na(recall_table)]<-0

p<-ggplot()
p<-p+geom_point(aes(x=precision_table$p_wo[precision_table$p_wo>0],y=recall_table$r_wo[recall_table$r_wo>0]),color="blue")
p<-p+geom_point(aes(x=precision_table$p_wo_r[precision_table$p_wo_r>0],y=recall_table$r_wo_r[recall_table$r_wo_r>0]),color="red")
p<-p+xlab("Precision")+ylab("Recall")+ggtitle("Without data separation")

g<-ggplot()
g<-p+geom_point(aes(x=precision_table$p_w[precision_table$p_w>0],y=recall_table$r_w[recall_table$r_w>0]),color="blue")
g<-p+geom_point(aes(x=precision_table$p_w_r[precision_table$p_w_r>0],y=recall_table$r_w_r[recall_table$r_w_r>0]),color="red")
g<-p+xlab("Precision")+ylab("Recall")+ggtitle("With data separation")
list_of_results<-list("precision_chart"=p,"recall_chart"=g,"precision_table"=precision_table,"recall_table"=recall_table,"critical_events_precision")
critical_events_precision<-precision_table[precision_table$event=="68917"|precision_table$event=="69481"|precision_table$event=="96731154",]
critical_events_recall<-recall_table[recall_table$event=="68917"|recall_table$event=="69481"|recall_table$event=="96731154",]
list_of_results<-list()
list_of_results<-list("precision_chart"=p,"recall_chart"=g,"precision_table"=precision_table,"recall_table"=recall_table,"critical_events_precision"=critical_events_precision,"critical_events_recall"=critical_events_recall)
names(list_of_results)<-paste(c("alpha=",alpha," / proportion=",zero_proportion))
list_of_results
}

only_test_mega_function<-function(alpha,zero_proportion){
  #NO rare events technique
  models<-list()
  model_table<-list()
  n_time<-system.time(result<-create_models(1,"auc",TRUE,TRUE,FALSE,zero_proportion))
  models<-result$models
  model_table<-result$table
  #rare events technique
  models2<-list()
  model_table2<-list()
  r_time<-system.time(result<-create_models(1,"auc",TRUE,TRUE,TRUE,zero_proportion))
  models2<-result$models
  model_table2<-result$table
  
  comparation_table<-data.frame("event"=model_table$event,"precision_w_test"=model_table$precision,"precision_w_test_r"=model_table2$precision,"recall_w_test"=model_table$recall,"recall_w_test_r"=model_table2$recall,"ones_w_test"=model_table$test_ones,"ones_w_test_r"=model_table2$test_ones)
  comparation_table$precision_w_test<-as.numeric(as.character(comparation_table$precision_w_test))
  comparation_table$precision_w_test_r<-as.numeric(as.character(comparation_table$precision_w_test_r))
  comparation_table$recall_w_test<-as.numeric(as.character(comparation_table$recall_w_test))
  comparation_table$recall_w_test_r<-as.numeric(as.character(comparation_table$recall_w_test_r))
  comparation_table$ones_w_test<-as.numeric(as.character(comparation_table$ones_w_test))
  result<-list("models"=models,"models_r"=models2,"table"=model_table,"table_r"=model_table2,"comp_table"=comparation_table)
}

cross_validation_for_alpha<-function(){ #Voy apuntando los que no: 12
  list_of_models<-list()
  for (i in c(1,3:15,17:46,49:58,60,61,64:71,73:length(predTables$b.30m$f.30m))){
    print(paste(c("Starting event ",names(predTables$b.30m$f.30m)[i])))
    print("____________")
    tryCatch({list_of_models[[i]]<-find_best_model(i)})
  }
  return(list_of_models)
}

tmpb30f30<-cross_validation_for_alpha()
