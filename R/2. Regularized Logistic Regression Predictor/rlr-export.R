models_list<-list()
#Extract coefficients
for (i in 1:length(tmpb5f5)){
  if(is.null(tmpb5f5[[i]])){}
    else{
    if (is.na(tmpb5f5[[i]]$fscore)){}
    else{  
      if (tmpb5f5[[i]]$fscore<0.5){
          #do nothing    
      }
      else{
          coef_list<-list()
          target_event<-names(predTables$b.5m$f.5m)[[i]]
          event_numbers<-c("Intercept",names(predTables$b.5m$f.5m[[i]])[2:(length(predTables$b.5m$f.5m[[i]])-1)])
          for (j in 1:length(coef(tmpb5f5[[i]]$model))){
            coef_list[[j]]<-c(event_numbers[j],coef(tmpb5f5[[i]]$model)[j])
          }
          models_list[[i]]<-coef_list
          names(models_list)[i]<-target_event
      }
  }
}
}
#Create output matrix
column_number<-0
i<-0
while (column_number==0){
  i<-i+1
  column_number<-length(models_list[[i]])
}
right_index<-i
model_matrix<-data.frame(matrix(nrow=length(which(names(models_list)!="")),ncol=column_number))
rownames(model_matrix)<-names(models_list)[names(models_list)!=""]
column_names<-character()
for (i in 1:length(models_list[[right_index]])){
  column_names<-c(column_names,models_list[[right_index]][[i]][1])
}
colnames(model_matrix)<-column_names

#populate output matrix
for (i in 1:nrow(model_matrix)){
  events<-numeric()
  for (k in 1:length(models_list[[1]])){
    events<-c(events,models_list[[right_index]][[k]][1])
  }
  for (j in 1:ncol(model_matrix)){
    model_number<-which(names(models_list)==rownames(model_matrix)[i])
    list_number<-which(events==names(model_matrix)[j])
    model_matrix[i,j]<-models_list[[model_number]][[list_number]][2]
  }
}

# Change event names for their hexadecimal ones
rownames(model_matrix)<-as.hexmode(as.numeric(rownames(model_matrix)))
