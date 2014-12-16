##We now work with the third iteration of the node aware elastic net, added with node resource consumption.
#In this final test, we add as continous variable every resource data of each item in the node we are predicting.
#We will use the breakout detection package to divide each time series into its mean and its deviation over the mean.


library(plyr)
library(dplyr)
require(reshape2)
require(lubridate)
require(glmnet)

extractNodeName<-function(str){
  splitted<-strsplit(str,"-")
  if (length(splitted[[1]])>2){
    res<-paste(splitted[[1]][1],splitted[[1]][2],sep="-") #we assume no name nodes with more than two hyphens
  }
  else{
    res<-splitted[[1]][1]
  }
  return (res)
}

createResVector<-function(window=300,ev_date,vec){ #IMPORTANT NOTE this function is crucial to the model.
  result<-vector("numeric",length(ev_date))        #It takes the resource value in the observation window
  for (i in 1:length(ev_date)){                    #If there are no values in the window, it takes the closest one
    windowOrigin<-ev_date[i]                       #If there is more than one value, it averages them
    data.filtered<-vec %.% filter (date<windowOrigin & date >= windowOrigin-window)
    if (dim(data.filtered)[1]==0){
      closest_index<-which(abs(vec$date-windowOrigin)==min(abs(vec$date-windowOrigin)))[1]
      result[i]<-vec$value[closest_index]
    }
    else {if (dim(data.filtered)[1]>1){
      result[i]<-mean(data.filtered$value) 
    }
    else{
      result[i]<-data.filtered$value
    }
    }
  }
  return(result)
}

breakout_res<-function(vec){
  require(BreakoutDetection)
  breakout_model<-breakout(vec)
  changes<-breakout_model$loc
  mean_vec<-vector()
  variation_vec<-vector()
  if(length(changes)>0){
    locs<-c(1,changes,length(vec))
    for (i in 2:length(locs)){
      res_part<-vec[locs[i-1]:locs[i]]
      mean_vec<-c(mean_vec,rep(mean(res_part),times=length(res_part)))
      variation_vec<-c(variation_vec,res_part-mean(res_part))
      if (i!=length(locs)){
        mean_vec<-mean_vec[-locs[i]]
        variation_vec<-variation_vec[-locs[i]]
      }
    }
  }
  else{
    mean_vec<-rep(mean(vec),length(vec))
    variation_vec<-vec-mean_vec
  }
  return(list(mean_vec,variation_vec))
}

create_res_df<-function(res_list,dates){
  #create column names
  columns<-vector()
  vectors<-list()
  vectors_mean<-list()
  vectors_var<-list()
  k<-1
  for (i in 1:length(res_list)){
    print(i)
    columns<-c(columns,paste(names(res_list)[i],names(res_list[[i]]),sep="-"))
    for (j in 1:length(res_list[[i]])){
      print(j)
      res_vec<-res_list[[i]][[j]]
      vectors[[k]]<-createResVector(ev_date=tables$back$date,vec=res_vec)
      res_divided<-breakout_res(vectors[[k]])
      vectors_mean[[k]]<-res_divided[[1]]
      vectors_var[[k]]<-res_divided[[2]]
      k<-k+1
    }
  }
  names(vectors)<-columns
  names(vectors_mean)<-paste(columns,"mean",sep="-")
  names(vectors_var)<-paste(columns,"var",sep="-")
  result<-tbl_df(data.frame(vectors_mean,vectors_var))
  return(result)
}

prune_and_cast<-function(df){
  df<-df[,-4]
  names(df)<-c("node","item","date","value","resource")
  df$date<-dmy_hms(df$date)
  cast_res<-tbl_df(dcast(df,node+item+date~resource))
  names(cast_res)<-c("node","item","date","av_cpu","bw_in","bw_out","cpu","mem","disk","phy_mem")
  return(cast_res)
}

load_resources<-function(){
  res_abril<-tbl_df(read.table('C:/Users/JM/Documents/experiment-data/parsed/04.Abril.csv',sep=","))
  res_mayo<-tbl_df(read.table('C:/Users/JM/Documents/experiment-data/parsed/05.Mayo.csv',sep=","))
  res_junio<-tbl_df(read.table('C:/Users/JM/Documents/experiment-data/parsed/06.Junio.csv',sep=","))
  res_julio<-tbl_df(read.table('C:/Users/JM/Documents/experiment-data/parsed/07.Julio.csv',sep=","))
  res_agosto<-tbl_df(read.table('C:/Users/JM/Documents/experiment-data/parsed/08.Agosto.csv',sep=","))
  res<-rbind(res_abril,res_mayo,res_junio,res_julio,res_agosto)
  
  res<-res[,-4]
  names(res)<-c("node","item","date","value","resource")
  res$date<-dmy_hms(res$date)
  nodes<-unique(res$node)
  res_list<-vector("list", length(nodes))
  names(res_list)<-nodes
  for (i in 1:length(res_list)){
    items<-unique(res$item[res$node==nodes[i]])
    res_list[[i]]<-vector("list", length(items))
    names(res_list[[i]])<-items
    for (j in 1:length(names(res_list[[i]]))){
      resources<-as.character(unique(res$resource[res$node==nodes[i]&res$item==names(res_list[[i]])[j]]))
      res_list[[i]][[j]]<-vector("list", length(resources))
      names(res_list[[i]][[j]])<-resources
      for (k in 1:length(resources)){
        res_list[[i]][[j]][[k]]<-res[(res$node==nodes[i])&(res$item==items[j])&(res$resource==resources[k]),]
      }
    }
  }
  return(res_list)
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
  levels(data$Severity)[levels(data$Severity)==""]<-"Blank"
  data$Event_Type<-factor(data$Event_Type)
  data<-data.frame("date"=data$Created_On,"node"=droplevels(data$Name),"type"=droplevels(data$Event_Type))
  if (!simple){data$type<-factor(paste(data$node,data$type,sep="-"))}
  data<-data %.% tbl_df() %.% select(date,type)
  data$date<-dmy_hms(data$date)
  data<-data[order(data$date),]
  return(data)
}

extractNodes<-function(){
  data_april<-read.table('C:/Users/JM/Documents/experiment-data/parsed/04.Abril-events.csv',sep=",")
  data_may<-read.table('C:/Users/JM/Documents/experiment-data/parsed/05.Mayo-events.csv',sep=",")
  data_june<-read.table('C:/Users/JM/Documents/experiment-data/parsed/06.Junio-events.csv',sep=",")
  data_july<-read.table('C:/Users/JM/Documents/experiment-data/parsed/07.Julio-events.csv',sep=",")
  data_august<-read.table('C:/Users/JM/Documents/experiment-data/parsed/08.Agosto-events.csv',sep=",")
  data<-rbind(data_april,data_may,data_june,data_july,data_august)
  names(data)<-c("Severity","Created_On","Name","Event_Type","Event","Created_By","Cleared_On","Cleared_By","Model_Type_Name","Event_Precedence")  
  return(tolower(unique(data$Name)))
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
    input_matrix<-model.matrix(target~.-date,complete_data$training)
    #input_matrix<-matrix(nrow=dim(input)[1],ncol=dim(input)[2])
    #input_matrix[which(input==TRUE)]<-1
    #input_matrix[which(input==FALSE)]<-0
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
  
  input_matrix<-model.matrix(target~.-date,complete_data$validation)
  #input_matrix<-matrix(nrow=dim(input)[1],ncol=dim(input)[2])
  #input_matrix[which(input==TRUE)]<-1
  #input_matrix[which(input==FALSE)]<-0
  
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
  
  input_matrix<-model.matrix(target~.-date,complete_data$test)
  #   input_matrix<-matrix(nrow=dim(input)[1],ncol=dim(input)[2])
  #   input_matrix[which(input==TRUE)]<-1
  #   input_matrix[which(input==FALSE)]<-0
  
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
#res<-load_resources()
#load("resources.Rdata")
tables<-getTables(data_simple,data_complex)
models_sc<-vector("list",ncol(tables$fw)-1)
names(models_sc)<-names(tables$fw)[2:ncol(tables$fw)]
# nodes<-extractNodes()
# res_tables<-vector("list",length=length(nodes))
# j<-1
# for (i in 1:length(nodes)){
#   print(paste(i,"/",length(nodes)))
#   res_index<-which(tolower(names(res))==tolower(nodes[i]))
#   if (length(res_index)>0){
#     names(res_tables)[j]<-nodes[i]
#     res_tables[[j]]<-create_res_df(res[[res_index]],dates=tables$back$date)
#     j<-j+1
#   }
# }
for (i in 1:(ncol(tables$fw)-1)){
#for (i in 1:20){
  node_name<-extractNodeName(names(tables$fw)[i+1])
  res_index<-which(names(res_tables)==tolower(node_name))
  if (length(res_index)>0){
    res_df<-res_tables[[res_index]]
    predTable<-cbind(tables$back,res_df,tables$fw[,i+1])
  }
  else{predTable<-cbind(tables$back,tables$fw[,i+1])}#If there were no events in a node
  names(predTable)[length(names(predTable))]<-"target"
  print(paste(c("Starting event ",names(tables$fw[i+1]))))
  print("____________")
  models_sc[[i]]<-find_best_model_trycatch(predTable)
} ##Started at 20:34