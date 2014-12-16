#This functions feed from winEvTable, created at log-reg-scf-01-preprocess.R, in the logistic regression folder

library(ROCR)


singleVariableTable<-function(back,forward,predictor,target){
  df<-data.frame(date=winEvTable$back$b.5m$date)
  backTbl<-winEvTable[['back']][[paste('b.',back,'m',sep='')]]
  df$variable<-backTbl[,eval(predictor)]
  fwTbl<-winEvTable[['fw']][[paste('f.',forward,'m',sep='')]]
  df$target<-fwTbl[,eval(target)]
  return(df)
}


calcAUC<-function(predcol,outcol){
  perf<-performance(prediction(predcol,outcol),'auc')
  as.numeric(perf@y.values)
}

positive<-'true'

mkPredC<-function(outCol,varCol,appCol){
  pPos<-sum(outCol==positive)/length(outCol)
  naTab<-table(as.factor(outCol[is.na(varCol)]))
  pPosWna<-(naTab/sum(naTab))[positive]
  vTab<-table(target=as.factor(outCol),predictor=varCol)
  pPosWv<-(vTab[positive,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3)
  pred<-pPosWv[appCol]
  pred[is.na(appCol)]<-pPosWna
  pred[is.na(pred)]<-pPos
  pred
}

singleVariableModels<-function(back=30,forward=30,seed=77777,train_rate=0.9,calibration_rate=0.1,predictors=c('1','2','3','4','5','6','7','8','9','10','11','12'),targets=c('1','3','4','5','6','7','8','9','10','11')){
  interesting_data<-
  for (input in predictors){
    for (output in targets){
      
      #Data Preparation
     
      try({
      df<-singleVariableTable(back,forward,input,output)
      df$variable[df$variable==TRUE]<-'true'
      df$variable[df$variable==FALSE]<-'false'
      df$variable<-factor(df$variable)
      df$target[df$target==TRUE]<-'true'
      df$target[df$target==FALSE]<-'false'
      df$target<-factor(df$target)
      
      set.seed(seed)
      rate<-train_rate
      df$group<-runif(dim(df)[1])
      tbl_train<-subset(df,group<=rate)
      tbl_test<-subset(df,group>rate)
      useForCal<-rbinom(n=dim(tbl_train)[[1]],size=1,prob=calibration_rate)>0
      tbl_cal<-subset(tbl_train,useForCal)
      tbl_train<-subset(tbl_train,!useForCal)
      
      train<-mkPredC(tbl_train$target,tbl_train$variable,tbl_train$variable)
      calibration<-mkPredC(tbl_train$target,tbl_train$variable,tbl_cal$variable)
      test<-mkPredC(tbl_train$target,tbl_train$variable,tbl_test$variable)
      
      aucTrain<-calcAUC(train,tbl_train$target)
      aucCal<-calcAUC(calibration,tbl_cal$target)
      aucTest<-calcAUC(test,tbl_test$target)
      
      #if(aucTrain>0.551){
        print(paste('Predictor:',input,', Output:',output,sep=""))
        print(sprintf("trainAUC:%4.3f",aucTrain)) 
        print(sprintf("calAUC:%4.3f",aucCal))
        print(sprintf('testAUC:%4.3f',aucTest))
        #Confusion Matrix Creation and Model Printing
        confusion_matrix<- table(predictor=tbl_train[,'variable'],target=tbl_train[,'target'])
        print("Confusion matrix:")
        print(confusion_matrix)
        print("Altered confusion matrix:")
        print(100*confusion_matrix[,2]/(confusion_matrix[,1]+confusion_matrix[,2]))
        print("-------------") 
      #}
      
      
      })
      }
    }
}







