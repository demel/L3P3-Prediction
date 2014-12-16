scores<-vector()
for (i in 1:length(models_sc)){
  scores[i]<-models_sc[[i]]$fscore
}

precisions<-vector()
for (i in 1:length(models_sc)){
  precisions[i]<-models_sc[[i]]$precision
}

recalls<-vector()
for (i in 1:length(models_sc)){
  recalls[i]<-models_sc[[i]]$recall
}

library(ggplot2)

ggplot()+geom_point(aes(x=precisions,y=recalls),size=3)+xlab("Precision")+ylab("Recall")


load("resources.Rdata")
load("res_list.Rdata")
items<-as.character(unique(res$item))
appearances<-tbl_df(data.frame((matrix(0,ncol=3,nrow=length(items)))))
names(appearances)<-c("appearances","non_zero_mean","non_zero_var")
rownames(appearances)<-items
for (i in 1:length(models_sc)){
  if (!is.null(models_sc[[i]]$model)){
  coef_names<-models_sc[[i]]$model$glmnet.fit$beta@Dimnames[1][[1]]
  coef_names<-coef_names[84:length(coef_names)]
  coef_numbers<-as.numeric(coef(models_sc[[i]]$model))
  coef_numbers<-coef_numbers[84:length(coef_numbers)]
  for (j in 1:length(items)){
    item<-gsub(pattern = "-",replacement = ".",x = items[j])
    coefs<-grep(pattern = item, x=coef_names,fixed=TRUE)
    if (length(coefs)==2){
      appearances$appearances<-appearances$appearances+1
      if (coef_numbers[coefs[1]]!=0){appearances$non_zero_mean[j]<-appearances$non_zero_mean[j]+1}
      if (coef_numbers[coefs[2]]!=0){appearances$non_zero_var[j]<-appearances$non_zero_var[j]+1}
    }
    if (length(coefs)>2){
      appearances$appearances<-appearances$appearances+1
      if (coef_numbers[coefs[1]]!=0){appearances$non_zero_mean[j]<-appearances$non_zero_mean[j]+1}
      if (coef_numbers[coefs[2]]!=0){appearances$non_zero_mean[j]<-appearances$non_zero_mean[j]+1}
      if (coef_numbers[coefs[3]]!=0){appearances$non_zero_var[j]<-appearances$non_zero_var[j]+1}
      if (coef_numbers[coefs[4]]!=0){appearances$non_zero_var[j]<-appearances$non_zero_var[j]+1}
    }
  }
  }
}


