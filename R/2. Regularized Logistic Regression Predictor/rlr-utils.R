result<-data.frame(matrix(nrow=length(tmp),ncol=4))
names(result)<-c("best_alpha","precision","recall","fscore")
for (i in c(1:11,13:50,52:length(tmp))){
  result$best_alpha[i]<-tmp[[i]]$best_alpha
  result$precision[i]<-tmp[[i]]$precision
  result$recall[i]<-tmp[[i]]$recall
  result$fscore[i]<-tmp[[i]]$fscore
}

# Voy apuntando los que no: 2 (sólo sucede 2 veces), 16 (sucede 1 vez),47 (sucede 3 veces)
# 48 (sucede 2 veces),59(sucede 2 veces),62 (3 veces),63 (1 vez), 72(3 veces)
#c(1,3:15,17:46,49:58,60,61,64:71,73:length(predTables$b.5m$f.5m))
resultb5f5<-result
resultb30f5<-result

#Fallan el 2,16,51
resultb5f30<-result
#Fallan el 12,51
resultb30f30<-result




info<-function(data){
  model_length<-dim(data)[1]
  non_zero<-length(which(data$fscore>0))
  precision_mean<-mean(data$precision[data$fscore>0],na.rm=TRUE)
  recall_mean<-mean(data$recall[data$fscore>0],na.rm=TRUE)
  fscore_mean<-mean(data$fscore[data$fscore>0],na.rm=TRUE)
  fscore_dev<-sd(data$fscore[data$fscore>0],na.rm=TRUE)
  return(c(model_length,non_zero,precision_mean,recall_mean,fscore_mean,fscore_dev))
}