model_eval<-function(obs,pre,date){
  # convert the pre to 0 or 1
  window_index=sort(as.data.frame(which(pre[]!=rep(1,ncol(pre)),arr.ind=TRUE))$row)
  anomaly_index = unique(window_index)
  anomaly_index_pre <- rep(0,nrow(pre))
  anomaly_index_pre[anomaly_index]=1
  # convert the obs to 0 or 1
  interval <- matrix(0, nrow = nrow(pre), ncol = 2)
  for (i in 1:nrow(pre)){
    interval[i,1]=obs[[i]][1,1]
    interval[i,2]=obs[[i]][nrow(obs[[i]]),1]
  }
  anomaly_index_obs <- rep(0,nrow(interval))
  for (i in 1: nrow(interval)){
    current <- interval[i,]
    if (sum(current[1]<median_date &median_date<current[2])){
      anomaly_index_obs[i] <- 1
    }
  }
  # calculate the F1 score, recall and precision for class "1"
  minor<-confusionMatrix(factor(anomaly_index_pre), factor(anomaly_index_obs), mode = "everything", positive="1")
  F1_minor<- minor$byClass[7]
  recall_minor <- minor$byClass[6]
  precision_minor <- minor$byClass[5]
  return(c(F1_minor,recall_minor,precision_minor))
}