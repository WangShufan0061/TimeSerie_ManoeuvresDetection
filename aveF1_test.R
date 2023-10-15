aveF1_test<-function(obs,pre,date){
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
  #print(anomaly_index_pre)
  #print(anomaly_index_obs)
  # calculate the weighted F1 score
  minor<-confusionMatrix(factor(anomaly_index_pre), factor(anomaly_index_obs), mode = "everything", positive="1")
  #print(minor)
  major<-confusionMatrix(factor(anomaly_index_pre), factor(anomaly_index_obs), mode = "everything", positive="0")
  minorF1<-minor$byClass[7]
  majorF1<-major$byClass[7]
  weighted_F1<-sum(anomaly_index_obs==0)/length(anomaly_index_obs)*majorF1+sum(anomaly_index_obs==1)/length(anomaly_index_obs)*minorF1
 # print("minor",minorF1,"major",majorF1,"ave",weighted_F1)
  return(list(minorF1,majorF1,weighted_F1))
}