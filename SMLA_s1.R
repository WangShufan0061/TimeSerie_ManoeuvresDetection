SMLA_s1<-function(timeseries, threshold,method_cor){
  #calculate the correlation matrix
  cor_matrix <- matrix(1:36, nrow = 6, dimnames = list(c("ecc","aop","inc","ma","bmm","ra"), c("ecc","aop","inc","ma","bmm","ra")))
  for (i in 1:6){
    for (j in 1:6){
      cor_matrix[i,j]<-cor(timeseries[,i+1],timeseries[,j+1] , method =method_cor)
    }
  }
  #calculate the Adjencecy matrix
  diag(cor_matrix)<-NA
  cor_v= sort(c(cor_matrix),na.last=NA)
  result = which(abs(cor_matrix) < threshold, arr.ind=TRUE)
  A<-cor_matrix
  A[result]=0
  #Louvain algorithm to cluster
  g1<-graph_from_adjacency_matrix( abs(A), weighted=T, mode="undirected", diag=F)
  cluster_s1=cluster_louvain(g1)
  return(cluster_s1)
}