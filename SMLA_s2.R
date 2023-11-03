SMLA_s2<-function(group,threshold,window_length,method_cor){
  #get windows
  n_windows=ceiling(nrow(group)/window_length)
  windows_norepeat<-list()
  for (i in 0:(n_windows-2)){
    windows_norepeat[i+1]<- list(window(ts(group),start = window_length*i+1, end = window_length*(i+1)))
  }
    windows_norepeat[n_windows] <- list(window(ts(group),start = window_length*(n_windows-1)+1, end = nrow(group)))
  #calculate the correlation
  n_ts=ncol(group)-1
  cor_matrix_w <- matrix(1:n_ts^2, nrow = n_ts)
  cluster_w <-matrix(0, nrow = n_windows,ncol=n_ts)
  for (i in 1:n_windows){
    for (j in 2:(n_ts+1)){#The first column is time
      for (k in 2:(n_ts+1)){
        cor_matrix_w[j-1,k-1]<-abs(cor(windows_norepeat[[i]][,j],windows_norepeat[[i]][,k], method =method_cor))
      }
    }
    #print(cor_matrix_w)
    # choose threshold2 to creat the adjacency matrix
    diag(cor_matrix_w)<-NA
    cor_v= sort(c(cor_matrix_w),na.last=NA)
    result = which(abs(cor_matrix_w) < threshold, arr.ind=TRUE)
    A<-cor_matrix_w
    A[result]=0

    #graph
    graph<-graph_from_adjacency_matrix( abs(A), weighted=T, mode="undirected", diag=F)
    if (is.nan(cluster_louvain(graph)$modularity)){
      cluster_w[i,]=rep(1:n_ts,1)
    }
    else{
      cluster_w[i,]=c(cluster_louvain(graph)$memberships)
    }
  }
  #list_return <-list()
  return(list(cluster_w,windows_norepeat))
}