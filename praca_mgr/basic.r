mean1=function(x){
  return (sum(x)/sum(x!=0))
}
# wczytanie
read_ml_file=function(file){
  ml<<-read.csv(file,header=FALSE,sep="\t")
  ml_bin<<-ml[,1:2]
  ml_usermax=max(ml[,1])
  ml_moviemax=max(ml[,2])
  ml_matrix1=matrix(0L,nrow=ml_usermax,ncol=ml_moviemax)
  for(i in 1:100000){
    ml_matrix1[ml[i,1],ml[i,2]]=ml[i,3]
  }
  ml_matrix<<-ml_matrix1
  ml_bin_matrix<<-(ml_matrix!=0)
  
  us_view_list<<-list(list())
  us_viewed<<-list()  
  for(i in 1:ml_usermax){
    us_view_list[[i]]<<-which(ml_matrix[i,]!=0)
    us_viewed[i]<<-length(us_view_list[[i]])
  }
  
  users<<-ml_usermax
  movies<<-ml_moviemax
  observations<<-nrow(ml)
  mov_means<<-apply(ml_matrix,2,mean1)
  us_means<<-apply(ml_matrix,1,mean1)
  glob_mean<<-mean(mov_means)
}


#system.time({
#  ml100k_SVD = svd(ml100k_matrix)
#})

remove_viewed=function(x,y){
  return ((x==0)*y)
}
# non-personalized
non_personalized=function(u,n){
  return(head(order(remove_viewed(ml_matrix[u,],mov_means),decreasing=TRUE),n))
}

non_personalized_recs=function(n){
  lapply(1:users,function(u) non_personalized(u,n)) 
}

# przekształcenie
rating_to_propos1=function(u,n){
  return(head(order(remove_viewed(ml_matrix[u,],predicted_ratings[u,]),decreasing=TRUE),n))
}
rating_to_propos=function(ratings,n){
  predicted_ratings<<-ratings
  lapply(1:users,function(u) rating_to_propos1(u,n))
}

