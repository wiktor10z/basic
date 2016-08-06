mean1=function(x){
  return (sum(x)/sum(x!=0))
}

read_ml_file=function(file){
  ml=read.csv(file,header=FALSE,sep="\t")
  ml_usermax=max(ml100k[,1])
  ml_moviemax=max(ml100k[,2])
  ml_matrix1=matrix(0L,nrow=ml100k_usermax,ncol=ml100k_moviemax)
  for(i in 1:100000){
    ml_matrix1[ml100k[i,1],ml100k[i,2]]=ml100k[i,3]
  }
  ml_matrix<<-ml_matrix1
  users<<-ml_usermax
  movies<<-ml_moviemax
  mov_means<<-apply(ml_matrix,2,mean1)
  us_means<<-apply(ml_matrix,1,mean1)
}

#system.time({
#  ml100k_SVD = svd(ml100k_matrix)
#})

