source("basic.r")
source("CF_functions.R")
source("SVD++.r")
read_ml_file("ml-100k/u.data")
mean(mov_means)
non_per_rating=rep(1,users)%*%t.default(mov_means)
trivial_rating=((rep(1,users)%*%t.default(mov_means))+(us_means%*%t.default(rep(1,items))))/2
non_per_propos=non_personalized_recs(10)
system.time({
  cor_matrix=make_sim_matrix(cor_similarity)
  cos_matrix=make_sim_matrix(cos_similarity)
})
system.time({
  CF_predicted_ratings=CF_predict_all(cor_matrix)
})
CF_propos=rating_to_propos(CF_predicted_ratings,10)

SVD(1,10,0.03)
SVDpp(1,3,0.01)


#BPR

BPR=function(Iter,f2,alpha2){
  f<<-f2
  alpha<<-alpha2
  r<<-ml_bin_matrix
  p<<-matrix(rnorm(users*f,mean=0,sd=1),users,f)
  q1<<-matrix(0,movies,f)
  b2<<-rep(0L,items)
  for(I in 1:Iter){
    u=sample(1:users,1)
    i=sample(c(1:items)[ml_bin_matrix[u,]],1)
    j=sample(c(1:items)[ml_bin_matrix[u,]==FALSE],1)
    s2=b[j]-b[i]+sum(p[u,]*(q1[j,]-q1[i,]))
    err=exp(s2)/(1+exp(s2))
    b2[i]<<-b2[i]+alpha*err
    b2[j]<<-b2[j]-alpha*err
    p[u,]<<-p[u,]+alpha*err*(q1[i,]-q1[j,])
    q1[i,]<<-q1[i,]+alpha*err*p[u,]
    q1[j,]<<-q1[j,]-alpha*err*p[u,]
  }
}
BPR_pseudo_ratings=function(Iter,f2,alpha2){
  BPR(Iter,f2,alpha2)
  r2=matrix(0L,nrow=users,ncol=movies)
  for(u in 1:users){
    for(i in 1:items){
      r2[u,i]=b2[i]+sum(q1[i,]*p[u,])
    }
  }
  return(r2)
}

if(FALSE){
f=3
pprim=matrix(rnorm(users*f,mean=0,sd=1),users,f)

  #init
  alpha=0.1
  p=pprim
  q=matrix(0,movies,f)#TODO może jednak jakoś inaczej zainicjować
  b2=rep(0L,items)



for(I in 1:10000){
  u=sample(1:users,1)
  i=sample(c(1:items)[ml_bin_matrix[u,]],1)
  j=sample(c(1:items)[ml_bin_matrix[u,]==FALSE],1)
  s2=b[j]-b[i]+sum(p[u,]*(q[j,]-q[i,]))
  err=exp(s2)/(1+exp(s2))
  b2[i]=b2[i]+alpha*err
  b2[j]=b2[j]-alpha*err
  p[u,]=p[u,]+alpha*err*(q[i,]-q[j,])
  q[i,]=q[i,]+alpha*err*p[u,]
  q[j,]=q[j,]-alpha*err*p[u,]
}
  
  l5=0
  for(I in 1:10000){
    u=sample(1:users,1)
    i=sample(c(1:items)[ml_bin_matrix[u,]],1)
    j=sample(c(1:items)[ml_bin_matrix[u,]==FALSE],1)
    s2=b2[j]-b2[i]+sum(p[u,]*(q[j,]-q[i,]))
   if(s2<0){
     l5=l5+1
   } 
  }
l5
}
  