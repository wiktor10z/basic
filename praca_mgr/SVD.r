init_SVD=function(f2,alpha2){
  f<<-f2
  alpha<<-alpha2
  us_viewed_root1<<-lapply(us_viewed,FUN=function(x){return(1/sqrt(x))})
  r<<-ml_matrix
  #r2<<-matrix(0L,nrow=users,ncol=movies)
  p<<-matrix(rnorm(users*f,mean=0,sd=1),users,f)
  q1<<-matrix(0,movies,f)
  y<<-matrix(rnorm(movies*f,mean=0,sd=1),movies,f)
  b<<-rep(0L,users)
  b2<<-rep(0L,movies)
}


SVD_item=function(u,i){
  r2=glob_mean+b[u]+b2[i]+sum(q1[i,]*p[u,])
  err=r[u,i]-r2
  b[u]<<-b[u]+alpha*err
  b2[i]<<-b2[i]+alpha*err
  p[u,]<<-p[u,]+alpha*(err*q1[i])
  q1[i,]<<-q1[i,]+alpha*(err*p[u,])
}
SVDpp_item=function(u,i){
  sum_y=us_viewed_root1[[u]]*colSums(y*ml_bin_matrix[u,])
  p_plus_y=sum_y+p[u,]
  r2=glob_mean+b[u]+b2[i]+sum(q1[i,]*p_plus_y)
  err=r[u,i]-r2
  b[u]<<-b[u]+alpha*err
  b2[i]<<-b2[i]+alpha*err
  p[u,]<<-p[u,]+alpha*(err*q1[i,])
  q1[i,]<<-q1[i,]+alpha*(err*p_plus_y)
  for(j in us_view_list[u]){
    y[j,]<<-y[j,]+alpha*(err*us_viewed_root1[[u]]*q1[i,])
  }  
}

# rozkłady SVD

SVD=function(Iter,f2,alpha2){
  init_SVD(f2,alpha2)
  for(I in 1:Iter){
    apply(ml_bin,1,FUN=function(x){SVD_item(x[1],x[2])})    
  }
}

SVDpp=function(Iter,f2,alpha2){
  init_SVD(f2,alpha2)
  for(I in 1:Iter){
    apply(ml_bin,1,FUN=function(x){SVDpp_item(x[1],x[2])})    
  }
}

BPR=function(Iter,f2,alpha2){
  init_SVD(f2,alpha2)
  for(I in 1:Iter){
    u=sample(1:users,1)
    i=sample(c(1:items)[ml_bin_matrix[u,]],1)
    j=sample(c(1:items)[ml_bin_matrix[u,]==FALSE],1)
    s2=b2[j]-b2[i]+sum(p[u,]*(q1[j,]-q1[i,]))
    err=exp(s2)/(1+exp(s2))
    b2[i]<<-b2[i]+alpha*err
    b2[j]<<-b2[j]-alpha*err
    p[u,]<<-p[u,]+alpha*err*(q1[i,]-q1[j,])
    q1[i,]<<-q1[i,]+alpha*err*p[u,]
    q1[j,]<<-q1[j,]-alpha*err*p[u,]
  }
}

# ratings

SVD_ratings=function(Iter,f2,alpha2){
  SVD(Iter,f2,alpha2)
  r2=matrix(0L,nrow=users,ncol=movies)
  for(u in 1:users){
    for(i in 1:items){
      r2[u,i]=glob_mean+b[u]+b2[i]+sum(q1[i,]*p[u,])
    }
  }
  return(r2)
}

SVDpp_ratings=function(Iter,f2,alpha2){
  SVDpp(Iter,f2,alpha2)
  r2=matrix(0L,nrow=users,ncol=movies)
  for(u in 1:users){
    sum_y=us_viewed_root1[[u]]*colSums(y*ml_bin_matrix[u,])
    p_plus_y=sum_y+p[u,]
    for(i in 1:items){
      r2[u,i]=glob_mean+b[u]+b2[i]+sum(q1[i,]*p_plus_y)
    }
  }
  return(r2)
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
SVD1=function(Iter,f,N){
  alpha<<-0.03
  r<<-ml_matrix
  #r2<<-matrix(0L,nrow=users,ncol=movies)
  p<<-matrix(rnorm(users*f,mean=0,sd=1),users,f)
  q1<<-matrix(0,movies,f)#TODO może jednak jakoś inaczej zainicjować
  b<<-rep(0L,users)
  b2<<-rep(0L,movies)
  for(I in 1:Iter){
    for(j in 1:N){
      if((sum(is.nan(p))==0)&&(sum(is.infinite(p))==0)&&(sum(p>10e+10)==0)&&(sum(p<(-10e+10))==0)){
        j2<<-j
       SVD_item(ml_bin[j,1],ml_bin[j,2])       
      }
    }
    #apply(ml_bin[1:N,],1,FUN=function(x){SVD_item(x[1],x[2])})
  }
 #for(u in 1:users){
  #  for(i in 1:items){
   #   r2[u,i]=glob_mean+b[u]+b2[i]+sum(q1[i,]*p[u,])
    #}
  #}
}

f=3
alpha=0.1
us_viewed_root1=apply(us_viewed,FUN=function(x){return(1/sqrt(x))})
pprim=matrix(rnorm(users*f,mean=0,sd=1),users,f)
yprim=matrix(rnorm(movies*f,mean=0,sd=1),movies,f)
{
  #init
  r=ml_matrix
  r2=matrix(0L,nrow=users,ncol=movies)
  p=pprim
  q1=matrix(0,movies,f)#TODO może jednak jakoś inaczej zainicjować
  y=yprim
  b=rep(0L,users)
  b2=rep(0L,movies)
}

}