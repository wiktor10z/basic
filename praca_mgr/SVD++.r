
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

SVD=function(Iter,f2,alpha2){
  f<<-f2
  alpha<<-alpha2
  r<<-ml_matrix
  #r2<<-matrix(0L,nrow=users,ncol=movies)
  p<<-matrix(rnorm(users*f,mean=0,sd=1),users,f)
  q1<<-matrix(0,movies,f)
  b<<-rep(0L,users)
  b2<<-rep(0L,movies)
  for(I in 1:Iter){
    apply(ml_bin,1,FUN=function(x){SVD_item(x[1],x[2])})    
  }
  #for(u in 1:users){
    #  for(i in 1:items){
      #r2[u,i]=glob_mean+b[u]+b2[i]+sum(q1[i,]*p[u,])
    #}
  #}
}
SVDpp=function(Iter,f2,alpha2){
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
  for(I in 1:Iter){
    apply(ml_bin,1,FUN=function(x){SVDpp_item(x[1],x[2])})    
  }
  #for(u in 1:users){
  #for(i in 1:items){
  #r2[u,i]=glob_mean+b[u]+b2[i]+sum(q1[i,]*p[u,])
  #}
  #}
}

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
#TODO obcięcie do <min,max>


#inits
if(FALSE){
SVD1=function(Iter,f,N){
  alpha<<-0.07
  r<<-ml_matrix
  #r2<<-matrix(0L,nrow=users,ncol=movies)
  p<<-matrix(rnorm(users*f,mean=0,sd=1),users,f)
  q1<<-matrix(0,movies,f)#TODO może jednak jakoś inaczej zainicjować
  b<<-rep(0L,users)
  b2<<-rep(0L,movies)
  for(I in 1:Iter){
    for(j in 1:N){
      if((sum(is.nan(p))==0)&&(sum(is.infinite(p))==0)&&(sum(p>10e+10)==0)&&(sum(p<(-10e+10))==0)){
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
pprim=matrix(rnorm(users*f,mean=0,sd=1),users,f)
yprim=matrix(rnorm(movies*f,mean=0,sd=1),movies,f)
{
us_viewed_root1=apply(us_viewed,FUN=function(x){return(1/sqrt(x))})
f=3
#r[u,i]=ml_matrix[u,i] lub ml_bin_matrix[u,i]
r=ml_matrix
r2=matrix(0L,nrow=users,ncol=movies)
b=rep(0L,users)
b2=rep(0L,movies)
p=matrix(0.001,nrow=users,ncol=f)
q=matrix(0.001,nrow=movies,ncol=f)
y=matrix(0.001,nrow=movies,ncol=f)
alpha=0.1
SVDpp_item0=function(u,i){#to jest dla f=1
  sum_y=us_viewed_root1[[u]]*sum(y*ml_bin_matrix[u,])
  r2[u,i]<<-glob_mean+b[u]+b2[i]+q[i]*(p[u]+sum_y)
  err=r[u,i]-r2[u,i]
  b[u]<<-b[u]+alpha*err
  b2[i]<<-b2[i]+alpha*err
  p[u]<<-p[u]+alpha*(err*q[i])
  q[i]<<-q[i]+alpha*(err*(p[u]+sum_y))
  for(j in us_view_list[u]){
    y[j]<<-alpha*(err*us_viewed_root1[[u]]*q[i])
  }
}
apply(ml_bin,1,FUN=function(x){SVDpp_item(x[1],x[2])})
}
{
pprim=matrix(rnorm(users*f,mean=0,sd=1),users,f)

alpha=0.1
f=4
r=ml_matrix
r2=matrix(0L,nrow=users,ncol=movies)
p=pprim
q=matrix(0,movies,f)#TODO może jednak jakoś inaczej zainicjować
b=rep(0L,users)
b2=rep(0L,movies)
}
{
  #init
  alpha=0.1
  us_viewed_root1=lapply(us_viewed,FUN=function(x){return(1/sqrt(x))})
  f=3
  r=ml_matrix
  r2=matrix(0L,nrow=users,ncol=movies)
  p=pprim
  q1=matrix(0,movies,f)#TODO może jednak jakoś inaczej zainicjować
  y=yprim
  b=rep(0L,users)
  b2=rep(0L,movies)
}

}