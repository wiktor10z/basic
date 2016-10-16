genres_norm=function(x){
  y=sum(x)
  if(y==0){
    return(0)
  }else{
    return(1/y)
  }
}

init_SVD=function(f2,alpha2){
  f<<-f2
  alpha<<-alpha2
  us_viewed_root1<<-unlist(lapply(us_viewed,FUN=function(x){return(1/sqrt(x))}))
  item_genres_norm1<<-apply(item_genres,1,genres_norm)
  r<<-ml_matrix
  p<<-matrix(rnorm(users*f,mean=0,sd=1),users,f)
  q1<<-matrix(rnorm(items*f,mean=0,sd=1),items,f)
  x<<-matrix(rnorm(genres*f,mean=0,sd=1),genres,f)
  y<<-matrix(rnorm(items*f,mean=0,sd=1),items,f)
  b<<-rep(0L,users)
  b2<<-rep(0L,items)
}
#TODO dodać te stałe w algorytmach - nawet tylko te z mymedialite i przetestować czy to coś daje
SVD_item=function(u,i){
  r2=glob_mean+b[u]+b2[i]+sum(q1[i,]*p[u,])
  err=r[u,i]-r2
  b[u]<<-b[u]+alpha*err
  b2[i]<<-b2[i]+alpha*err
  p[u,]<<-p[u,]+alpha*(err*q1[i])
  q1[i,]<<-q1[i,]+alpha*(err*p[u,])
}
SVDpp_item=function(u,i){
  y_sum=us_viewed_root1[u]*colSums(y*ml_bin_matrix[u,])
  p_plus_y=p[u,]+y_sum
  r2=glob_mean+b[u]+b2[i]+sum(q1[i,]*p_plus_y)
  err=r[u,i]-r2
  b[u]<<-b[u]+alpha*err
  b2[i]<<-b2[i]+alpha*err
  p[u,]<<-p[u,]+alpha*(err*q1[i,])
  q1[i,]<<-q1[i,]+alpha*(err*p_plus_y)
  for(j in us_view_list[u]){
    y[j,]<<-y[j,]+alpha*(err*us_viewed_root1[u]*q1[i,])
  }  
}
gSVDpp_item=function(u,i){
  y_sum=us_viewed_root1[u]*colSums(y*ml_bin_matrix[u,])
  x_sum=item_genres_norm1[i]*colSums(x*item_genres[i,])
  p_plus_y=p[u,]+y_sum
  q_plus_x=q1[i,]+x_sum
  r2=glob_mean+b[u]+b2[i]+sum(q_plus_x*p_plus_y)
  err=r[u,i]-r2
  b[u]<<-b[u]+alpha*err
  b2[i]<<-b2[i]+alpha*err
  p[u,]<<-p[u,]+alpha*(err*q_plus_x)
  q1[i,]<<-q1[i,]+alpha*(err*p_plus_y)
  for(j in us_view_list[u]){
    y[j,]<<-y[j,]+alpha*(err*us_viewed_root1[u]*q_plus_x)
  }
  if(!sum(item_genres[i,])){
    x=x+item_genres[i,]%*%t(err/sum(item_genres[i,])*p_plus_y)
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

gSVDpp=function(Iter,f2,alpha2){
  init_SVD(f2,alpha2)
  for(I in 1:Iter){
    apply(ml_bin,1,FUN=function(x){gSVDpp_item(x[1],x[2])})    
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

MABPR_learn_weights=function(Iter){
  w<<-matrix(0,users,genres)
  for(I in 1:Iter){
    u=sample(1:users,1)
    i=sample(c(1:items)[ml_bin_matrix[u,]],1)
    j=sample(c(1:items)[ml_bin_matrix[u,]==FALSE],1)
    s2=sum(w[u,]*(item_genres[i,]-item_genres[j,]))
    err=exp(s2)/(1+exp(s2))
    w[u,]<<-w[u,]+alpha*err*(item_genres[i,]-item_genres[j,])
  }
}
MABPR=function(Iter,f2,alpha2){
  init_SVD(f2,alpha2)
  MABPR_learn_weights(Iter)
  for(I in 1:Iter){
    u=sample(1:users,1)
    i=sample(c(1:items)[ml_bin_matrix[u,]],1)
    j=sample(c(1:items)[-i],1)
    if(ml_bin_matrix[u,j]){
      wi=item_genres_norm1[i]*sum(item_genres[i,]*w[u,])
      wj=item_genres_norm1[j]*sum(item_genres[j,]*w[u,])
      if(wj>wi){
        k=i
        i=j
        j=k
      }
    }
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
  r2=matrix(0L,nrow=users,ncol=items)
  for(u in 1:users){
    for(i in 1:items){
      r2[u,i]=glob_mean+b[u]+b2[i]+sum(q1[i,]*p[u,])
    }
  }
  return(r2)
}

SVDpp_ratings=function(Iter,f2,alpha2){
  SVDpp(Iter,f2,alpha2)
  r2=matrix(0L,nrow=users,ncol=items)
  for(u in 1:users){
    y_sum=us_viewed_root1[u]*colSums(y*ml_bin_matrix[u,])
    p_plus_y=p[u,]+y_sum
    for(i in 1:items){
      r2[u,i]=glob_mean+b[u]+b2[i]+sum(q1[i,]*p_plus_y)
    }
  }
  return(r2)
}

gSVDpp_ratings=function(Iter,f2,alpha2){
  gSVDpp(Iter,f2,alpha2)
  r2=matrix(0L,nrow=users,ncol=items)
  for(u in 1:users){
    y_sum=us_viewed_root1[u]*colSums(y*ml_bin_matrix[u,])
    x_sum=item_genres_norm1[i]*colSums(x*item_genres[i,])
    p_plus_y=p[u,]+y_sum
    q_plus_x=q1[i,]+x_sum
    for(i in 1:items){
      r2[u,i]=glob_mean+b[u]+b2[i]+sum(q_plus_x*p_plus_y)
    }
  }
  return(r2)
}

BPR_pseudo_ratings=function(Iter,f2,alpha2){
  BPR(Iter,f2,alpha2)
  r2=matrix(0L,nrow=users,ncol=items)
  for(u in 1:users){
    for(i in 1:items){
      r2[u,i]=b2[i]+sum(q1[i,]*p[u,])
    }
  }
  return(r2)
}

MABPR_pseudo_ratings=function(Iter,f2,alpha2){
  MABPR(Iter,f2,alpha2)
  r2=matrix(0L,nrow=users,ncol=items)
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
  p<<-matrix(rnorm(users*f,mean=0,sd=1),users,f)
  q1<<-matrix(0,items,f)#TODO może jednak jakoś inaczej zainicjować
  b<<-rep(0L,users)
  b2<<-rep(0L,items)
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
yprim=matrix(rnorm(items*f,mean=0,sd=1),items,f)
{
  #init
  r=ml_matrix
  r2=matrix(0L,nrow=users,ncol=items)
  p=pprim
  q1=matrix(0,items,f)#TODO może jednak jakoś inaczej zainicjować
  y=yprim
  b=rep(0L,users)
  b2=rep(0L,items)
}

}