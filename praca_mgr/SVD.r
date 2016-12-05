# ALGORYTMY Z GRUPY SVD

genres_norm=function(x){
  y=sum(x)
  if(y==0){
    return(0)
  }else{
    return(1/y)
  }
}

# losowa inicjalizacja zmiennych
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

# uaktualnienie zmiennych na podstawie pojedynczej oceny
SVD_one=function(u,i,l_b=0.01,l_b2=0.01,l_p=0.3,l_q=0.3){#test 0.01 i 0.3 wyglądają na dobre w MyMediaLite 0.005 i 0.015
  r2=glob_mean+b[u]+b2[i]+sum(q1[i,]*p[u,])
  err=r[u,i]-r2
  b[u]<<-b[u]+alpha*(err-l_b*b[u])
  b2[i]<<-b2[i]+alpha*(err-l_b2*b2[i])
  p[u,]<<-p[u,]+alpha*(err*q1[i]-l_p*p[u,])
  q1[i,]<<-q1[i,]+alpha*(err*p[u,]-l_q*q1[i,])
}

SVDpp_one=function(u,i,l_b=0.01,l_b2=0.01,l_p=0.3,l_q=0.3,l_y=0.3){
  y_sum=us_viewed_root1[u]*colSums(y*ml_bin_matrix[u,])
  p_plus_y=p[u,]+y_sum
  r2=glob_mean+b[u]+b2[i]+sum(q1[i,]*p_plus_y)
  err=r[u,i]-r2
  b[u]<<-b[u]+alpha*(err-l_b*b[u])
  b2[i]<<-b2[i]+alpha*(err-l_b2*b2[i])
  p[u,]<<-p[u,]+alpha*(err*q1[i]-l_p*p[u,])
  q1[i,]<<-q1[i,]+alpha*(err*p_plus_y-l_q*q1[i,])
  for(j in us_view_list[u]){
    y[j,]<<-y[j,]+alpha*(err*us_viewed_root1[u]*q1[i,]-l_y*y[j,])
  }  
}

gSVDpp_one=function(u,i,l_b=0.01,l_b2=0.01,l_p=0.3,l_q=0.3,l_y=0.3,l_x=0.15){
  y_sum=us_viewed_root1[u]*colSums(y*ml_bin_matrix[u,])
  x_sum=item_genres_norm1[i]*colSums(x*item_genres[i,])
  p_plus_y=p[u,]+y_sum
  q_plus_x=q1[i,]+x_sum
  r2=glob_mean+b[u]+b2[i]+sum(q_plus_x*p_plus_y)
  err=r[u,i]-r2
  b[u]<<-b[u]+alpha*(err-l_b*b[u])
  b2[i]<<-b2[i]+alpha*(err-l_b2*b2[i])
  p[u,]<<-p[u,]+alpha*(err*q_plus_x-l_p*p[u,])
  q1[i,]<<-q1[i,]+alpha*(err*p_plus_y-l_q*q1[i,])
  for(j in us_view_list[u]){
    y[j,]<<-y[j,]+alpha*(err*us_viewed_root1[u]*q_plus_x-l_y*y[j,])
  }
  if(sum(item_genres[i,])>0){
    x<<-x*(1-alpha*l_x*item_genres[i,])+item_genres[i,]%*%t(alpha*err*item_genres_norm1[i]*p_plus_y)
  }
}

# wyliczenie wszystkich zmiennych wykorzystywanych w predykcji  Iter - ilość iteracji f2 - długość wektorów, l_* - stałe regularyzacyjne
SVD=function(Iter,f2,alpha2=0.01,l_b=0.01,l_b2=0.01,l_p=0.3,l_q=0.3){
  init_SVD(f2,alpha2)
  for(I in 1:Iter){
    apply(ml_bin,1,FUN=function(x){SVD_one(x[1],x[2],l_b,l_b2,l_p,l_q)})    
  }
}

SVDpp=function(Iter,f2,alpha2=0.01,l_b=0.01,l_b2=0.01,l_p=0.3,l_q=0.3,l_y=0.3){
  init_SVD(f2,alpha2)
  for(I in 1:Iter){
    apply(ml_bin,1,FUN=function(x){SVDpp_one(x[1],x[2],l_b,l_b2,l_p,l_q,l_y)})    
  }
}

gSVDpp=function(Iter,f2,alpha2=0.01,l_b=0.01,l_b2=0.01,l_p=0.3,l_q=0.3,l_y=0.3,l_x=0.15){
  init_SVD(f2,alpha2)
  for(I in 1:Iter){
    apply(ml_bin,1,FUN=function(x){gSVDpp_one(x[1],x[2],l_b,l_b2,l_p,l_q,l_y,l_x)})    
  }
}

BPR=function(Iter,f2,alpha2=0.04,unif_user=TRUE,l_b2=0.005,l_p=0.025,l_q1=0.025,l_q2=0.0025){
  init_SVD(f2,alpha2)
  if(unif_user){
    list1=sample(1:users,Iter*nrow(ml_bin),replace=TRUE)
  }else{
    list1=sample(1:users,Iter*nrow(ml_bin),replace=TRUE,prob=(us_viewed*(items-us_viewed)))
  }
  for(u in list1){
    i=sample(c(1:items)[ml_bin_matrix[u,]],1)
    j=sample(c(1:items)[ml_bin_matrix[u,]==FALSE],1)
    s2=b2[i]-b2[j]+sum(p[u,]*(q1[i,]-q1[j,]))
    err=1/(1+exp(s2))
    b2[i]<<-b2[i]+alpha*(err-l_b2*b2[i])
    b2[j]<<-b2[j]+alpha*(-err-l_b2*b2[j])
    p[u,]<<-p[u,]+alpha*(err*(q1[i,]-q1[j,])-l_p*p[u,])
    q1[i,]<<-q1[i,]+alpha*(err*p[u,]-l_q1*q1[i,])
    q1[j,]<<-q1[j,]+alpha*(-err*p[u,]-l_q2*q1[j,])
  }
}

# preprocesing MABPR polegający na wyliczeniu wag w
MABPR_learn_weights=function(Iter,unif_user=TRUE,l_w){
  w<<-matrix(0,users,genres)
  if(unif_user){
    list1=sample(1:users,Iter*nrow(ml_bin),replace=TRUE)
  }else{
    list1=sample(1:users,Iter*nrow(ml_bin),replace=TRUE,prob=(us_viewed*(items-us_viewed)))
  }
  for(u in list1){
    i=sample(c(1:items)[ml_bin_matrix[u,]],1)
    j=sample(c(1:items)[ml_bin_matrix[u,]==FALSE],1)
    s2=sum(w[u,]*(item_genres[i,]-item_genres[j,]))
    err=exp(s2)/(1+exp(s2))
    w[u,]<<-w[u,]+alpha*(err*(item_genres[i,]-item_genres[j,])-l_w*w[u,])
  }
}

MABPR=function(Iter,f2,alpha2,unif_user=TRUE,l_w=0.075,l_b2=0.005,l_p=0.025,l_q1=0.025,l_q2=0.0025){
  init_SVD(f2,alpha2)
  MABPR_learn_weights(Iter,unif_user,l_w)
  if(unif_user){
    list1=sample(1:users,Iter*nrow(ml_bin),replace=TRUE)
  }else{
    list1=sample(1:users,Iter*nrow(ml_bin),replace=TRUE,prob=us_viewed)
  }
  for(u in list1){
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
    s2=b2[i]-b2[j]+sum(p[u,]*(q1[i,]-q1[j,]))
    err=1/(1+exp(s2))
    b2[i]<<-b2[i]+alpha*(err-l_b2*b2[i])
    b2[j]<<-b2[j]+alpha*(-err-l_b2*b2[j])
    p[u,]<<-p[u,]+alpha*(err*(q1[i,]-q1[j,])-l_p*p[u,])
    q1[i,]<<-q1[i,]+alpha*(err*p[u,]-l_q1*q1[i,])
    q1[j,]<<-q1[j,]+alpha*(-err*p[u,]-l_q2*q1[j,])
  }
}

MABPR_gSVDpp=function(Iter,f2,alpha2,unif_user=TRUE,l_w=0.075,l_b2=0.005,l_p=0.025,l_q1=0.025,l_q2=0.0025,l_y=0.006,l_x1=0.006,l_x2=0.006){
  init_SVD(f2,alpha2)
  MABPR_learn_weights(Iter,unif_user,l_w)
  if(unif_user){
    list1=sample(1:users,Iter*nrow(ml_bin),replace=TRUE)
  }else{
    list1=sample(1:users,Iter*nrow(ml_bin),replace=TRUE,prob=us_viewed)
  }
  for(u in list1){
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
    y_sum=us_viewed_root1[u]*colSums(y*ml_bin_matrix[u,])
    x_sum1=item_genres_norm1[i]*colSums(x*item_genres[i,])
    x_sum2=item_genres_norm1[j]*colSums(x*item_genres[j,])
    p_plus_y=p[u,]+y_sum
    q_plus_x1=q1[i,]+x_sum1
    q_plus_x2=q1[j,]+x_sum2
    q_diff=q_plus_x1-q_plus_x2
    s2=b2[i]-b2[j]+sum(q_diff*p_plus_y)
    err=1/(1+exp(s2))
    b2[i]<<-b2[i]+alpha*(err-l_b2*b2[i])
    b2[j]<<-b2[j]+alpha*(-err-l_b2*b2[j])
    p[u,]<<-p[u,]+alpha*(err*q_diff-l_p*p[u,])
    q1[i,]<<-q1[i,]+alpha*(err*p_plus_y-l_q1*q1[i,])
    q1[j,]<<-q1[j,]+alpha*(-err*p_plus_y-l_q2*q1[j,])
    for(k in us_view_list[u]){
      y[k,]<<-y[k,]+alpha*(err*us_viewed_root1[u]*q_diff-l_y*y[k,])
    }
    if(sum(item_genres[i,])>0){
      x<<-x*(1-alpha*l_x1*item_genres[i,])+item_genres[i,]%*%t(alpha*err*item_genres_norm1[i]*p_plus_y)
    }
    if(sum(item_genres[j,])>0){
      x<<-x*(1-alpha*l_x2*item_genres[j,])-item_genres[j,]%*%t(alpha*err*item_genres_norm1[j]*p_plus_y)
    }
  }
}

#------------------------------------------------------------------------------
# wyliczenie zmiennych a następnie zwrócenie wszystkich ocen

SVD_ratings=function(Iter,f2,alpha2=0.01,l_b=0.01,l_b2=0.01,l_p=0.3,l_q=0.3){
  SVD(Iter,f2,alpha2,l_b,l_b2,l_p,l_q)
  r2=matrix(0L,nrow=users,ncol=items)
  for(u in 1:users){
    for(i in 1:items){
      r2[u,i]=glob_mean+b[u]+b2[i]+sum(q1[i,]*p[u,])
    }
  }
  return(r2)
}

SVDpp_ratings=function(Iter,f2,alpha2=0.01,l_b=0.01,l_b2=0.01,l_p=0.3,l_q=0.3,l_y=0.3){
  SVDpp(Iter,f2,alpha2,l_b,l_b2,l_p,l_q,l_y)
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

gSVDpp_ratings=function(Iter,f2,alpha2=0.01,l_b=0.01,l_b2=0.01,l_p=0.3,l_q=0.3,l_y=0.3,l_x=0.15){
  gSVDpp(Iter,f2,alpha2,l_b,l_b2,l_p,l_q,l_y,l_x)
  r2=matrix(0L,nrow=users,ncol=items)
  for(u in 1:users){
    y_sum=us_viewed_root1[u]*colSums(y*ml_bin_matrix[u,])
    p_plus_y=p[u,]+y_sum
    for(i in 1:items){
      x_sum=item_genres_norm1[i]*colSums(x*item_genres[i,])
      q_plus_x=q1[i,]+x_sum     
      r2[u,i]=glob_mean+b[u]+b2[i]+sum(q_plus_x*p_plus_y)
    }
  }
  return(r2)
}

BPR_pseudo_ratings=function(Iter,f2,alpha2=0.04,unif_user=TRUE,l_b2=0.005,l_p=0.025,l_q1=0.025,l_q2=0.0025){
  BPR(Iter,f2,alpha2,unif_user,l_b2,l_p,l_q1,l_q2)
  r2=matrix(0L,nrow=users,ncol=items)
  for(u in 1:users){
    for(i in 1:items){
      r2[u,i]=b2[i]+sum(q1[i,]*p[u,])
    }
  }
  return(affine_rating(r2,1,5))
}

MABPR_pseudo_ratings=function(Iter,f2,alpha2=0.04,unif_user=TRUE,l_w=0.075,l_b2=0.005,l_p=0.025,l_q1=0.025,l_q2=0.0025){
  MABPR(Iter,f2,alpha2,unif_user,l_w,l_b2,l_p,l_q1,l_q2)
  r2=matrix(0L,nrow=users,ncol=items)
  for(u in 1:users){
    for(i in 1:items){
      r2[u,i]=b2[i]+sum(q1[i,]*p[u,])
    }
  }
  return(affine_rating(r2,1,5))
}

MABPR_gSVDpp_pseudo_ratings=function(Iter,f2=0.04,alpha2,unif_user=TRUE,l_w=0.075,l_b2=0.005,l_p=0.025,l_q1=0.025,l_q2=0.0025,l_y=0.006,l_x1=0.006,l_x2=0.006){
  MABPR_gSVDpp(Iter,f2,alpha2,unif_user,l_w,l_b2,l_p,l_q1,l_q2,l_y,l_x1,l_x2)
  r2=matrix(0L,nrow=users,ncol=items)
  for(u in 1:users){
    y_sum=us_viewed_root1[u]*colSums(y*ml_bin_matrix[u,])
    p_plus_y=p[u,]+y_sum
    for(i in 1:items){
      x_sum=item_genres_norm1[i]*colSums(x*item_genres[i,])
      q_plus_x=q1[i,]+x_sum
      r2[u,i]=b2[i]+sum(q_plus_x*p_plus_y)
    }
  }
  return(affine_rating(r2,1,5))
}