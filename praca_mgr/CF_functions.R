viewed_both=function(x,y){
  return ((x!=0)&(y!=0))
}

# podobieństwo kowariancyjne Paersona
cov_similarity_vec=function(x,y){
  viewed=viewed_both(x,y)
  x2=x-mov_means
  y2=y-mov_means
  if(sum(viewed)<2){
    return(0)
  }else{
    return((sum(viewed*x2*y2))/(sqrt(sum(viewed*x2*x2)*sum(viewed*y2*y2))))
  }
}
# podobieństwo kosinusowe

cos_similarity_vec=function(x,y){
  viewed=viewed_both(x,y)
  if(sum(viewed)<1){
    return(0)
  }else{
    return((sum(viewed*x*y))/(sqrt(sum(viewed*x*x)*sum(viewed*y*y))))
  }
}

cov_similarity=function(i,j){
  return(cov_similarity_vec(ml_matrix[i,],ml_matrix[j,]))
}
cos_similarity=function(i,j){
  return(cos_similarity_vec(ml_matrix[i,],ml_matrix[j,]))
}

make_sim_matrix=function(sim_fun){
  matrix1=matrix(0L,nrow=users,ncol=users)
  for(i in 1:users){
    for(j in 1:users){
      matrix1[i,j]=sim_fun(i,j)
    } 
  }
  return(matrix1)
}

neighbours=function(i,n,x){
  all=sum(similarity_matrix[i,]>x)
  if(all<n){
    list=head(order(similarity_matrix[i,],decreasing=TRUE),all)
  }else{
    list=head(order(similarity_matrix[i,],decreasing=TRUE),n+1)
  }
  return(list[-(match(i,list,nomatch=length(list)))])
}

neighbours2=function(i,n,x,it){
  sim2=similarity_matrix[i,]*(ml_matrix[,it]!=0)
  all=sum(sim2>x)
  if(all<n){
    list=head(order(sim2,decreasing=TRUE),all)
  }else{
    list=head(order(sim2,decreasing=TRUE),n+1)
  }
  return(list[-(match(i,list,nomatch=length(list)))])
}


CF_predict=function(u){
  mean_u=us_means[u]
  rating=numeric(length=movies)
  for(i in 1:movies){
    neighbours_ui=neighbours2(u,30,0,i)
    if(length(neighbours_ui)>0){
      sum1=0
      sum2=0
      for(v in neighbours_ui){
        sum1=sum1+similarity_matrix[u,v]*(ml_matrix[v,i]-us_means[v])
        sum2=sum2+similarity_matrix[u,v]
      }
      rating[i]=mean_u+sum1/sum2
    }else{
      rating[i]=0
    }
  }
  return(rating)
}
CF_predict_all=function(sim_mat){
  similarity_matrix<<-sim_mat
  return(matrix(sapply(1:users,CF_predict),byrow=TRUE,nrow=users))
}


