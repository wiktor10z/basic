viewed_both=function(x,y){
  return ((x!=0)&(y!=0))
}

similarity_vec=function(x,y){
  viewed=viewed_both(x,y)
  if(sum(viewed)<1){
    return(0)
  }else{
    return((sum(viewed*x*y))/(sqrt(sum(viewed*x*x)*sum(viewed*y*y))))
  }
}

# podobieństwo korelacji Paersona
cor_similarity=function(i,j){
  return(similarity_vec(ml_matrix[i,]-us_means[i],ml_matrix[j,]-us_means[j]))
}

# podobieństwo kosinusowe
cos_similarity=function(i,j){
  return(similarity_vec(ml_matrix[i,],ml_matrix[j,]))
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

neighbours=function(u,n,x){
  all=sum(similarity_matrix[u,]>x)
  if(all<n){
    list=head(order(similarity_matrix[u,],decreasing=TRUE),all)
  }else{
    list=head(order(similarity_matrix[u,],decreasing=TRUE),n+1)
  }
  return(list[-(match(u,list,nomatch=length(list)))])
}

neighbours2=function(u,n,x,it){
  sim2=similarity_matrix[u,]*(ml_matrix[,it]!=0)
  all=sum(sim2>x)
  if(all<n){
    list=head(order(sim2,decreasing=TRUE),all)
  }else{
    list=head(order(sim2,decreasing=TRUE),n+1)
  }
  return(list[-(match(u,list,nomatch=length(list)))])
}

CF_predict=function(u){
  mean_u=us_means[u]
  rating=numeric(length=items)
  for(i in 1:items){
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

CF_ratings=function(sim_fun){
  return(CF_predict_all(make_sim_matrix(sim_fun)))
}