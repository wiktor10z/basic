#TODO można spróbować zespolić funkcje zwykłego podobieństwa i przedmiotów z ustawianym parametrem

viewed_both=function(x,y){
  return ((x!=0)&(y!=0))
}# może zmienić nazwę skoro działa też odwrotnie

similarity_vec=function(x,y){
  viewed=viewed_both(x,y)
  if(sum(viewed)<1){
    return(0)
  }else{
    return((sum(viewed*x*y))/(sqrt(sum(viewed*x*x)*sum(viewed*y*y))))
  }
}

# podobieństwo korelacji Paersona
cor_similarity=function(u,v){
  return(similarity_vec(ml_matrix[u,]-us_means[u],ml_matrix[v,]-us_means[v]))
}

# podobieństwo kosinusowe
cos_similarity=function(u,v){
  return(similarity_vec(ml_matrix[u,],ml_matrix[v,]))
}

make_sim_matrix=function(sim_fun){#TODO można przyśpieszyć licząc tylko górny trójkąt - symetryczna
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
  sim2=similarity_matrix[u,]*(ml_matrix[,it]!=0) # TODO można by usunąć u już teraz albo generować w ogóle macierz podobieństwa z 0 na przekątnej
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
      rating[i]=0 #TODO może średnia, z drugiej strony skoro nie ma podobnych, to znaczy, że raczej nieporządane
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

# to samo dla podobiestwa miedzy przedmiotami

item_cor_similarity=function(i,j){
  return(similarity_vec(ml_matrix[,i]-mov_means[i],ml_matrix[,j]-mov_means[j]))
}

item_cos_similarity=function(i,j){
  return(similarity_vec(ml_matrix[,i],ml_matrix[,j]))
}

item_make_sim_matrix=function(item_sim_fun){#TODO można przyśpieszyć licząc tylko górny trójkąt - symetryczna
  matrix1=matrix(0L,nrow=items,ncol=items)
  for(i in 1:items){
    for(j in 1:items){
      matrix1[i,j]=item_sim_fun(i,j)
    } 
  }
  return(matrix1)
}

item_neighbours=function(i,u,n,x){
  sim2=similarity_matrix[i,]*(ml_matrix[u,]!=0)
  all=sum(sim2>x)
  if(all<n){
    list=head(order(sim2,decreasing=TRUE),all)
  }else{
    list=head(order(sim2,decreasing=TRUE),n+1)
  }
  return(list[-(match(i,list,nomatch=length(list)))])
}

item_CF_predict=function(u){
  rating=numeric(length=items)
  for(i in 1:items){
    neighbours_ui=item_neighbours(i,u,30,0)
    if(length(neighbours_ui)>0){
      sum1=0
      sum2=0
      for(j in neighbours_ui){
        sum1=sum1+similarity_matrix[i,j]*(ml_matrix[u,j]-mov_means[j])
        sum2=sum2+similarity_matrix[i,j]
      }
      rating[i]=mov_means[i]+sum1/sum2
    }else{
      rating[i]=0 #TODO może średnia, z drugiej strony skoro nie ma podobnych, to znaczy, że raczej nieporządane
                  # przy jednym tylko ledwo podobnym daje to jego ocenę, przy użyciu tych z ujemnym podobieństwem
                  # jest możliwość wystąpienia sumy wag ujemnej=tak samo jakby zamienić znak podobieństwa - gorsze od średniej
    }
  }
  return(rating)
}
item_CF_predict_all=function(item_sim_mat){
  similarity_matrix<<-item_sim_mat
  return(matrix(sapply(1:users,item_CF_predict),byrow=TRUE,nrow=users))
}

item_CF_ratings=function(item_sim_fun){
  return(item_CF_predict_all(item_make_sim_matrix(item_sim_fun)))
}

