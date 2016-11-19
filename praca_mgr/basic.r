mean1=function(x){
  if(sum(x)==0){
    return(3)
  }else{
    return (sum(x)/sum(x!=0))
  }
}
arguments_from_list=function(fun,args){
  l=length(args)
  if(l==0){#TODO jak zero, to powinno byc fun(0) - inna funkcja wywoływana dopiero w tym momencie
    return(fun(0))
  }else if(l==1){
    return(fun(args[[1]]))
  }else if(l==2){
    return(fun(args[[1]],args[[2]]))
  }else if(l==3){
    return(fun(args[[1]],args[[2]],args[[3]]))
  }else if(l==4){
    return(fun(args[[1]],args[[2]],args[[3]],args[[4]]))    
  }else if(l==5){
    return(fun(args[[1]],args[[2]],args[[3]],args[[4]],args[[5]])) 
  }else if(l==6){
    return(fun(args[[1]],args[[2]],args[[3]],args[[4]],args[[5]],args[[6]])) 
  }else if(l==7){
    return(fun(args[[1]],args[[2]],args[[3]],args[[4]],args[[5]],args[[6]],args[[7]]))  
  }else if(l==8){
    return(fun(args[[1]],args[[2]],args[[3]],args[[4]],args[[5]],args[[6]],args[[7]],args[[8]]))  
  }else if(l==9){
    return(fun(args[[1]],args[[2]],args[[3]],args[[4]],args[[5]],args[[6]],args[[7]],args[[8]],args[[9]]))  
  }else if(l==10){
    return(fun(args[[1]],args[[2]],args[[3]],args[[4]],args[[5]],args[[6]],args[[7]],args[[8]],args[[9]],args[[10]]))  
  }else if(l==11){
    return(fun(args[[1]],args[[2]],args[[3]],args[[4]],args[[5]],args[[6]],args[[7]],args[[8]],args[[9]],args[[10]],args[[11]]))  
  }else if(l==12){
    return(fun(args[[1]],args[[2]],args[[3]],args[[4]],args[[5]],args[[6]],args[[7]],args[[8]],args[[9]],args[[10]],args[[11]],args[[12]]))  
  }
}

items=1682
users=943
genres=19

# wczytanie
read_ml_file=function(file){
  ml<<-read.csv(file,header=FALSE,sep="\t")
  observations<<-nrow(ml)  
  ml_bin<<-ml[,1:2]
  ml_matrix1=matrix(0L,nrow=users,ncol=items)
  for(i in 1:observations){
    ml_matrix1[ml[i,1],ml[i,2]]=ml[i,3]
  }
  ml_matrix<<-ml_matrix1
  ml_bin_matrix<<-(ml_matrix!=0)
  ml_like_matrix<<-matrix(sapply(ml_matrix,function(x){
    if(x==0){return(0)}
    else if(x>=3){return(1)}
    else{return(-1)}})
    ,nrow=users,ncol=items)
  us_view_list<<-list(list())
  us_viewed=list()  
  for(i in 1:users){
    us_view_list[[i]]<<-which(ml_matrix[i,]!=0)
    us_viewed[i]=length(us_view_list[[i]])
  }
  us_viewed<<-unlist(us_viewed)
  mov_means<<-apply(ml_matrix,2,mean1)
  mov_pop<<-apply(ml_bin_matrix,2,sum)
  us_means<<-apply(ml_matrix,1,mean1)
  glob_mean<<-mean(mov_means[mov_means>0])
}
read_meta_file=function(file,names_file=NULL){
  ml_meta_data<<-read.csv(file,header=FALSE,sep="|")
  item_genres<<-data.matrix(ml_meta_data[,6:24])
  if(is.null(names_file)){
    colnames(item_genres)<<-c(1:19)
  }else{
    genre_names=read.csv(names_file,header=FALSE,sep="|")
    colnames(item_genres)<<-genre_names[,1]
  }
}
read_ml_test=function(file){
  ml_test<<-read.csv(file,header=FALSE,sep="\t")
  ml_test_bin<<-ml_test[,1:2]
  ml_matrix1=matrix(0L,nrow=users,ncol=items)
  for(i in 1:nrow(ml_test)){
    ml_matrix1[ml_test[i,1],ml_test[i,2]]=ml_test[i,3]
  }
  ml_test_matrix<<-ml_matrix1
  ml_test_bin_matrix<<-(ml_test_matrix!=0)
  ml_test_best_matrix<<-(ml_test_matrix==5)
}

# trivial recomendations

non_personalized_rating=function(popularity=FALSE){
  if(popularity){
    return(affine_rating(rep(1,users)%*%t.default(mov_pop),1,5))
  }else{
    return(((rep(1,users)%*%t.default(mov_means))+(us_means%*%t.default(rep(1,items))))/2)
  }
}

non_personalized=function(u,n=items,popularity=FALSE){#TODO może jakieś ważenie popularności i średniej
  if(popularity){
    score=mov_pop
  }else{
    score=mov_means
  }
  return(head(order(remove_viewed(ml_matrix[u,],score),decreasing=TRUE),n))
}
non_personalized_recs=function(n=items,popularity=FALSE){
  lapply(1:users,function(u) non_personalized(u,min(n,items-us_viewed[[u]]),popularity))
}

optimal_rating=function(x=0){
  return(ml_matrix+ml_test_matrix)
}
optimal_recs=function(n=items){
  recs=list(list())
  for(u in 1:users){
    recs[[u]]=head(order(ml_test_matrix[u,]-ml_matrix[u,],decreasing=TRUE),min(n,items-us_viewed[[u]]))
  }
  return(recs)
}

random_rating=function(min1=1,max1=5){
  return(matrix(runif(users*items,min=min1,max=max1),users,items))
}
random_recs=function(n=items){
  lapply(1:users,function(u){head(sample((1:items)[!ml_bin_matrix[u,]]),min(n,items-us_viewed[[u]]))})
}

# przekształcenia

#TODO sprawdzić czy czyta pomiędzy plikami
vec_reg=function(x){
  if(sum(x)==0){
    return(x)
  }else{
    return(x/sqrt(sum(x!=0)))
  }
}

normalize_rating=function(rat,min1=1,max1=5){
  return(matrix(sapply(rat,function(x){max(min1,min(max1,x))}),nrow=nrow(rat)))
}

affine_rating=function(rat,min1=1,max1=5){#cała macierz
  min2=min(rat)
  max2=max(rat)
  f1=(max1-min1)/(max2-min2)
  return(matrix(sapply(rat,function(x){f1*(x-min2)+min1}),nrow=nrow(rat)))
}

affine_rating0=function(rat0,min1=1,max1=5){
  min2=min(rat0)
  max2=max(rat0)
  f1=(max1-min1)/(max2-min2) 
  return(sapply(rat0,function(x){f1*(x-min2)+min1}))
}
affine_rating2=function(rat,min1=1,max1=5){#każdy użytkownik osobno
  return(t(apply(rat,1,function(x){affine_rating0(x,min1,max1)})))
}

remove_viewed=function(x,y){
  return ((x==0)*y)
}
rating_to_recs1=function(u,n=items){
  return(head(order(remove_viewed(ml_matrix[u,],predicted_ratings[u,]),decreasing=TRUE),min(n,items-us_viewed[[u]])))
}
rating_to_recs=function(ratings,n=items){
  predicted_ratings<<-affine_rating(ratings,1,2)
  lapply(1:users,function(u) rating_to_recs1(u,n))
}