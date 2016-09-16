mean1=function(x){
  if(sum(x)==0){
    return(0)
  }else{
    return (sum(x)/sum(x!=0))
  }
}
arguments_from_list=function(fun,args){
  l=length(args)
  if(l==0){#TODO jak zero, to powinno byc fun(0) - inna funkcja wywoływana dopiero w tym momencie
    return(fun)
  }else if(l==1){
    return(fun(args[[1]]))
  }else if(l==2){
    return(fun(args[[1]],args[[2]]))
  }else{
    return(fun(args[[1]],args[[2]],args[[3]]))
  }
}

movies=1682
items=1682
users=943

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
  us_view_list<<-list(list())
  us_viewed<<-list()  
  for(i in 1:users){
    us_view_list[[i]]<<-which(ml_matrix[i,]!=0)
    us_viewed[i]<<-length(us_view_list[[i]])
  }
  mov_means<<-apply(ml_matrix,2,mean1)
  us_means<<-apply(ml_matrix,1,mean1)
  glob_mean<<-mean(mov_means[mov_means>0])
}
read_ml_test=function(file){
  ml_test<<-read.csv(file,header=FALSE,sep="\t")
  ml_test_bin<<-ml_test[,1:2]
  ml_matrix1=matrix(0L,nrow=users,ncol=movies)
  for(i in 1:nrow(ml_test)){
    ml_matrix1[ml_test[i,1],ml_test[i,2]]=ml_test[i,3]
  }
  ml_test_matrix<<-ml_matrix1
  ml_test_bin_matrix<<-(ml_test_matrix!=0)
}

#system.time({
#  ml100k_SVD = svd(ml100k_matrix)
#})

remove_viewed=function(x,y){
  return ((x==0)*y)
}
# non-personalized
non_personalized=function(u,n){
  return(head(order(remove_viewed(ml_matrix[u,],mov_means),decreasing=TRUE),n))
}

non_personalized_recs=function(n){
  lapply(1:users,function(u) non_personalized(u,min(n,items-us_viewed[[u]])))
}

random_recs=function(n){
  lapply(1:users,function(u){head(sample((1:items)[!ml_bin_matrix[u,]]),min(n,us_viewed[[u]]))})
}

# przekształcenia

normalize_rating=function(rat,min1,max1){
  return(matrix(sapply(rat,function(x){max(min1,min(max1,x))}),nrow=nrow(rat)))
}

affine_rating=function(rat){
  min1=min(rat)
  max1=max(rat)
  f1=4/(max1-min1)
  return(matrix(sapply(rat,function(x){f1*(x-min1)+1}),nrow=nrow(rat)))
}

rating_MSE=function(rating,test){
  return(sqrt(sum(apply(test,1,function(x){(x[3]-rating[x[1],x[2]])*(x[3]-rating[x[1],x[2]])}))/nrow(test)))
}

count_MSE=function(rating_function,args){
  value=0
  for(t1 in 1:5){
    read_ml_file(paste("ml-100k/u",t1,".base",sep=""))
    read_ml_test(paste("ml-100k/u",t1,".test",sep=""))
    rating=arguments_from_list(rating_function,args)
    value=value+rating_MSE(rating,ml_test)
  }
  return(value/5)
}
count_ROC_rating=function(rating_function,args,resolution){
  roc_sum=data.frame(rep(0,resolution),rep(0,resolution))
  for(t1 in 1:5){
    read_ml_file(paste("ml-100k/u",t1,".base",sep=""))
    read_ml_test(paste("ml-100k/u",t1,".test",sep=""))
    propos=rating_to_propos(arguments_from_list(rating_function,args),items)
    roc_sum=roc_sum+propos_ROC(propos,resolution)
  }
  return(roc_sum/5)
}
count_ROC_recs=function(recs_function,resolution){
  roc_sum=data.frame(rep(0,resolution),rep(0,resolution))
  for(t1 in 1:5){
    read_ml_file(paste("ml-100k/u",t1,".base",sep=""))
    read_ml_test(paste("ml-100k/u",t1,".test",sep=""))
    propos=recs_function(items)
    roc_sum=roc_sum+propos_ROC(propos,resolution)
  }
  return(roc_sum/5)
}

rating_to_propos1=function(u,n){
  return(head(order(remove_viewed(ml_matrix[u,],predicted_ratings[u,]),decreasing=TRUE),min(n,items-us_viewed[[u]])))
}
rating_to_propos=function(ratings,n){
  predicted_ratings<<-ratings
  lapply(1:users,function(u) rating_to_propos1(u,n))
}

normalized_AUC=function(roc,resolution){
  return(sum(roc[,2])/resolution)
}

trivial_roc=function(resolution){
  roc1=data.frame(rep(0,resolution),rep(0,resolution))
  for(j1 in 1:resolution){
    roc1[j1,1]=(j1/resolution)
    roc1[j1,2]=((j1-1)/resolution)
  }
  return(roc1)
}

normalize_roc=function(roc1,resolution){
  if((sum(is.nan(roc1[,1]))>0)||(sum(is.nan(roc1[,2]))>0)){
    return(trivial_roc(resolution))
  }else{
    roc2=data.frame(rep(0,resolution),rep(0,resolution))
    k1=1
    for(j1 in 1:resolution){
      while(roc1[k1,1]<(j1/resolution)){
        k1=k1+1
      }
      roc2[j1,1]=(j1/resolution)
      roc2[j1,2]=roc1[k1,2]
    }  
    return(roc2)
  }
}

propos_ROC=function(propos,resolution){
  roc_sum=data.frame(rep(0,resolution),rep(0,resolution))
  for(u in 1:users){
    hit_vec=ml_test_bin_matrix[u,propos[[u]]]
    roc1=data.frame(cumsum(hit_vec)/sum(hit_vec),cumsum(!hit_vec)/sum(!hit_vec))
    roc_sum=roc_sum+normalize_roc(roc1,resolution)
  }
  return(roc_sum/users)
}


