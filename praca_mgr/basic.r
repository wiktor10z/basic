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
  mov_pop<<-apply(ml_bin_matrix,2,sum)
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

# trivial recomendations

non_personalized=function(u,n=items,popularity=FALSE){#TODO może jakieś ważenie popularności i średniej
  if(popularity){
      score=mov_pop
  }else{
    score=mov_means
  }
  return(head(order(remove_viewed(ml_matrix[u,],score),decreasing=TRUE),n))
}
non_personalized_recs=function(n=items,popularity=FALSE){
  lapply(1:users,function(u) non_personalized(u,min(n,items-us_viewed[[u]],popularity)))
}

optimal_recs=function(n=items){
  recs=list(list())
  for(u in 1:users){
    recs[[u]]=head(order(ml_test_matrix[u,]-ml_matrix[u,],decreasing=TRUE),min(n,items-us_viewed[[u]]))
  }
  return(recs)
}

random_recs=function(n=items){
  lapply(1:users,function(u){head(sample((1:items)[!ml_bin_matrix[u,]]),min(n,us_viewed[[u]]))})
}

# przekształcenia

normalize_rating=function(rat,min1=1,max1=5){
  return(matrix(sapply(rat,function(x){max(min1,min(max1,x))}),nrow=nrow(rat)))
}

affine_rating=function(rat,min1=1,max1=5){
  min2=min(rat)
  max2=max(rat)
  f1=(max1-min1)/(max2-min2)
  return(matrix(sapply(rat,function(x){f1*(x-min2)+min1}),nrow=nrow(rat)))
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

# funkcje oceny

rating_MSE=function(rating,test){#mean square error
  return(sqrt(sum(apply(test,1,function(x){(x[3]-rating[x[1],x[2]])*(x[3]-rating[x[1],x[2]])}))/nrow(test)))
}

trivial_roc=function(resolution=1000){
  roc1=data.frame(rep(0,resolution),rep(0,resolution))
  for(j1 in 1:resolution){
    roc1[j1,1]=(j1/resolution)
    roc1[j1,2]=((j1-1)/resolution)
  }
  return(roc1)
}

normalize_roc=function(roc1,resolution=1000){
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

recs_ROC=function(recs,resolution=1000,quality=FALSE){#receiver operating characteristic
  roc_sum=data.frame(rep(0,resolution),rep(0,resolution))
  if(quality){
    matrix1=ml_test_matrix
  }else{
    matrix1=ml_test_bin_matrix
  }
  for(u in 1:users){
    hit_vec=matrix1[u,recs[[u]]]
    roc1=data.frame(cumsum(!hit_vec)/sum(!hit_vec),cumsum(hit_vec)/sum(hit_vec))
    roc_sum=roc_sum+normalize_roc(roc1,resolution)
  }
  return(roc_sum/users)
}

normalized_AUC=function(roc,resolution=1000){#area under curve
  return(sum(roc[,2])/resolution)
}





recs_precision1=function(recs,u){
  hit_vec=c(ml_test_bin_matrix[u,recs[[u]]],rep(FALSE,us_viewed[[u]]))
  return(cumsum(hit_vec)/cumsum(rep(1,items)))
}
recs_precision=function(recs){
  return(colSums(do.call(rbind,lapply(1:users,function(u){recs_precision1(recs,u)})))/users)
}

recs_MAP=function(recs){#mean average precission
  hit_vec=lapply(1:users,function(u){c(ml_test_bin_matrix[u,recs[[u]]],rep(FALSE,us_viewed[[u]]))})
  s1=rowSums(do.call(rbind,hit_vec))
  MAP_sum=colSums(do.call(rbind,lapply(1:users,function(u){
    if(s1[[u]]==0){
      rep(0,items)
    }else{
      cumsum(recs_precision1(recs,u)*hit_vec[[u]])/s1[[u]]
    }
    })))
  return(MAP_sum/users)
}

recs_MAP2=function(recs){
  hit_vec=lapply(1:users,function(u){c(ml_test_bin_matrix[u,recs[[u]]],rep(FALSE,us_viewed[[u]]))})
  s1=rowSums(do.call(rbind,hit_vec))
  MAP_sum=colSums(do.call(rbind,lapply(1:users,function(u){
    if(s1[[u]]==0){
      rep(0,items)
    }else{
      cumsum(recs_precision1(recs,u))/pmin(s1[[u]],1:items)
    }
  })))
  return(MAP_sum/users)
}

{  #MAP_sum=rep(0,items)
  #precision=recs_precision(recs)
  #for(u in 1:users){
  #  hit_vec=ml_test_bin_matrix[u,recs[[u]]]
  #  AveP=(cumsum(precision[[u]]*hit_vec))/sum(hit_vec)
  #  MAP_sum=MAP_sum+AveP
  #}
  #return(MAP_sum/users)
}
# ocena systemu

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

count_ROC_rating=function(rating_function,args,resolution=1000,quality=FALSE){
  roc_sum=data.frame(rep(0,resolution),rep(0,resolution))
  for(t1 in 1:5){
    read_ml_file(paste("ml-100k/u",t1,".base",sep=""))
    read_ml_test(paste("ml-100k/u",t1,".test",sep=""))
    recs=rating_to_recs(arguments_from_list(rating_function,args),items)
    roc_sum=roc_sum+recs_ROC(recs,resolution,quality)
  }
  return(roc_sum/5)
}

count_ROC_recs=function(recs_function,resolution=1000,quality=FALSE){
  roc_sum=data.frame(rep(0,resolution),rep(0,resolution))
  for(t1 in 1:5){
    read_ml_file(paste("ml-100k/u",t1,".base",sep=""))
    read_ml_test(paste("ml-100k/u",t1,".test",sep=""))
    recs=recs_function(items)
    roc_sum=roc_sum+recs_ROC(recs,resolution,quality)
  }
  return(roc_sum/5)
}

count_precision_rating=function(rating_function,args){
  precision_sum=rep(0,items)
  for(t1 in 1:5){
    read_ml_file(paste("ml-100k/u",t1,".base",sep=""))
    read_ml_test(paste("ml-100k/u",t1,".test",sep=""))
    recs=rating_to_recs(arguments_from_list(rating_function,args),items)
    precision_sum=precision_sum+recs_precision_vector(recs)
  }
  return(precision_sum/5)
}

count_precision_recs=function(recs_function){
  precision_sum=rep(0,items)
  for(t1 in 1:5){
    read_ml_file(paste("ml-100k/u",t1,".base",sep=""))
    read_ml_test(paste("ml-100k/u",t1,".test",sep=""))
    recs=recs_function(items)
    precision_sum=precision_sum+recs_precision_vector(recs)
  }
  return(precision_sum/5)
}
