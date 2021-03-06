# EWALUACJA OCEN,REKOMENDACJI,ALGORYTMÓW + WYKRESY

# ewaluacja ocen i rekomendacji - zakłada wczytany plik testowy

# ocena błędów przewidywanych ocen
rating_MAE=function(rating,test=ml_test){
  return(sum(apply(test,1,function(x){abs(x[3]-rating[x[1],x[2]])}))/nrow(test))
}

rating_MSE=function(rating,test=ml_test){#mean square error
  return(sum(apply(test,1,function(x){(x[3]-rating[x[1],x[2]])*(x[3]-rating[x[1],x[2]])}))/nrow(test))
}

# tworzenie krzywej roc dla list rekomendacji - z odpowiednim skalowaniem osi x
zero_roc=function(resolution=1000){
  return(data.frame(rep(0,resolution),rep(0,resolution)))
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
    return(zero_roc(resolution))
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

# quality=0 - zwykły ROC, quality=1 - wzięcie pod uwagę ocen, quality=2 - wzięcie pod uwagę tylko przedmiotów z oceną 
# resolution - ilość punktów w krzywej (mniejsza ilość przyśpiesza tworzenie i przetwarzanie krzywych)
recs_ROC=function(recs,resolution=1000,quality=0){#receiver operating characteristic
  roc_sum=data.frame(rep(0,resolution),rep(0,resolution))
  if(quality==0){
    matrix1=ml_test_matrix
  }else if(quality==1){
    matrix1=ml_test_bin_matrix
  }else{
    matrix1=ml_test_best_matrix
  }
  for(u in 1:users){
    hit_vec=matrix1[u,recs[[u]]]
    roc1=data.frame(cumsum(!hit_vec)/sum(!hit_vec),cumsum(hit_vec)/sum(hit_vec))
    roc_sum=roc_sum+normalize_roc(roc1,resolution)
  }
  if(quality==2){
    return(roc_sum/(users-test_empty_users_best))
  }else{
    return(roc_sum/(users-test_empty_users))
  }
}

normalized_AUC=function(roc,resolution=1000){#area under curve
  return(sum(roc[,2])/resolution)
}

# przekształcenie list rekomendacji na listy trafień
hit_vector=function(recs,only_best=FALSE){
  if(only_best){
    return(lapply(1:users,function(u){c(ml_test_best_matrix[u,recs[[u]]],rep(FALSE,us_viewed[[u]]))})) 
  }else{
    return(lapply(1:users,function(u){c(ml_test_bin_matrix[u,recs[[u]]],rep(FALSE,us_viewed[[u]]))}))
  }
}

# wyliczenie precyzji
hit_precision=function(hit_vec){
  return(do.call(rbind,lapply(1:users,function(u){cumsum(hit_vec[[u]])/cumsum(rep(1,items))})))
}

recs_precision=function(recs,only_best=FALSE){
  return(colSums(hit_precision(hit_vector(recs,only_best)))/users)
}

recs_AveP=function(recs,only_best=FALSE){#praca198
  hit_vec=hit_vector(recs,only_best)
  prec1=hit_precision(hit_vec)
  return(do.call(rbind,lapply(1:users,function(u){
    if(sum(hit_vec[[u]])==0){
      rep(0,items)
    }else{
      cumsum(prec1[u,]*hit_vec[[u]])/sum(hit_vec[[u]])
    }
  })))
}
recs_MAP=function(recs,only_best=FALSE){
  return(colSums(recs_AveP(recs,only_best))/users)
}

recs_coverage=function(recs){
  cov1=lapply(1:items,function(i){unique(unlist(lapply(recs,function(x){x[i]})))})
  cov2=list(list())
  cov2[1]=cov1[1]
  for(i in 2:items){
    cov2[[i]]=unique(unlist(c(cov2[i-1],cov1[i])))
  }
  return(unlist(lapply(cov2,length))/items)
}

#------------------------------------------------------------------------------
# ocena systemu = wyliczenie wartości pewnej miary dla algorytmu

count_MAE=function(rating_function,args){
  value=0
  for(t1 in 1:5){
    read_ml_file(paste("ml-100k/u",t1,".base",sep=""))
    read_ml_test(paste("ml-100k/u",t1,".test",sep=""))
    rating=arguments_from_list(rating_function,args)
    value=value+rating_MAE(rating,ml_test)
  }
  return(value/5)
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

count_ROC_rating=function(rating_function,args,resolution=1000,quality=0){
  roc_sum=data.frame(rep(0,resolution),rep(0,resolution))
  for(t1 in 1:5){
    read_ml_file(paste("ml-100k/u",t1,".base",sep=""))
    read_ml_test(paste("ml-100k/u",t1,".test",sep=""))
    recs=rating_to_recs(arguments_from_list(rating_function,args),items)
    roc_sum=roc_sum+recs_ROC(recs,resolution,quality)
  }
  return(roc_sum/5)
}

count_ROC_recs=function(recs_function,resolution=1000,quality=0){
  roc_sum=data.frame(rep(0,resolution),rep(0,resolution))
  for(t1 in 1:5){
    read_ml_file(paste("ml-100k/u",t1,".base",sep=""))
    read_ml_test(paste("ml-100k/u",t1,".test",sep=""))
    recs=recs_function(items)
    roc_sum=roc_sum+recs_ROC(recs,resolution,quality)
  }
  return(roc_sum/5)
}

count_precision_rating=function(rating_function,args,only_best=FALSE){
  precision_sum=rep(0,items)
  for(t1 in 1:5){
    read_ml_file(paste("ml-100k/u",t1,".base",sep=""))
    read_ml_test(paste("ml-100k/u",t1,".test",sep=""))
    recs=rating_to_recs(arguments_from_list(rating_function,args),items)
    precision_sum=precision_sum+recs_precision(recs,only_best)
  }
  return(precision_sum/5)
}

count_precision_recs=function(recs_function,only_best=FALSE){
  precision_sum=rep(0,items)
  for(t1 in 1:5){
    read_ml_file(paste("ml-100k/u",t1,".base",sep=""))
    read_ml_test(paste("ml-100k/u",t1,".test",sep=""))
    recs=recs_function(items)
    precision_sum=precision_sum+recs_precision(recs,only_best)
  }
  return(precision_sum/5)
}

count_MAP_rating=function(rating_function,args,only_best=FALSE){
  MAP_sum=rep(0,items)
  for(t1 in 1:5){
    read_ml_file(paste("ml-100k/u",t1,".base",sep=""))
    read_ml_test(paste("ml-100k/u",t1,".test",sep=""))
    recs=rating_to_recs(arguments_from_list(rating_function,args),items)
    MAP_sum=MAP_sum+recs_MAP(recs,only_best)
  }
  return(MAP_sum/5)
}

count_MAP_recs=function(recs_function,only_best=FALSE){
  MAP_sum=rep(0,items)
  for(t1 in 1:5){
    read_ml_file(paste("ml-100k/u",t1,".base",sep=""))
    read_ml_test(paste("ml-100k/u",t1,".test",sep=""))
    recs=recs_function(items)
    MAP_sum=MAP_sum+recs_MAP(recs,only_best)
  }
  return(MAP_sum/5)
}

#------------------------------------------------------------------------------
# porównania algorytmów

# stworzenie list rekomendacyjnych dla kilku algorytmów na raz
multi_recs=function(functions_list,quick=FALSE){
  l=1+4*(!quick)
  len=length(functions_list)
  recs=matrix(list(),nrow=l,ncol=length(functions_list))
  names=list()
  for(i in 1:len){
    names[i]=functions_list[[i]][[1]]
  }
  colnames(recs)=names
  read_meta_file("ml-100k/u.item","ml-100k/u.genre")
  for(t1 in 1:l){
    read_ml_file(paste("ml-100k/u",t1,".base",sep=""))
    read_ml_test(paste("ml-100k/u",t1,".test",sep=""))
    for(i in 1:len){
    rating=arguments_from_list(functions_list[[i]][[2]],functions_list[[i]][[3]])
    recs[[t1,i]]=rating_to_recs(rating,items)
    }
  }
  return(recs)
}

# wyliczenie wszystkich miar jakości dla podanej listy alogorytmów
multi_evaluation_rating=function(functions_list,resolution=100,quick=FALSE){
  l=1+4*(!quick)
  len=length(functions_list)
  results=matrix(list(),nrow=13,ncol=length(functions_list))
  rownames(results)=c("MAE","MSE","ROC","AUC","quality ROC","quality AUC","bests ROC",
                      "bests AUC","Precision","MAP","bests Precision","bests MAP","Coverage")
  names=list()
  for(i in 1:len){
    names[i]=functions_list[[i]][[1]]
    results[[1,i]]=0
    results[[2,i]]=0
    results[[3,i]]=data.frame(rep(0,resolution),rep(0,resolution))
    results[[4,i]]=0
    results[[5,i]]=data.frame(rep(0,resolution),rep(0,resolution))
    results[[6,i]]=0
    results[[7,i]]=data.frame(rep(0,resolution),rep(0,resolution))
    results[[8,i]]=0
    results[[9,i]]=rep(0,items)
    results[[10,i]]=rep(0,items)
    results[[11,i]]=rep(0,items)
    results[[12,i]]=rep(0,items)
    results[[13,i]]=rep(0,items)
  }
  colnames(results)=names
  read_meta_file("ml-100k/u.item","ml-100k/u.genre")
  for(t1 in 1:l){
    read_ml_file(paste("ml-100k/u",t1,".base",sep=""))
    read_ml_test(paste("ml-100k/u",t1,".test",sep=""))
    for(i in 1:len){
      rating=arguments_from_list(functions_list[[i]][[2]],functions_list[[i]][[3]])
      recs=rating_to_recs(rating,items)
      results[[1,i]]=results[[1,i]]+rating_MAE(normalize_rating(rating,1,5),ml_test)
      results[[2,i]]=results[[2,i]]+rating_MSE(normalize_rating(rating,1,5),ml_test)
      results[[3,i]]=results[[3,i]]+recs_ROC(recs,resolution,quality=0)
      results[[5,i]]=results[[5,i]]+recs_ROC(recs,resolution,quality=1)
      results[[7,i]]=results[[7,i]]+recs_ROC(recs,resolution,quality=2)
      results[[9,i]]=results[[9,i]]+recs_precision(recs,only_best=FALSE)
      results[[10,i]]=results[[10,i]]+recs_MAP(recs,only_best=FALSE)     
      results[[11,i]]=results[[11,i]]+recs_precision(recs,only_best=TRUE)
      results[[12,i]]=results[[12,i]]+recs_MAP(recs,only_best=TRUE)
      results[[13,i]]=results[[13,i]]+recs_coverage(recs)
    }
  }
  for(i in 1:len){
    results[[1,i]]=results[[1,i]]/l
    results[[2,i]]=results[[2,i]]/l
    results[[3,i]]=results[[3,i]]/l
    results[[4,i]]=normalized_AUC(results[[3,i]],resolution)
    results[[5,i]]=results[[5,i]]/l
    results[[6,i]]=normalized_AUC(results[[5,i]],resolution)
    results[[7,i]]=results[[7,i]]/l
    results[[8,i]]=normalized_AUC(results[[7,i]],resolution)
    results[[9,i]]=results[[9,i]]/l 
    results[[10,i]]=results[[10,i]]/l
    results[[11,i]]=results[[11,i]]/l
    results[[12,i]]=results[[12,i]]/l 
    results[[13,i]]=results[[13,i]]/l 
  }
  return(results)
}

#------------------------------------------------------------------------------
# wykresy

# funkcje pomocnicze funkcji tworzącej wykresy
cut_result1=function(df1,point_list=c(-items*1000)){
  if(is.null(ncol(df1))){
    if(length(point_list)==1){
      return(data.frame(c(1:length(df1)),df1[point_list]))      
    }else{
      return(data.frame(point_list,df1[point_list]))
    }
  }else{
    return(df1[point_list,])
  }
}

cut_result=function(result_list,point_list=c(-items*1000)){
  lapply(result_list,function(x){cut_result1(x,point_list)})
}

cut_to_bar=function(result_list,point){
  if(is.null(ncol(result_list[[1]]))){
    return(lapply(result_list,function(x){x[point]}))
  }else{
    return(lapply(result_list,function(x){x[point,2]}))
  }
}

# funkcja tworząca wykres dla listy wyników algorytmów według jednej miary
# big - formatownie wymiarów 0 - małe okno wykresów, 1 - duże okno, 2 - plik
# point_list - obcięcie wykresu do konkretnych długości list rekomendacyjnych / odpowiednich części wykresu
multi_plot=function(result_list,title="",point_list=c(-items*1000),big=0,color=TRUE,legend=TRUE){
  l=length(result_list)
  if((length(point_list)==1)&&(point_list!=c(-items*1000))){
    title=paste(title," at ",point_list[[1]])
    result_list=cut_to_bar(result_list,point_list[1])
  }
  if((typeof(result_list[[1]])=="double")&&(length(result_list[[1]])==1)){
    if(color){
      plot1<-barplot(unlist(result_list),col=rainbow(l),main=title,cex.main=2)
    }else{
      plot1<-barplot(unlist(result_list),main=title,cex.main=2)
    }
    text(plot1,round(unlist(result_list),digits=4),labels=round(unlist(result_list),digits=4),pos=1,cex=2)
  }else{
    result_list=cut_result(result_list,point_list)
    xlim1=c(min(result_list[[1]][,1]),max(result_list[[1]][,1]))
    ymin1=min(unlist(lapply(result_list,function(x){min(x[,2])})))
    ymax1=max(unlist(lapply(result_list,function(x){max(x[,2])})))
    ylim1=c(ymin1,ymax1)
    if(big==1){
      inset1=c(-0.28,-0.15)
      seglen1=1
      xinter1=0.3
      cex1=1
      par(mar=c(3,3,3,9)) 
    }else if(big==2){
      inset1=c(-0.25,-0.07)
      seglen1=3
      xinter1=1
      cex1=1.7
      par(mar=c(3,3,3,14))
    }else{
      inset1=c(-0.3,-0.3)
      seglen1=2
      xinter1=1
      cex1=1
      par(mar=c(3,3,3,5.5))
    }
    for(i in 1:l){
      df1=result_list[[i]]
      df2=df1[(c(0:10)+i/l)*(nrow(df1)/10),]
      if(color){
        plot(df1,type="l",col=rainbow(l)[i],main=title,cex.main=2,xlab="",ylab="",xlim=xlim1,ylim=ylim1,bty="L",lwd=1.5)
      }else{
        plot(df1,type="l",main=title,cex.main=2,xlab="",ylab="",xlim=xlim1,ylim=ylim1,bty="L",lty=i,lwd=1.5)
        par(new=TRUE)
        plot(df2,type="p",main=title,cex.main=2,xlab="",ylab="",xlim=xlim1,ylim=ylim1,pch=i%%26,bty="L",cex=2)
      }
      par(new=TRUE)
    }
    if((legend)&&(l>1)){
      if(color){
        legend("bottomright",inset=inset1,xpd=TRUE,legend=names(result_list),col=rainbow(l),lty=1,box.lwd=0,bg="transparent",seg.len=seglen1,x.intersp=xinter1,cex=cex1)
      }else{
        legend("bottomright",inset=inset1,xpd=TRUE,legend=names(result_list),box.lwd=0,lty=c(1:l),pch=c(1:l)%%26,bg="transparent",seg.len=seglen1,x.intersp=xinter1,cex=cex1)
      }
    }
    par(new=FALSE)
  }
}

# stworzenie wykresu i zapisanie do pliku .jpg
plot_to_file=function(file_name,result_list,title,point_list=c(-items*1000),color=TRUE,width1=3000,height1=1500,legend=TRUE){
  jpeg(file_name,width=width1,height=height1,units="px",res=200)
  multi_plot(result_list,title,point_list,big=2,color,legend)
  dev.off()
}