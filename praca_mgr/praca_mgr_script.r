source("basic.r")
source("evaluation.r")
source("CF.r")
source("SVD.r")

read_ml_file("ml-100k/u.data")
read_meta_file("ml-100k/u.item","ml-100k/u.genre")
read_ml_file(paste("ml-100k/u3.base",sep=""))
read_ml_test(paste("ml-100k/u3.test",sep=""))



non_per_rating=rep(1,users)%*%t.default(mov_means)
trivial_rating=((rep(1,users)%*%t.default(mov_means))+(us_means%*%t.default(rep(1,items))))/2

system.time({
  cor_rating=CF_ratings(cor_similarity)
  cos_rating=CF_ratings(cos_similarity)
  item_cor_rating=item_CF_ratings(item_cor_similarity)
  item_cos_rating=item_CF_ratings(item_cos_similarity)
  SO_rating1=SO_ratings()
})

CF_recs=rating_to_recs(CF_predicted_ratings,10)

SVD(1,10,0.03)
SVDpp(1,3,0.01)

roc0=recs_ROC(random_recs(items),1000)
non_per=non_personalized_recs(items)
roc4=recs_ROC(non_per,1000)
plot(roc3,type="l",col="green")
par(new=TRUE)
plot(trivial_roc(1000),type="l",col="red")
par(new=TRUE)
plot(roc4,type="l",col="blue")
normalized_AUC(roc3,1000)
normalized_AUC(roc4,1000)
normalized_AUC(trivial_roc(1000),1000)

roc6=count_ROC_rating(SVD_ratings,c(1,7,0.02),1000)
roc7=count_ROC_rating(BPR_pseudo_ratings,c(1,7,0.02),1000)

BPR_rating1=BPR_pseudo_ratings(1,10,0.03)


c1=list(list(list()))
c1[[1]][[1]]="non per"
c1[[1]][[2]]=non_personalized_rating
c1[[1]][[3]]=c(0)
functions_list=c1
c1[[1]][[1]]="CF cor"
c1[[1]][[2]]=CF_ratings
c1[[1]][[3]]=c(cor_similarity)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="CF cos"
c1[[1]][[2]]=CF_ratings
c1[[1]][[3]]=c(cos_similarity)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="SO"
c1[[1]][[2]]=SO_ratings
c1[[1]][[3]]=c(0)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="SVD"
c1[[1]][[2]]=SVD_ratings
c1[[1]][[3]]=c(1,4,0.01)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="SVD++"
c1[[1]][[2]]=SVDpp_ratings
c1[[1]][[3]]=c(1,4,0.01)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="gSVD++"
c1[[1]][[2]]=gSVDpp_ratings
c1[[1]][[3]]=c(1,4,0.01)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="BPR"
c1[[1]][[2]]=BPR_pseudo_ratings
c1[[1]][[3]]=c(1,4,0.01)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="MABPR"
c1[[1]][[2]]=MABPR_pseudo_ratings
c1[[1]][[3]]=c(1,4,0.01)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="MABPR gSVD++"
c1[[1]][[2]]=MABPR_gSVDpp_pseudo_ratings
c1[[1]][[3]]=c(1,4,0.01)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="COMPLEX"
c1[[1]][[2]]=COMPLEX_pseudo_ratings
c1[[1]][[3]]=list(c(1,1e-06))
functions_list=c(functions_list,c1)
system.time({
results_matrix=multi_evaluation_rating(functions_list,quick=TRUE)
})

mplot1=function(x){
  multi_plot(results_matrix[x,],rownames(results_matrix)[x])
}

c1=list(list(list()))
c1[[1]][[1]]="SVDpp"
c1[[1]][[2]]=SVDpp_ratings
c1[[1]][[3]]=c(20,5,0.01)
functions_list=c1
c1[[1]][[1]]="gSVDpp"
c1[[1]][[2]]=gSVDpp_ratings
c1[[1]][[3]]=c(20,5,0.01)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="SVDpp0"
c1[[1]][[2]]=SVDpp_ratings
c1[[1]][[3]]=c(20,5,0.01,0,0,0,0,0)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="gSVDpp0"
c1[[1]][[2]]=gSVDpp_ratings
c1[[1]][[3]]=c(20,5,0.01,0,0,0,0,0,0)
functions_list=c(functions_list,c1)
system.time({
results_matrix23=multi_evaluation_rating(functions_list,quick=TRUE)
})
system.time({
  roc2=recs_ROC(recs2)
})

system.time(SVDpp(20,5,0.01))
system.time(SVDpp2(20,5,0.01))
system.time(SVDpp3(20,5,0.01))
system.time(SVDpp4(20,5,0.01))
system.time(SVD(20,5,0.01))
system.time(SVD2(20,5,0.01))
system.time(gSVDpp(1,5,0.01))
rat5=SVD_ratings3(40,5,0.03,3,6)
#TODO można teraz przetestować, czy zwiększenie czybkości uczenia nie niszczy

multi_plot(results_matrix4[2,],rownames(results_matrix4)[2])

png("1.png",width=3000,height=1500,units="px",res=200)
multi_plot(results_matrix[6,],rownames(results_matrix)[6],big=2,color=FALSE)
dev.off()

system.time({
MSE1=count_MSE(SVDpp_ratings,c(1,10,0.01))
})
