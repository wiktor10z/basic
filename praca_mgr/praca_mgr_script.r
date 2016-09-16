source("basic.r")
source("CF_functions.R")
source("SVD.r")

read_ml_file("ml-100k/u.data")
read_ml_file(paste("ml-100k/u1.base",sep=""))
read_ml_test(paste("ml-100k/u1.test",sep=""))



non_per_rating=rep(1,users)%*%t.default(mov_means)
trivial_rating=((rep(1,users)%*%t.default(mov_means))+(us_means%*%t.default(rep(1,items))))/2

system.time({
  cor_rating=CF_ratings(cor_similarity)
  cos_rating=CF_ratings(cos_similarity)
})


CF_propos=rating_to_propos(CF_predicted_ratings,10)

SVD(1,10,0.03)
SVDpp(1,3,0.01)

roc0=propos_ROC(random_recs(items),1000)
non_per=non_personalized_recs(items)
roc4=propos_ROC(non_per,1000)
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

system.time({
MSE1=count_MSE(SVDpp_ratings,c(1,10,0.01))
})
