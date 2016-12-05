# SKRYPT Z INSTRUKCJAMI UŻYTYMI DO WYLICZENIA DANYCH ZAMIESZCZONYCH W PRACY

source("basic.r")
source("evaluation.r")
source("CF.r")
source("SVD.r")

{
# non personalized
{
c1=list(list(list()))
c1[[1]][[1]]="non per"
c1[[1]][[2]]=non_personalized_rating
c1[[1]][[3]]=c(FALSE)
functions_list=c1
c1[[1]][[1]]="non per pop"
c1[[1]][[2]]=non_personalized_rating
c1[[1]][[3]]=c(TRUE)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="random"
c1[[1]][[2]]=random_rating
c1[[1]][[3]]=c(1,5)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="optimal"
c1[[1]][[2]]=optimal_rating
c1[[1]][[3]]=c(0)
functions_list=c(functions_list,c1)
res_non_per=multi_evaluation_rating(functions_list,quick=FALSE)
}
# CF Paerson corelation
{
c1=list(list(list()))
c1[[1]][[1]]="CF cor"
c1[[1]][[2]]=CF_ratings
c1[[1]][[3]]=c(cor_similarity,FALSE,FALSE)
functions_list=c1
c1[[1]][[1]]="CF cor SF"
c1[[1]][[2]]=CF_ratings
c1[[1]][[3]]=c(cor_similarity,FALSE,TRUE)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="CF cor item"
c1[[1]][[2]]=CF_ratings
c1[[1]][[3]]=c(item_cor_similarity,TRUE,FALSE)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="CF cor SF item"
c1[[1]][[2]]=CF_ratings
c1[[1]][[3]]=c(item_cor_similarity,TRUE,TRUE)
functions_list=c(functions_list,c1)
res_CF_cor=multi_evaluation_rating(functions_list,quick=FALSE)
}
# CF cosine
{
c1=list(list(list()))
c1[[1]][[1]]="CF cos"
c1[[1]][[2]]=CF_ratings
c1[[1]][[3]]=c(cos_similarity,FALSE,FALSE)
functions_list=c1
c1[[1]][[1]]="CF cos SF"
c1[[1]][[2]]=CF_ratings
c1[[1]][[3]]=c(cos_similarity,FALSE,TRUE)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="CF cos item"
c1[[1]][[2]]=CF_ratings
c1[[1]][[3]]=c(item_cos_similarity,TRUE,FALSE)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="CF cos SF item"
c1[[1]][[2]]=CF_ratings
c1[[1]][[3]]=c(item_cos_similarity,TRUE,TRUE)
functions_list=c(functions_list,c1)
res_CF_cos=multi_evaluation_rating(functions_list,quick=FALSE)
}
# CF mixed
{
c1=list(list(list()))
c1[[1]][[1]]="CF mix"
c1[[1]][[2]]=CF_ratings
c1[[1]][[3]]=c(mixed_similarity,FALSE,FALSE)
functions_list=c1
c1[[1]][[1]]="CF mix SF"
c1[[1]][[2]]=CF_ratings
c1[[1]][[3]]=c(mixed_similarity,FALSE,TRUE)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="CF mix item"
c1[[1]][[2]]=CF_ratings
c1[[1]][[3]]=c(item_mixed_similarity,TRUE,FALSE)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="CF mix SF item"
c1[[1]][[2]]=CF_ratings
c1[[1]][[3]]=c(item_mixed_similarity,TRUE,TRUE)
functions_list=c(functions_list,c1)
res_CF_mix=multi_evaluation_rating(functions_list,quick=FALSE)
}
# CF-ADV
{
c1=list(list(list()))
c1[[1]][[1]]="CF-ADV"
c1[[1]][[2]]=CF_ratings_further
c1[[1]][[3]]=c(FALSE)
functions_list=c1
c1[[1]][[1]]="CF-ADV item"
c1[[1]][[2]]=CF_ratings_further
c1[[1]][[3]]=c(TRUE)
functions_list=c(functions_list,c1)
res_CF_ADV=multi_evaluation_rating(functions_list,quick=FALSE)
}
# SO
{
c1=list(list(list()))
c1[[1]][[1]]="SO"
c1[[1]][[2]]=SO_ratings
c1[[1]][[3]]=c(0)
functions_list=c1
res_SO=multi_evaluation_rating(functions_list,quick=FALSE)
}
# SVD
{
c1=list(list(list()))
c1[[1]][[1]]="SVD"
c1[[1]][[2]]=SVD_ratings
c1[[1]][[3]]=c(40,5,0.005)
functions_list=c1
c1[[1]][[1]]="SVD++"
c1[[1]][[2]]=SVDpp_ratings
c1[[1]][[3]]=c(40,5,0.005)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="gSVD++"
c1[[1]][[2]]=gSVDpp_ratings
c1[[1]][[3]]=c(40,5,0.005)
functions_list=c(functions_list,c1)
res_SVD=multi_evaluation_rating(functions_list,quick=FALSE)
}
# BPR
{
c1=list(list(list()))
c1[[1]][[1]]="BPR"
c1[[1]][[2]]=BPR_pseudo_ratings
c1[[1]][[3]]=c(40,5,0.02)
functions_list=c1
c1[[1]][[1]]="MABPR"
c1[[1]][[2]]=MABPR_pseudo_ratings
c1[[1]][[3]]=c(40,5,0.02)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="MABPR gSVD++"
c1[[1]][[2]]=MABPR_gSVDpp_pseudo_ratings
c1[[1]][[3]]=c(40,5,0.02)
functions_list=c(functions_list,c1)  
res_BPR=multi_evaluation_rating(functions_list,quick=FALSE)
}
# Complex
{
c1=list(list(list()))
c1[[1]][[1]]="Complex"
c1[[1]][[2]]=COMPLEX_pseudo_ratings
c1[[1]][[3]]=c(c(1),TRUE,FALSE,FALSE)
functions_list=c1
c1[[1]][[1]]="Complex impl"
c1[[1]][[2]]=COMPLEX_pseudo_ratings
c1[[1]][[3]]=list(c(1),FALSE,FALSE,FALSE)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="Complex i"
c1[[1]][[2]]=COMPLEX_pseudo_ratings
c1[[1]][[3]]=c(c(1),TRUE,TRUE,FALSE)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="Complex impl i"
c1[[1]][[2]]=COMPLEX_pseudo_ratings
c1[[1]][[3]]=list(c(1),FALSE,TRUE,FALSE)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="Complex u"
c1[[1]][[2]]=COMPLEX_pseudo_ratings
c1[[1]][[3]]=c(c(1),TRUE,FALSE,TRUE)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="Complex impl u"
c1[[1]][[2]]=COMPLEX_pseudo_ratings
c1[[1]][[3]]=list(c(1),FALSE,FALSE,TRUE)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="Complex iu"
c1[[1]][[2]]=COMPLEX_pseudo_ratings
c1[[1]][[3]]=c(c(1),TRUE,TRUE,TRUE)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="Complex impl iu"
c1[[1]][[2]]=COMPLEX_pseudo_ratings
c1[[1]][[3]]=list(c(1),FALSE,TRUE,TRUE)
functions_list=c(functions_list,c1)
res_Complex=multi_evaluation_rating(functions_list,quick=FALSE)
}
# porównanie czasowe
{
  read_ml_file(paste("ml-100k/u3.base",sep=""))
  read_ml_test(paste("ml-100k/u3.test",sep=""))
  
  time_list=list()
  time_list2=list()
  time_list2[[1]]=system.time({
    CF_ratings(cor_similarity,item_sim=FALSE)
  })
  time_list2[[2]]=system.time({
    CF_ratings(item_cor_similarity,item_sim=TRUE)
  })
  time_list2[[3]]=system.time({
    CF_ratings(mixed_similarity,item_sim=FALSE)
  })
  time_list2[[4]]=system.time({
    CF_ratings(item_mixed_similarity,item_sim=TRUE)
  })
  time_list2[[5]]=system.time({
    CF_ratings_further(item_sim=FALSE)
  })
  time_list2[[6]]=system.time({
    CF_ratings_further(item_sim=TRUE)
  })
  time_list2[[7]]=system.time({
    CF_ratings_further2(item_sim=FALSE)
  })
  time_list2[[8]]=system.time({
    CF_ratings_further2(item_sim=TRUE)
  })
  time_list2[[9]]=system.time({
    SO_ratings()
  })
  names(time_list2)=c("CF","CF item","CF mixed","CF mixed item","CF further","CF further item","CF further slow","CF further item slow","SO")
  time_list=time_list2
  time_list2=list()
  time_list2[[1]]=system.time({
    SVD_ratings(20,5,0.01)
  })
  time_list2[[2]]=system.time({
    SVDpp_ratings(20,5,0.01)
  })
  time_list2[[3]]=system.time({
    gSVDpp_ratings(20,5,0.01)
  })
  time_list2[[4]]=system.time({
    BPR_pseudo_ratings(20,5,0.04)
  })
  time_list2[[5]]=system.time({
    MABPR_pseudo_ratings(20,5,0.04)
  })
  time_list2[[6]]=system.time({
    MABPR_gSVDpp_pseudo_ratings(20,5,0.04)
  })
  names(time_list2)=c("SVD20","SVD++20","gSVD++20","BPR20","MABPR20","MABPR gSVD++20")
  time_list=c(time_list,time_list2)
  time_list2=list()
  time_list2[[1]]=system.time({
    SVD_ratings(40,5,0.005)
  })
  time_list2[[2]]=system.time({
    SVDpp_ratings(40,5,0.005)
  })
  time_list2[[3]]=system.time({
    gSVDpp_ratings(40,5,0.005)
  })
  time_list2[[4]]=system.time({
    BPR_pseudo_ratings(40,5,0.02)
  })
  time_list2[[5]]=system.time({
    MABPR_pseudo_ratings(40,5,0.02)
  })
  time_list2[[6]]=system.time({
    MABPR_gSVDpp_pseudo_ratings(40,5,0.02)
  })
  names(time_list2)=c("SVD40","SVD++40","gSVD++40","BPR40","MABPR40","MABPR gSVD++40")
  time_list=c(time_list,time_list2)
  time_list2=list()
  time_list2[[1]]=system.time({
    COMPLEX_pseudo_ratings(c(1),user_reg=FALSE,item_reg=FALSE)
  })
  time_list2[[2]]=system.time({
    COMPLEX_pseudo_ratings(c(1,10^-6),user_reg=FALSE,item_reg=FALSE)
  })
  time_list2[[3]]=system.time({
    COMPLEX_pseudo_ratings(c(1,10^-6,10^-11),user_reg=FALSE,item_reg=FALSE)
  })
  time_list2[[4]]=system.time({
    COMPLEX_pseudo_ratings2(c(1),user_reg=FALSE,item_reg=FALSE)
  })
  time_list2[[5]]=system.time({
    COMPLEX_pseudo_ratings2(c(1,10^-6),user_reg=FALSE,item_reg=FALSE)
  })
  time_list2[[6]]=system.time({
    COMPLEX_pseudo_ratings2(c(1,10^-6,10^-11),user_reg=FALSE,item_reg=FALSE)
  })
  names(time_list2)=c("COMPLEX","COMPLEX 2","COMPLEX 3","COMPLEX slow","COMPLEX 2 slow","COMPLEX 3 slow")
  time_list=c(time_list,time_list2)
  time_list3=unlist(time_list)[(1:length(time_list))*5-4]
  names(time_list3)=names(time_list)
  time_list=time_list3
}
# SVD i BPR z mniejszą ilością iteracji
{
c1=list(list(list()))
c1[[1]][[1]]="SVD20"
c1[[1]][[2]]=SVD_ratings
c1[[1]][[3]]=c(20,5,0.01)
functions_list=c1
c1[[1]][[1]]="SVD++20"
c1[[1]][[2]]=SVDpp_ratings
c1[[1]][[3]]=c(20,5,0.01)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="gSVD++20"
c1[[1]][[2]]=gSVDpp_ratings
c1[[1]][[3]]=c(20,5,0.01)
functions_list=c(functions_list,c1)
res_SVD_20iter=multi_evaluation_rating(functions_list,quick=FALSE)
}
{
c1=list(list(list()))
c1[[1]][[1]]="BPR20"
c1[[1]][[2]]=BPR_pseudo_ratings
c1[[1]][[3]]=c(20,5,0.04)
functions_list=c1
c1[[1]][[1]]="MABPR20"
c1[[1]][[2]]=MABPR_pseudo_ratings
c1[[1]][[3]]=c(20,5,0.04)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="MABPR gSVD++20"
c1[[1]][[2]]=MABPR_gSVDpp_pseudo_ratings
c1[[1]][[3]]=c(20,5,0.04)
functions_list=c(functions_list,c1)
res_BPR_20iter=multi_evaluation_rating(functions_list,quick=FALSE)
}
}

# zapisanie wyliczonych danych
save(res_non_per,res_CF_cor,res_CF_cos,res_CF_mix,res_CF_ADV,res_SO,res_SVD,res_BPR,res_Complex,file="results")
save(res_SVD_20iter,res_BPR_20iter,time_list,file="extra_results")

# wczytywanie gotowych danych
load(file="results")
load(file="extra_results")

res_CF=cbind(res_CF_cor,res_CF_cos,res_CF_mix,res_CF_ADV)
res_all=cbind(res_non_per,res_CF,res_SO,res_SVD,res_BPR,res_Complex)

# podziały algorytmów
{
res_rat_pred=res_all[,c(1,3:22)]
res_rest=res_all[,c(2,3,23:33)]
res_CF_user=res_rat_pred[,c(4,5,8,9,12,13,16)]
res_CF_item=res_rat_pred[,c(6,7,10,11,14,15,17)]
colnames(res_CF_item)=colnames(res_CF_user)
res_rat_pred2=res_rat_pred[,c(1,6,17,18:21)]
}

# wykresy
{
plot_to_file("../img/CF_item_MAE_color.jpg",res_CF_item[1,],"MAE dla algorytmów CF z podobieństwem przedmiotów",height1=1000)
plot_to_file("../img/CF_item_MSE_color.jpg",res_CF_item[2,],"MSE dla algorytmów CF z podobieństwem przedmiotów",height1=1000)
plot_to_file("../img/CF_item_MAE.jpg",res_CF_item[1,],"MAE dla algorytmów CF z podobieństwem przedmiotów",col=FALSE,height1=1000)
plot_to_file("../img/CF_item_MSE.jpg",res_CF_item[2,],"MSE dla algorytmów CF z podobieństwem przedmiotów",col=FALSE,height1=1000)

plot_to_file("../img/MAE2_color.jpg",res_rat_pred2[1,],"Porównanie MAE dla róźnych algorytmów",height1=1000)
plot_to_file("../img/MSE2_color.jpg",res_rat_pred2[2,],"Porównanie MSE dla róźnych algorytmów",height1=1000)
plot_to_file("../img/MAE2.jpg",res_rat_pred2[1,],"Porównanie MAE dla róźnych algorytmów",col=FALSE,height1=1000)
plot_to_file("../img/MSE2.jpg",res_rat_pred2[2,],"Porównanie MSE dla róźnych algorytmów",col=FALSE,height1=1000)

plot_to_file("../img/ROC_all_color.jpg",res_all[3,],"ROC",legend=FALSE)
plot_to_file("../img/Prec_all_100_color.jpg",res_all[9,],"Precision",point_list=1:100,legend=FALSE)
plot_to_file("../img/ROC_all.jpg",res_all[3,],"ROC",col=FALSE,legend=FALSE)
plot_to_file("../img/Prec_all_100.jpg",res_all[9,],"Precision",point_list=1:100,col=FALSE,legend=FALSE)

plot_to_file("../img/Prec_Complex_impl_color.jpg",res_Complex[9,c(2,4,6,8,7)],"Precyzja (wszystkie użycia) algorytmów Complex",point_list=1:30,height1=1000)
plot_to_file("../img/Prec_Complex_exp_color.jpg",res_Complex[11,c(1,3,5,7,8)],"Precyzja (same maksymalne oceny) algorytmów Complex",point_list=1:30,height1=1000)
plot_to_file("../img/Prec_Complex_impl.jpg",res_Complex[9,c(2,4,6,8,7)],"Precyzja (wszystkie użycia) algorytmów Complex",point_list=1:30,col=FALSE,height1=1000)
plot_to_file("../img/Prec_Complex_exp.jpg",res_Complex[11,c(1,3,5,7,8)],"Precyzja (same maksymalne oceny) algorytmów Complex",point_list=1:30,col=FALSE,height1=1000)

plot_to_file("../img/ROC_Complex_all_color.jpg",res_Complex[3,],"ROC (wszystkie użycia) algorytmów Complex",height1=1000)
plot_to_file("../img/ROC_Complex_best_color.jpg",res_Complex[7,],"ROC (same maksymalne oceny) algorytmów Complex",height1=1000)
plot_to_file("../img/ROC_Complex_all.jpg",res_Complex[3,],"ROC (wszystkie użycia) algorytmów Complex",col=FALSE,height1=1000)
plot_to_file("../img/ROC_Complex_best.jpg",res_Complex[7,],"ROC (same maksymalne oceny) algorytmów Complex",col=FALSE,height1=1000)

plot_to_file("../img/Prec_BPR_color.jpg",cbind(res_BPR,res_Complex)[9,c(1:3,11)],"Precyzja algorytmów BPR (Complex dla porównania)",point_list=1:30,height1=1000)
plot_to_file("../img/ROC_BPR_color.jpg",cbind(res_BPR,res_Complex)[3,c(1:3,11)],"ROC algorytmów BPR",height1=1000)
plot_to_file("../img/MAP_BPR_color.jpg",cbind(res_BPR,res_Complex)[10,c(1:3,11)],"MAP algorytmów BPR",point_list=1:30,height1=1000)
plot_to_file("../img/Prec_BPR.jpg",cbind(res_BPR,res_Complex)[9,c(1:3,11)],"Precyzja algorytmów BPR (Complex dla porównania)",point_list=1:30,col=FALSE,height1=1000)
plot_to_file("../img/ROC_BPR.jpg",cbind(res_BPR,res_Complex)[3,c(1:3,11)],"ROC algorytmów BPR",col=FALSE,height1=1000)
plot_to_file("../img/MAP_BPR.jpg",cbind(res_BPR,res_Complex)[10,c(1:3,11)],"MAP algorytmów BPR",point_list=1:30,col=FALSE,height1=1000)

plot_to_file("../img/Cov_non_per_color.jpg",res_non_per[13,],"Pokrycie algorytmów niespersonalizowanych i sztucznych",point_list=1:100,height1=1000)
plot_to_file("../img/Cov_CF_color.jpg",cbind(res_CF_item,res_SO)[13,c(2,4,6,7,8)],"Pokrycie algorytmów CF (z podobieństwem przedmiotów i SF)",point_list=1:100,height1=1000)
plot_to_file("../img/Cov_SVD_color.jpg",cbind(res_SVD,res_BPR)[13,],"Pokrycie algorytmów SVD i BPR",point_list=1:100,height1=1000)
plot_to_file("../img/Cov_Complex_color.jpg",res_Complex[13,],"Pokrycie algorytmów Complex",point_list=1:100,height1=1000)
plot_to_file("../img/Cov_between_color.jpg",res_all[13,c(3,4,8,25,30)],"Porównanie pokryć między grupami",point_list=1:100,height1=1000)
plot_to_file("../img/Cov_non_per.jpg",res_non_per[13,],"Pokrycie algorytmów niespersonalizowanych i sztucznych",point_list=1:100,col=FALSE,height1=1000)
plot_to_file("../img/Cov_CF.jpg",cbind(res_CF_item,res_SO)[13,c(2,4,6,7,8)],"Pokrycie algorytmów CF (z podobieństwem przedmiotów i SF)",point_list=1:100,col=FALSE,height1=1000)
plot_to_file("../img/Cov_SVD.jpg",cbind(res_SVD,res_BPR)[13,],"Pokrycie algorytmów SVD i BPR",point_list=1:100,col=FALSE,height1=1000)
plot_to_file("../img/Cov_Complex.jpg",res_Complex[13,],"Pokrycie algorytmów Complex",point_list=1:100,col=FALSE,height1=1000)
plot_to_file("../img/Cov_between.jpg",res_all[13,c(3,4,8,25,30)],"Porównanie pokryć między grupami",point_list=1:100,col=FALSE,height1=1000)
}

# wartości użyte w pracy
{
item_user_diff_MAE=1-unlist(res_CF_item[1,])/unlist(res_CF_user[1,])
item_user_diff_MSE=1-unlist(res_CF_item[2,])/unlist(res_CF_user[2,])  
mean(item_user_diff_MAE)
mean(item_user_diff_MSE)

min(unlist(res_rat_pred[1,-c(2,3)]))
max(unlist(res_rat_pred[1,-c(2,3)]))
min(unlist(res_rest[1,-2]))
max(unlist(res_rest[1,-2]))

min(unlist(res_rat_pred[2,-c(2,3)]))
max(unlist(res_rat_pred[2,-c(2,3)]))
min(unlist(res_rest[2,-2]))
max(unlist(res_rest[2,-2]))

min(unlist(res_rat_pred[4,-c(2,3)]))
max(unlist(res_rat_pred[4,-c(2,3)]))
min(unlist(res_rest[4,-2]))
max(unlist(res_rest[4,-2]))
}