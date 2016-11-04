source("basic.r")
source("evaluation.r")
source("CF.r")
source("SVD.r")

read_ml_file("ml-100k/u.data")
read_meta_file("ml-100k/u.item","ml-100k/u.genre")
read_ml_file(paste("ml-100k/u1.base",sep=""))
read_ml_test(paste("ml-100k/u1.test",sep=""))

{
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
  c1[[1]][[3]]=c(20,5,0.02,0,0,0,0)
  functions_list=c(functions_list,c1)
  c1[[1]][[1]]="SVD++"
  c1[[1]][[2]]=SVDpp_ratings
  c1[[1]][[3]]=c(20,5,0.02,0,0,0,0,0)
  functions_list=c(functions_list,c1)
  c1[[1]][[1]]="gSVD++"
  c1[[1]][[2]]=gSVDpp_ratings
  c1[[1]][[3]]=c(20,5,0.02,0,0,0,0,0,0)
  functions_list=c(functions_list,c1)
  c1[[1]][[1]]="BPR"
  c1[[1]][[2]]=BPR_pseudo_ratings
  c1[[1]][[3]]=c(20,5,0.02,TRUE,0,0,0,0)
  functions_list=c(functions_list,c1)
  c1[[1]][[1]]="MABPR"
  c1[[1]][[2]]=MABPR_pseudo_ratings
  c1[[1]][[3]]=c(20,5,0.02,TRUE,0,0,0,0,0)
  functions_list=c(functions_list,c1)
  c1[[1]][[1]]="MABPR gSVD++"
  c1[[1]][[2]]=MABPR_gSVDpp_pseudo_ratings
  c1[[1]][[3]]=c(20,5,0.02,TRUE,0,0,0,0,0,0,0,0)
  functions_list=c(functions_list,c1)
  c1[[1]][[1]]="COMPLEX"
  c1[[1]][[2]]=COMPLEX_pseudo_ratings
  c1[[1]][[3]]=list(c(1),TRUE,FALSE,FALSE)
  functions_list=c(functions_list,c1)
  c1[[1]][[1]]="COMPLEX impl"
  c1[[1]][[2]]=COMPLEX_pseudo_ratings
  c1[[1]][[3]]=list(c(1),FALSE,FALSE,FALSE)
  functions_list=c(functions_list,c1)
  system.time({
    results_matrix1=multi_evaluation_rating(functions_list,quick=TRUE)
  })
}
{
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
c1[[1]][[3]]=c(20,5,0.01)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="SVD++"
c1[[1]][[2]]=SVDpp_ratings
c1[[1]][[3]]=c(20,5,0.01)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="gSVD++"
c1[[1]][[2]]=gSVDpp_ratings
c1[[1]][[3]]=c(20,5,0.01)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="BPR"
c1[[1]][[2]]=BPR_pseudo_ratings
c1[[1]][[3]]=c(20,5,0.04)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="MABPR"
c1[[1]][[2]]=MABPR_pseudo_ratings
c1[[1]][[3]]=c(20,5,0.04)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="MABPR gSVD++"
c1[[1]][[2]]=MABPR_gSVDpp_pseudo_ratings
c1[[1]][[3]]=c(20,5,0.04)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="COMPLEX"
c1[[1]][[2]]=COMPLEX_pseudo_ratings
c1[[1]][[3]]=list(c(1),TRUE)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="COMPLEX impl"
c1[[1]][[2]]=COMPLEX_pseudo_ratings
c1[[1]][[3]]=list(c(1),FALSE)
functions_list=c(functions_list,c1)
system.time({
results_matrix2=multi_evaluation_rating(functions_list,quick=TRUE)
})
system.time({
recs2=multi_recs(functions_list,quick=TRUE)
})
}


multi_plot(results_matrix4[2,],rownames(results_matrix4)[2])

plot_to_file("1.png",results_matrix[6,],rownames(results_matrix)[6],color=FALSE)
