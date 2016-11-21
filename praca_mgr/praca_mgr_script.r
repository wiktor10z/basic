source("basic.r")
source("evaluation.r")
source("CF.r")
source("SVD.r")

#non personalized
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
system.time({
  res_non_per=multi_evaluation_rating(functions_list,quick=FALSE)
})
}
#CF Paerson corelation
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
system.time({
  res_CF_cor=multi_evaluation_rating(functions_list,quick=FALSE)
})
}
#CF cosine
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
system.time({
  res_CF_cos=multi_evaluation_rating(functions_list,quick=FALSE)
})
}
#CF mixed
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
  system.time({
    res_CF_mix=multi_evaluation_rating(functions_list,quick=FALSE)
  })
}
#CF-ADV
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
system.time({
  res_CF_ADV=multi_evaluation_rating(functions_list,quick=FALSE)
})
}
#SO
{
c1=list(list(list()))
c1[[1]][[1]]="SO"
c1[[1]][[2]]=SO_ratings
c1[[1]][[3]]=c(0)
functions_list=c1
system.time({
  res_SO=multi_evaluation_rating(functions_list,quick=FALSE)
})
}
#SVD
{
c1=list(list(list()))
c1[[1]][[1]]="SVD"
c1[[1]][[2]]=SVD_ratings
c1[[1]][[3]]=c(20,5,0.01)
functions_list=c1
c1[[1]][[1]]="SVD++"
c1[[1]][[2]]=SVDpp_ratings
c1[[1]][[3]]=c(20,5,0.01)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="gSVD++"
c1[[1]][[2]]=gSVDpp_ratings
c1[[1]][[3]]=c(20,5,0.01)
functions_list=c(functions_list,c1)
system.time({
  res_SVD=multi_evaluation_rating(functions_list,quick=FALSE)
})
}
#BPR
{
c1=list(list(list()))
c1[[1]][[1]]="BPR"
c1[[1]][[2]]=BPR_pseudo_ratings
c1[[1]][[3]]=c(20,5,0.04)
functions_list=c1
c1[[1]][[1]]="MABPR"
c1[[1]][[2]]=MABPR_pseudo_ratings
c1[[1]][[3]]=c(20,5,0.04)
functions_list=c(functions_list,c1)
c1[[1]][[1]]="MABPR gSVD++"
c1[[1]][[2]]=MABPR_gSVDpp_pseudo_ratings
c1[[1]][[3]]=c(20,5,0.04)
functions_list=c(functions_list,c1)  
system.time({
  res_BPR=multi_evaluation_rating(functions_list,quick=FALSE)
})
}
#Complex
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
system.time({
  res_Complex=multi_evaluation_rating(functions_list,quick=FALSE)
})
}

save(res_non_per,res_CF_cor,res_CF_cos,res_CF_mix,res_CF_ADV,res_SO,res_SVD,res_BPR,res_Complex,file="results")
load(file="results")
res_all=cbind(res_non_per,res_CF_cor,res_CF_cos,res_CF_mix,res_CF_ADV,res_SO,res_SVD,res_BPR,res_Complex)