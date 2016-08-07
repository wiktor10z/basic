source("basic.r")
source("CF_functions.R")
read_ml_file("ml-100k/u.data")
mean(mov_means)
non_per_propos=non_personalized_recs(10)
system.time({
  cov_matrix=make_sim_matrix(cov_similarity)
  cos_matrix=make_sim_matrix(cos_similarity)
})
system.time({
  CF_predicted_ratings=CF_predict_all(cov_matrix)
})
CF_propos=rating_to_propos(CF_predicted_ratings,10)


#SVD++(
us_viewed_root1=apply(us_viewed,FUN=function(x){return(1/sqrt(x))})
SVDppitem=function(u,i){
  r2[u,i]<<-glob_mean+b[u]+b2[i]+q[i]*(p[u]+us_viewed_root1[u]*sum(y*ml_bin_matrix[,i]))
  
}
