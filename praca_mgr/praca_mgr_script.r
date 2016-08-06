# wczytanie
source("basic.r")
source("CF_functions.R")
read_ml_file("ml-100k/u.data")
# non-personalized
mean(ml_means)

remove_viewed=function(x,y){
  return ((x==0)*y)
}

non_personalized=function(u,n){
  return(head(order(remove_viewed(ml_matrix[u,],ml_means),decreasing=TRUE),n))
}

non_personalized_recs=function(n){
 lapply(1:users,function(u) non_personalized(u,n)) 
}
non_per_propos=non_personalized_recs(10)
# colaborative-filtering
system.time({
  cov_matrix=make_sim_matrix(cov_similarity)
  cos_matrix=make_sim_matrix(cos_similarity)
})
system.time({
  CF_predicted_ratings=CF_predict_all(cov_matrix)
})
predicted_ratings=CF_predicted_ratings

rating_to_propos1=function(u,n){
  return(head(order(remove_viewed(ml_matrix[u,],predicted_ratings[u,]),decreasing=TRUE),n))
}
rating_to_propos=function(n){
  lapply(1:users,function(u) rating_to_propos1(u,n))
}
CF_propos=rating_to_propos(10)
