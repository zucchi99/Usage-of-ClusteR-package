
source("functions.R")
library(plotly)

# main

df_times = data.frame()
dimensions = c(100, 1000, 2000, 4000, 6000, 8000, 100000, 20000, 50000, 75000, 100000, 200000, 300000, 400000, 500000, 800000, 1000000, 2000000)

for (r in dimensions){
  
  m = random_matrix(r, 10)
  
  if (r <= 5000){
    df_times = measure.time(df_times, "Cluster_Medoids", rows=r, f=function() Cluster_Medoids(m, 10, distance_metric="euclidean") )
  }
  
  if (r <= 50000){
    df_times = measure.time(df_times, "Clara_Medoids", rows=r, f=function() Clara_Medoids(m, 10, samples=2, sample_size=0.05, "euclidean") )
  }
  
  if (r <= 500000){
    df_times = measure.time(df_times, "KMeans_rcpp", rows=r, f=function() KMeans_rcpp(m, 10) )
  }
  
  if (r <= 1000000){
    df_times = measure.time(df_times, "GMM", rows=r, f=function() GMM(m, 10, dist_mode = "maha_dist", seed_mode = "random_subset") )
  }
  
  df_times = measure.time(df_times, "MiniBatchKmeans", rows=r, f=function() MiniBatchKmeans(m, 10) )
  df_times = measure.time(df_times, "KMeans_arma", rows=r, f=function() KMeans_arma(m, 10) )
  
}

colnames(df_times) = c("method", "rows", "features", "n_clusters", "time")

method = unique(df_times$method)
color  = seq(length(methods))
colors = as.data.frame(cbind(method, color))

color = as.integer(unlist(map(df_times$method, function(x) { colors[colors$method == x,2] } )))
df_times = cbind(df_times, color)

df_times

write.csv(df_times, "clustering_exec_times.csv", row.names=F)


df_times$rows = as.integer(df_times$rows)
df_times$features = as.integer(df_times$features)
df_times$n_clusters = as.integer(df_times$n_clusters)
df_times$time = as.double(df_times$time)

#interactive plot
fig <- plot_ly(data = df_times, x = ~rows, y = ~time, color = ~method)

fig