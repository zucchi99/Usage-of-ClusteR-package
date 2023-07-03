source("functions.R")

dim = 2
num_clusters = 3

df = generate_data_random(dim, num_clusters)
real_centroids = calculate_real_centroids(df)
real_centroids

data_cols = subset(df, select=c(X1, X2))
plot_2d(data = data_cols, clusters = df$color, centroids_medoids = real_centroids)

# perform KMeans_rcpp clustering
km_rc = KMeans_rcpp(
  data = df, 
  clusters = num_clusters, 
  num_init = 5, 
  max_iters = 100, 
  initializer = 'optimal_init', 
  verbose = F
)

# between.SS_DIV_total.SS = (total_SSE - sum(WCSS_per_cluster)) / total_SSE
km_rc$between.SS_DIV_total.SS

# predict centroids, covariance matrix and weights
df['KMeans_rcpp'] = predict(km_rc, newdata = df)

pred_centroids = km_rc$centroids %>% as.data.frame() %>% subset(select=c(1,2))
colnames(pred_centroids) = c("X1", "X2")
pred_centroids

pred_clusters = km_rc$clusters
table(pred_clusters)

# each observation is associated with the nearby centroid
new_data = centroids[clusters, ]

plot_2d(data = data_cols, clusters = pred_clusters, centroids_medoids = pred_centroids)

