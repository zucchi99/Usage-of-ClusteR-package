# load needed libraries
library(MASS)
library(dplyr)
library(ClusterR)

dim = 2
num_clusters = 3

generate_data <- function(dim, clust, n_mul, n_base, mu_mul, mu, sigma) {
  df = data.frame()
  j = 1
  for (i in 1:clust) {
    df = rbind(
      df,
      data.frame(
        mvrnorm(n = n_base[i] * n_mul, mu = mu[i,] * mu_mul, Sigma = sigma[j:(j+1),]), 
        color=i
      )
    )
    j = j + 2
  }
  return(df)
}

generate_default_data <- function(dim = 2, num_clusters=3) {
  dim = dim
  clust = num_clusters
  n_mul = 5
  
  sigma = rbind(
    matrix(c(1, -.6, 0, 1), ncol = dim),
    matrix(c(.8, 0,  0, 4), ncol = dim),
    matrix(c(3, .7,  0, 2), ncol = dim)
  )
  colnames(sigma) = c("x", "y")
  
  df = generate_data(
    dim = dim,
    clust = clust,
    n_mul = n_mul,
    n_base = c(10,10,15),
    mu_mul = 1.3, #log(base=10, n_mul)/2,
    mu = matrix(c(5,5,0,0,3,1), ncol=dim, byrow = TRUE),
    sigma = sigma
  )
  return(df)
}

df = generate_default_data(dim, num_clusters)

# create bivariate normal distribution
colors = c('blue', 'darkgreen', 'orange')
labels = seq(1, num_clusters, 1)

# define shapes to be used in chart:
# use_shapes=F ==> always circles
# use_shapes=T ==> each cluster its shape
use_shapes = TRUE
if (use_shapes) {
  shapes = df$color  
  shap_labels = labels
} else {
  shapes = 1
  shap_labels = seq(1, num_clusters, 1)
}

plot(x=df$X1, y=df$X2, col=colors[df$color], pch=shapes)
legend("topleft", as.character(seq(1, num_clusters, 1)), cex=.8, col=colors, pch=shap_labels)

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

centroids = km_rc$centroids
clusters = km_rc$clusters
table(clusters)

# each observation is associated with the nearby centroid
new_data = centroids[clusters, ]

# plot predictions
plot(x=df$X1, y=df$X2, col=colors[df$KMeans_rcpp], pch=shapes)
legend("topleft", as.character(seq(1, num_clusters, 1)), cex=.8, col=colors, pch=shap_labels)
