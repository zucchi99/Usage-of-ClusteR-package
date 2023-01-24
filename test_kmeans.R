# load needed libraries
library(MASS)
library(dplyr)
library(ClusterR)

generate_data <- function(dim, clust, n_mul, n_base, mu_mul, mu, sigma) {
  df = data.frame()
  j = 1
  
  for (i in 1:clust) {
    
    print(n_base[i] * n_mul)
    print(mu[1,] * mu_mul)
    print(sigma[j:(j+1),])
    print('')
    
    df = rbind(
      df,
      data.frame(
        mvrnorm(n = n_base[i] * n_mul, mu = mu[1,] * mu_mul, Sigma = sigma[j:(j+1),]), 
        color=i
      )
    )
    j = j + 2
  }
  return(df)
}

generate_default_data <- function() {
  dim = 2
  clust = 3
  n_mul = 100
  
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
    mu_mul = log(base=10, n_mul)/2,
    mu = matrix(c(5,5,0,0,3,1), ncol=dim, byrow = TRUE),
    sigma = sigma
  )
  return(df)
}

df = generate_default_data()

# create bivariate normal distribution
colors = c('blue', 'darkgreen', 'orange')
labels = c(1,2,3)

use_shapes = FALSE
if (use_shapes) {
  shapes = df$color  
  shap_labels = labels
} else {
  shapes = 1
  shap_labels = c(1,1,1)
}

plot(x=df$X1, y=df$X2, col=colors[df$color], pch=shapes)

legend("topleft", c("1","2","3"), cex=.8, col=colors, pch=shap_labels)

# perform KMeans_rcpp clustering
km_rc = KMeans_rcpp(
    data = df$color, 
    clusters = 5, 
    num_init = 5, 
    max_iters = 100, 
    initializer = 'optimal_init', 
    verbose = F
)

km_rc$between.SS_DIV_total.SS

# between.SS_DIV_total.SS = (total_SSE - sum(WCSS_per_cluster)) / total_SSE

pr = predict(km_rc, newdata = im_vec)

centroids = km_rc$centroids
clusters = km_rc$clusters
table(clusters)

# each observation is associated with the nearby centroid
new_data = centroids[clusters, ]

# back-convertion to a 3-dimensional image
dim(new_data) = c(nrow(im_res), ncol(im_res), 3)

imageShow(new_im)



