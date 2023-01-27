# load needed libraries
library(MASS)
library(dplyr)
library(ClusterR)

generate_data <- function(dim, clust, n_mul, n_base, mu_mul, mu, sigma) {
  df = data.frame()
  j = 1
  for (i in 1:clust) {
    df = rbind(
      df,
      data.frame(
        mvrnorm(n = n_base[i] * n_mul, mu = mu[i,] * mu_mul, Sigma = sigma[j:(j+(dim-1)),]), 
        color=i
      )
    )
    j = j + dim
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
  #colnames(sigma) = c("x", "y")
  
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

random_cov_matrix <- function() {
  return(matrix(c(rnorm(1, 3, 1), rnorm(1, 0, 0.5), 0, rnorm(1, 4, 1)), ncol = dim))
}

generate_default_data_general <- function(dim = 2, num_clusters=3) {
  dim = dim
  clust = num_clusters
  n_mul = 5
  
  sigma = random_cov_matrix()
  for (i in 1:(clust-1)) {
    # generate random 2x2 covariance matrix (works only for 2D data)
    sigma = rbind(
      sigma,
      random_cov_matrix()
    )
  }
  
  mu = matrix(rnorm(clust*dim, 4, 4), ncol=dim, byrow = TRUE)
  n_base = round(rnorm(clust, 10, 5))
  
  print("n (points per centroids):")
  print(n_base * n_mul)
  print('mu (centroid mean point):')
  print(mu)
  print('sigma (covariance matrix):')
  print(sigma)
  
  df = generate_data(
    dim = dim,
    clust = clust,
    n_mul = n_mul,
    n_base = n_base,
    mu_mul = 1.3, #log(base=10, n_mul)/2,
    mu = mu,
    sigma = sigma
  )
  return(df)
}
