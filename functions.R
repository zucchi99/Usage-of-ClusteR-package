# load needed libraries
library(MASS)
library(dplyr)
library(ClusterR)

# assume df to have as input: 
# - first column: true labels
# - second to last column: predicted labels
compare_metrics <- function(df, metrics, metrics_abb) {
  n_preds = ncol(df)
  n_metrics = length(metrics)
  comparisons_df = data.frame()
  for (i in 1:n_metrics) {
    row = c()
    for (j in 2:n_preds) {
      row = append(row, external_validation(df[,1], df[,j], method=metrics[i]))
    }
    comparisons_df = rbind(comparisons_df, row)
  }
  names(comparisons_df) = names(df)[-1]
  row.names(comparisons_df) <- metrics_abb
  return(t(comparisons_df))
}

reorder_data <- function(obj, centroids, items, k=3, i=1, drop_order=T) {
  obj[[centroids]] = sort_centroids_by_ith_coordinate(obj[[centroids]], k=k, i=i)
  if (length(items) > 0) {
    # 1:0 = c(1,0) e non c(0) ==> if è necessario
    for (i in 1:length(items)) {
      attr_name = items[[i]]
      item = matrix(obj[[attr_name]], nrow=k)
      obj[[attr_name]] = sort_rows_according_to_centroids(item, obj[[centroids]][,k])
    }
  }
  if (drop_order) {
    # drop the column with the current order
    obj[[centroids]] = obj[[centroids]][,-ncol(obj[[centroids]])]    
  }
  return(obj)
}

sort_centroids_by_ith_coordinate <- function(centroids, k=3, i=1) {
  # add column with original order (needed for other attributes)
  centroids = cbind(centroids, 1:k)
  # sort centroids (rows) by i-th axis (by default first coordinate)
  centroids = centroids[order(centroids[,i]), ] 
  return(centroids)
}

new_cluster <- function(cluster, ordering) { 
  return(ordering[cluster]) 
}

change_centroids <- function(obj, ordering, items) {
  if (length(items) > 0) {
    # 1:0 = c(1,0) e non c(0) ==> if è necessario
    for (i in 1:length(items)) { 
      attr_name = items[[i]]
      obj[[attr_name]] = unlist(lapply(obj[[attr_name]], new_cluster, ordering))
    }
  }
  return(obj)
}

sort_rows_according_to_centroids <- function(matrix, ordering) {
  cols = ncol(matrix) + 1
  matrix = cbind(matrix, c(ordering)) # add right ordering column
  matrix = matrix[order(matrix[,cols]), ] # sort data according to right ordering
  matrix = matrix[,-cols]
  return(matrix)
}

plot_points <- function(coordinates, num_points=3, cex=2, col="black") {
  for (i in seq(1, num_points)) {
    points(coordinates[[i]][1], coordinates[[i]][2], pch=toString(i), cex=cex, col=col)
  }
}

calculate_real_centroids <- function(df) {
  x = df %>%
    group_by(df$color) %>% 
    summarise_at(vars("X1", "X2"), mean) %>%
    as.data.frame() %>%
    subset(select = c("X1", "X2"))
  return (x)
}

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

generate_data_default <- function(dim = 2, num_clusters=3) {
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
  return(matrix(c(
    rnorm(1, 3, 1), 
    rnorm(1, 0, 0.5), 
    0, 
    rnorm(1, 4, 1)
  ), ncol = dim))
}

generate_data_random <- function(dim = 2, num_clusters=3) {
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
