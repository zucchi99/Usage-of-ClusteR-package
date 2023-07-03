source("functions.R")

dim = 2
num_clusters = 3

df = generate_data_random(dim, num_clusters)


# create bivariate normal distribution
color_set = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
colors = sample(color_set, num_clusters)
colors
labels = seq(1, num_clusters, 1)

# define shapes to be used in chart:
# use_shapes=F ==> always circles
# use_shapes=T ==> each cluster its shape
use_shapes = TRUE
if (use_shapes) {
  shapes = df$color
} else {
  shapes = 1
}
shap_labels = labels

plot(x=df$X1, y=df$X2, col=colors[df$color], pch=shapes)
legend("topleft", as.character(seq(1, num_clusters, 1)), cex=.8, col=colors, pch=shap_labels, title="real", text.font=4)

real_clusters = df %>%
  group_by(df$color) %>% 
  summarise_at(vars("X1", "X2"), mean) %>%
  as.data.frame() %>%
  subset(select=c(X1, X2))

#data_cols = subset(df, select=c(X1, X2))
#plot_2d(data = data_cols, clusters = df$color, centroids_medoids = real_clusters)

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
legend("topleft", as.character(seq(1, num_clusters, 1)), cex=.8, col=colors, pch=shap_labels, title="preds", text.font=4)

