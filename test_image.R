library(OpenImageR)
library(ClusterR)
library(imager)

# read image
im = readImage('images/elephant.jpg')

# plot original image
imageShow(im)

# first resize the image to reduce the dimensions
im = resizeImage(im, 75, 75, method = 'bilinear') 

use_2D = TRUE

if(use_2D){
  im = grayscale(im)
  dim = 2
} else {
  dim = 3
}

plot(grayscale(im))


# plot resized image
# dev.off() # clear plots
imageShow(im)

# vectorize RGB
im_vec = apply(im, 3, as.vector)

# perform KMeans_rcpp clustering
km_rc = KMeans_rcpp(
    data = im_vec, 
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
new_im = centroids[clusters, ]

# back-convertion to a 3-dimensional image
dim(new_im) = c(nrow(im_res), ncol(im_res), 3)

imageShow(new_im)



