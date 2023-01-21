library(OpenImageR)

# read image
im = readImage('images/elephant.jpg')

# plot original image
imageShow(im)

# first resize the image to reduce the dimensions
im_res = resizeImage(im, 75, 75, method = 'bilinear')            

# plot resized image
# dev.off() # clear plots
imageShow(im_res)

# vectorize RGB
im_vec = apply(im_res, 3, as.vector)

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

pr = predict(km_rc, newdata = im2)

get_cent = km_rc$centroids
get_clust = km_rc$clusters

# each observation is associated with the nearby centroid
new_im = get_cent[get_clust, ]

# back-convertion to a 3-dimensional image
dim(new_im) = c(nrow(im), ncol(im), 3)

imageShow(new_im)

