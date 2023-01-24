# load library MASS
library(MASS)
library(dplyr)
library(ClusterR)

load_factor = 100
ncol=2

use_shapes = FALSE
coeff = log(base=10, load_factor)/2    #choose if augmenting distance when generating more points
#coeff = 1

# create bivariate normal distribution
colors = c('blue', 'darkgreen', 'orange')
labels = c(1,2,3)

df1 = bind_rows(list(
  data.frame(mvrnorm(n = 10 * load_factor, mu = c(5*coeff, 5*coeff), Sigma = matrix(c(1, -.6, 0, 1), ncol = 2)), color=labels[1]),
  data.frame(mvrnorm(n = 10 * load_factor, mu = c(0*coeff, 0*coeff), Sigma = matrix(c(.8, 0, 0, 4),  ncol = 2)), color=labels[2]),
  data.frame(mvrnorm(n = 15 * load_factor, mu = c(3*coeff, 1*coeff), Sigma = matrix(c(3, .7, 0, 2),  ncol = 2)), color=labels[3])
))

if (use_shapes) {
  shapes = df1$color  
  shap_labels = labels
} else {
  shapes = 1
  shap_labels = c(1,1,1)
}


plot(x=df1$X1, y=df1$X2, col=colors[df1$color], pch=shapes)

legend("topleft", c("1","2","3"), cex=.8, col=colors, pch=shap_labels) #, title="sample types", text.font=4)
