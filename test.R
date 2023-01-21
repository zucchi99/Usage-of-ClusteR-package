library(OpenImageR)

im = readImage('images/elephant.jpg')

# first resize the image to reduce the dimensions
im = resizeImage(im, 200, 200, method = 'bilinear')            

imageShow(im)

