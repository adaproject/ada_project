### Active Shaped Model
### choose training set

### dealing with the missing value and outliers ##
d.train1 = na.omit(d.train)
summary(d.train1)
summary(d.train)
dim(na.omit(d.train))

### First use d.train1(no missing point) as the training set ###
### Some exploratory data analysis ### 

## remove the image gray scale data to another dataset called im.train
im.train1<- d.train1$Image
d.train1$Image <- NULL

## now d.train only contains the keypoints' locations
head(d.train1)

## in im.train dataset, convert strings to integers by splitting them and converting 
##the result to integer
## implement parallelization to unlist the im.train dataset

install.packages('foreach')
library(foreach)

im.train1 <- foreach(im = im.train1, .combine=rbind) %do% {
  as.integer(unlist(strsplit(im, " ")))
}
str(im.train1)

im.test <- foreach(im = d.test$Image, .combine=rbind) %do% {
  as.integer(unlist(strsplit(im, " ")))
}

d.test$Image <- NULL
save(d.train1, im.train1, d.test, im.test, file='data1.Rd')

## save the data for easy load
load('data1.Rd')


############# Examples ######################

im <- matrix(data=rev(im.train1[1,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))

points(96-d.train1$nose_tip_x[1],         96-d.train1$nose_tip_y[1],         col="red")
points(96-d.train1$left_eye_center_x[1],  96-d.train1$left_eye_center_y[1],  col="blue")
points(96-d.train1$right_eye_center_x[1], 96-d.train1$right_eye_center_y[1], col="green")
for(i in 1:nrow(d.train1)) {
  points(96-d.train1$nose_tip_x[i], 96-d.train1$nose_tip_y[i], col="red")
}

### We can see that the nose points are nearly concentrated on the center. ###
names(d.train1)
summary(d.train1)
### d.train1 is the ideal shaped data frame

