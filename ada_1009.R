####### Read, prepare data ###################
d.train<-read.csv("training.csv",header = TRUE,sep=",",stringsAsFactors=F)
d.test<-read.csv("test.csv",header = TRUE,sep=",",stringsAsFactors=F)
str(d.train)

## remove the image gray scale data to another dataset called im.train
im.train<- d.train$Image
d.train$Image <- NULL

## now d.train only contains the keypoints' locations
head(d.train)

## in im.train dataset, convert strings to integers by splitting them and converting 
##the result to integer
## implement parallelization to unlist the im.train dataset

install.packages('foreach')
library(foreach)

im.train <- foreach(im = im.train, .combine=rbind) %do% {
  as.integer(unlist(strsplit(im, " ")))
}
str(im.train)

im.test <- foreach(im = d.test$Image, .combine=rbind) %do% {
  as.integer(unlist(strsplit(im, " ")))
}

d.test$Image <- NULL
save(d.train, im.train, d.test, im.test, file='data.Rd')

## save the data for easy load
load('data.Rd')

############# Examples ######################

im <- matrix(data=rev(im.train[1,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))

points(96-d.train$nose_tip_x[1],         96-d.train$nose_tip_y[1],         col="red")
points(96-d.train$left_eye_center_x[1],  96-d.train$left_eye_center_y[1],  col="blue")
points(96-d.train$right_eye_center_x[1], 96-d.train$right_eye_center_y[1], col="green")
for(i in 1:nrow(d.train)) {
  points(96-d.train$nose_tip_x[i], 96-d.train$nose_tip_y[i], col="red")
}

## test for some "outlier" images
idx <- which.max(d.train$nose_tip_y)
im  <- matrix(data=rev(im.train[6907,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))

points(96-d.train$nose_tip_x[idx], 96-d.train$nose_tip_y[idx], col="red")

which(d.train$nose_tip_y > 96 - 15)


#############Simple Means###################
colMeans(d.train, na.rm=T)
