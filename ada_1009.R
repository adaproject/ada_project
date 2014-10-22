#######Read, prepare data###################
d.train<-read.csv("training.csv",header = TRUE,sep=",",stringsAsFactors=F)
d.test<-read.csv("test.csv",header = TRUE,sep=",",stringsAsFactors=F)
str(d.train)
im.train<- d.train$Image
d.train$Image <- NULL
head(d.train)
#as.integer(unlist(strsplit(im.train[1], " ")))
#install.packages('doMC')
#library(doMC)
#registerDoMC()
im.train <- foreach(im = im.train, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}
str(im.train)
im.test <- foreach(im = d.test$Image, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}
d.test$Image <- NULL
save(d.train, im.train, d.test, im.test, file='data.Rd')
load('data.Rd')


#######average locations############
colMeans(d.train, na.rm=T)


#############Get average eye from the training group###################
coord <- "left_eye_center"
patch_size <- 10
coord_x <- paste(coord, "x", sep="_")
coord_y <- paste(coord, "y", sep="_")
patches <- foreach (i = 1:nrow(d.train), .combine=rbind) %do% {
  im  <- matrix(data = im.train[i,], nrow=96, ncol=96)
  x   <- d.train[i, coord_x]
  y   <- d.train[i, coord_y]
  x1  <- (x-patch_size)
  x2  <- (x+patch_size)
  y1  <- (y-patch_size)
  y2  <- (y+patch_size)
  if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96) )
  {
    as.vector(im[x1:x2, y1:y2])
  }
  else
  {
    NULL
  }
}
mean.patch <- matrix(data = colMeans(patches), nrow=2*patch_size+1, ncol=2*patch_size+1)
image(1:21, 1:21, mean.patch[21:1,21:1], col=gray((0:255)/255))

#######Search the average eye in the testing group and get the best coordinate##########
search_size <- 2
mean_x <- mean(d.train[, coord_x], na.rm=T)
mean_y <- mean(d.train[, coord_y], na.rm=T)
x1     <- as.integer(mean_x)-search_size
x2     <- as.integer(mean_x)+search_size
y1     <- as.integer(mean_y)-search_size
y2     <- as.integer(mean_y)+search_size
params <- expand.grid(x = x1:x2, y = y1:y2)
im <- matrix(data = im.test[1,], nrow=96, ncol=96)

r  <- foreach(j = 1:nrow(params), .combine=rbind) %dopar% {
  x     <- params$x[j]
  y     <- params$y[j]
  p     <- im[(x-patch_size):(x+patch_size), (y-patch_size):(y+patch_size)]
  score <- cor(as.vector(p), as.vector(mean.patch))
  score <- ifelse(is.na(score), 0, score)
  data.frame(x, y, score)
}
best <- r[which.max(r$score), c("x", "y")]
best  #best coordinate