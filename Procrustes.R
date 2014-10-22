### Procrustes analysis ###

### funtion to make a face align to the origin ###
align.o = function(X){
  pts = ps(X)
  pts[,1] = pts[,1] - mean(pts[,1])
  pts[,2] = pts[,2] - mean(pts[,2])
  for (i in 1:(dim(X)[2]/2)){
    X[2*i - 1] = pts[i,1]
    X[2*i] = pts[i,2]
  }
  return(X)
}

### calculate the average face ###
### input a set of face X1,X2,X3.... output their average shape Xbar
average.shape = function(face.set){
  mean.face = c()
  for (i in 1:dim(face.set)[2]){
    mean.face[i] = mean(face.set[,i])
  }
  return(t(data.frame(mean.face, row.names = names(face.set))))
}

### normalize a face ##
normalize = function(X){
  pts = ps(X)
  pts[,1] = pts[,1] / sqrt(sum(pts[,1]^2))
  pts[,2] = pts[,2] / sqrt(sum(pts[,2]^2))
  return(vec(pts))
}

### function to compute the error between a face.set and a mean.face
### input mean.face and a face.set, output the error
dist = function(X, new.X){
  return(mean(sqrt((dxy(X, new.X)^2))))
}

err = function(face.set, mean.face){
  d = foreach(i = 1:dim(face.set)[1], .combine = '+') %do% {
    sum(dist(face.set[i,], mean.face))
  }
  return(d)
}


### step by step to get converged mean.face ###

d.train1 = 96 - d.train1
face.set = d.train1[1:500,]

### align all faces to the origin 
### time consuming steps
face.set <- foreach(i = 1:dim(face.set)[1], .combine=rbind) %do% {
  align.o(face.set[i,])
}

weight.matrix = w.matrix(face.set)

## set mean face as face1, and align all faces to face1
mean.face = face.set[1,]

face.set = alignment.all(face.set, mean.face)
mean.face = average.shape(face.set)

mean.face = normalize(mean.face)
face.set = alignment.all(face.set, mean.face)
## compute average shape
mean.face = average.shape(face.set)
# mean.face = normalize(mean.face)

## compute the distance 
d = err(face.set, mean.face)
d

## iteration
while (d > 7.7) {
  ### compute all faces' mean.face
  ### normalize the mean.face 
  mean.face = normalize(mean.face)
  ### align all the faces to the normalized mean.face
  face.set = alignment.all(face.set, mean.face)
  mean.face = average.shape(face.set)
  ### compute error(check convergency)
  d = err(face.set, mean.face)
  if (d < 7.7)
    break
}

## converges very slow !!!!!!!
d

  
  