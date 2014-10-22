### ASM Algorithm ###

### Alignment ###

###  Inpute two faces X1 X2, align X2 to X1, calculate fore transfer parameters: theta, s, xi, yi###
### and return the transform parameters
### e.g. Align 2 faces

alignment = function(X2, X1, W = weight.matrix){
  n = dim(X1)[2] / 2
  pts1 = ps(X1)
  pts2 = ps(X2)
  # W = weight.matrix(face.set)
  x1 = diag(W) %*% pts1[,1]
  y1 = diag(W) %*% pts1[,2]
  x2 = diag(W) %*% pts2[,1]
  y2 = diag(W) %*% pts2[,2]
  z = diag(W) %*% (pts2[,1]^2 + pts2[,2]^2)
  w = sum(diag(W))
  c1 = diag(W) %*% (pts1[,1] * pts2[,1] + pts1[,2] * pts2[,2])
  c2 = diag(W) %*% (pts1[,2] * pts2[,1] - pts1[,1] * pts2[,2])
  m = matrix(c(x2, -y2, w, 0, y2, x2, 0, w, z, 0, x2, y2, 0, z, -y2, x2), nrow = 4, byrow = T)
  t = c(x1, y1, c1, c2)
  result = solve(m, t)
  names(result) = c("ax","ay","tx","ty")
  return(result)
}

## given the parameters and face X, compute the aligned face
aligned.face = function(index, X){
  pts = ps(X)
  ax = index[1]
  ay = index[2]
  tx = index[3]
  ty = index[4]
  for (i in 1:(dim(X)[2]/2)){
    pts[i,] = matrix(c(ax, ay, -ay, ax), nrow = 2, byrow = F) %*% pts[i,] + c(tx, ty)
  }
  ## get the aligned pts, then reshape it to X2
  return(vec(pts))
}
## inverse
inv.aligned.face = function(index, s.X){
  pts = ps(s.X)
  ax = index[1]
  ay = index[2]
  tx = index[3]
  ty = index[4]
  pts = pts - c(tx, ty)
  for (i in 1:(dim(X)[2]/2)){
    pts[i,] = solve(matrix(c(ax, ay, -ay, ax), nrow = 2, byrow = F)) %*% pts[i,]
  }
  ## get the aligned pts, then reshape it to X2
  return(vec(pts))
}

### align all the faces to one face X
### input a set of face, and the face to align with. e.g. align all face in face.set to X
alignment.all = function(face.set, X){
  face.set = foreach(i = 1:dim(face.set)[1], .combine = rbind) %do% {
    aligned.face(alignment(face.set[i,], X), face.set[i,])
  }
  return(data.frame(face.set))
}

### function to transfer a keypoints vector to a keypoints matrix
ps = function(X){
  ## create a matrix pts, to record all points in a n*2 matrix, every row is the point (X[i],Y[i])
  xaxis = c()
  yaxis = c()
  for (i in 1:(dim(X)[2]/2)){
    xaxis[i] =  X[2*i -1]
    yaxis[i] =  X[2*i]
  }
  pts = matrix(as.numeric(c(xaxis, yaxis)), ncol = 2, byrow = F)
  return(pts)
}

## function to reshape the keypoints matrix to the keypoints vector
name = names(d.train1)
vec = function(pts){
  vector = c()
  for (i in 1:dim(pts)[1]){
    vector[2*i - 1] = pts[i,1]
    vector[2*i] = pts[i,2]
  }
  return(t(data.frame(vector, row.names = name)))
}

### funciton to calculate weithts matrix W ###

w.matrix = function(face.set){
  ## compute the diagnol weight matrix W
  n = dim(face.set)[1]
  m = dim(face.set)[2]/2
  ### compute kth and lth point's distance for each image
  w = c()
  for (k in 1:m){ ### for kth keypoint, create a matrix to store all the distances needed
    vk = c() 
    d = matrix(0, n, m)
    for (i in 1:n){ ### in ith face
      X = ps(face.set[i,])
      dk = c() ### record the distance between kth(fixed) point and lth point in ith face
      for (l in 1:m){
        ### compute kth keypoint and lth keypoint's distance in ith face
        dk[l] = sqrt((X[k,1] - X[l,1])^2 + (X[k,2] - X[l,2])^2)
      }
      d[i,] = dk
    }
    for (l in 1:m){
      vk[l] = var(d[,l])
    }
    w[k] = 1/sum(vk)
  }
  return(diag(w))
}


