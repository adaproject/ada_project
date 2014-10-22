### Gray level appearance ###
### given the point (x,y), get the gray scale in the image ###
### input point p = (x,y), return gray scale ##
n = 96
gray.scale = function(p, im, n = 96){
  if (n^2 - (p[1] - 1 + (p[2] - 1) * n) <= n^2)
    return(im[n^2 - (p[1] - 1 + (p[2] - 1) * n)])
  else 
    
}

### given ith point in one image, get the line of i-1 th and i+1 th point, compute ith point's locan
### gray apperarance

### function fo fine the last and next keypoints
### input: index i j , output: the i + j th point in the face X
find.point = function(i, k, X){
  pts = ps(X)
  n = dim(pts)[1]
  k = mod(k, n)
  j = i + k
  if (j > n)
    j = mod(j, n)
  return(pts[j,])
}

### function for out put ith point's gray apperarance on a image
### input: index i, face, image, parameter m
### output: ith point's normalized gradient gray appearance
### use ith point, i-1th point i+1th point on the face X
m = 3
local.gray = function(i, X, im, m = 3){
  pts = ps(X)
  p1 = pts[i,] # ith point
  p2 = find.point(i, -1, X) # i-1 th point
  p3 = find.point(i, +1, X) # i+1 th point
  # p1: ith point, p2: i-1 th point, p3: i+1 th point
  
  # take step distance = sqrt(2) to find the nearest point
  # take 2m points
  d = -m:m * sqrt(2)
  ### the line function is (p3 - p2)[1] * (x - p1[1]) + (p3 - p2)[2] * (y - p1[2]) = 0
  if ((p2-p3)[2] != 0) {
    a = (p2 - p3)[1] / (p3 - p2)[2]
    b = p1[2] + (p1[1] * (p3 - p2)[1]) / (p3 - p2)[2]
    # get gray scales in this line
    # generate a 2*m + 1 points' coordinates
    gx = p1[1] + d / sqrt(a^2 + 1)
    gy = gx * a + b
  }
  if ((p2-p3)[2] == 0){
    gx = rep(p1[1], 2*m+1)
    gy = p1[2] + -m:m
  }
  g = matrix(c(gx, gy), ncol = 2, byrow = F)

  # compute the corresponding gray scale and gradient gray scale
  g.scale = c()
  g.scale.g = c()
  for (i in 1:dim(g)[1]){
    g.scale[i] = gray.scale(g[i,], im)
    if (i > 1) g.scale.g[i-1] = g.scale[i] - g.scale[i - 1] 
  }
  
  ### normalize the gradient gray scale
  if (sum(abs(g.scale.g)) != 0) {
    g.scale.g = g.scale.g / sum(abs(g.scale.g))
  }
  
  return(list(gray = g.scale.g, points = g))
  ### g.scale.g is a vector of length 2*m
  ### points is a matrix of 2*m + 1 points
}

### function to compute the whole face.set's average of gradient vector of g.scale and covariance matrix
### this is for all the face.set
### input: face.set, im.train, parameter m
### ouput: list contains: [[1]]: ave. gray appearance for each keypoint
###                      [[2]]: list contains every keypoint's ave gray covariance matrix
gradient = function(d.train1, im.train1, m = 3){
  ### compute every keypoint's local gradient gray appearance among the face.set
  ### and it's covariance matirx
  n = dim(d.train1)[1] ### n faces in total
  k = dim(d.train1)[2]/2 ### k keypoints in total
  
  g = matrix(0, k, 2*m) ## g stores every keypoint's average gray apperearance among all faces
  s = list() # stores each keypoint's gray appeareance covariance matrix
  
  for (j in 1:k){ ## jth keypoint
    gj = rep(0, 2*m)  ## average jth keypoint's neighbor gray scale among all faces
    gij = matrix(0, n, 2*m) ### store each face's jth keypoint's gray scale  
    
    gij = foreach(i = 1:n, .combine = rbind) %do% {
      local.gray(j, d.train1[i,], im.train1[i,])$gray
    }
    gj = apply(gij, 2, mean)
    
    sj = matrix(0, 2*m, 2*m) ### covariance matrix for jth keypoint's gray appearance for all faces
    sj = foreach(i = 1:n, .combine = '+') %do% {
      (gij[i,] - gj) %*% t(gij[i,] - gj)
    }
    sj = sj / n
    
    g[j,] = mean(gj)
    s[[j]] = sj
  }
  ### g is the matrix contains average local gradient gray appearance for every the point among all faces
  ### s is a list of covariance matrix 
  return(list(gray = g, cov = s))
}

### use Mahalanobis distance to compare two point's local gray apperarance ###
### input: a new image's ith keypoint's ave. gradient gray appearance, and ith point's ave.gradient 
### for all faces, and it's cov matirx
m.d = function(new.g.scale.g, g.scale.g, cov.m){
  md = t(new.g.scale.g - g.scale.g) %*% solve(cov.m) %*% (new.g.scale.g - g.scale.g)
  return(md)
}



