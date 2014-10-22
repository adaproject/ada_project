### Searching ###

### could have some assumptions
### e.g. set the initial nose point of the new image as the average nose point of all training image.


### function to search the near gray scale
### input: a point index j, a face (structure) X, parameter l, m, (new) image
### output: the new jth point (xnew, ynew)

new.position = function(j, X, m = 3, l = 5, im = new.image, gradient = g){
  local = local.gray(j, X, im, l)
  gray.l = local$gray
  ### then can get 2*(l-m)+1 local gradient gray scale
  md = c()
  for (i in -(l-m):(l-m)){
    gray.m = gray.l[(l+1+i-m):(l+1+i+m-1)]
    ### compute the Mahalanobis distance to this point's local gray appearance
    k = i + (l-m) + 1
    md[k] = m.d(gray.m, gradient$gray[j,], gradient$cov[[j]]) 
  }
  return(local$points[which.min(md),])
}

### updata each keypoint's new point to get the new shape
new.shape = function(X, m = 3, l = 5, im = new.image, gradient = g){
  pts = ps(X)
  d2 = dim(X)[2]/2
  for (j in 1:d2){
    pts[j,] = new.position(j, X, m, l, im, gradient)
  }
  return(vec(pts))
}

### compute the shift vector
dxy = function(newx, oldx){
  return(vec(ps(newx) - ps(oldx)))
}

### given transform ax, ay, tx, ty, compute it inverse transform's index
inv.shift = function(index){
  ax= index[1]; ay = index[2]; tx = index[3]; ty = index[4]
  s = sqrt(ax^2 + ay^2)
  theta = atan(ax/ay)
  s = 1/s
  theta = -theta
  return(c(s*cos(theta), s*sin(theta), -tx, -ty))
}

### given the index and d.index, update the index
update.index = function(index, d.index){
  ax= index[1]; ay = index[2]; tx = index[3]; ty = index[4]
  dax= d.index[1]; day = d.index[2]; dtx = d.index[3]; dty = d.index[4]
  
  s = sqrt(ax^2 + ay^2)
  theta = atan(ax/ay)
  ds = sqrt(dax^2 + day^2)
  dtheta = atan(dax/day)
  s.new = s * (ds)
  theta.new = theta + dtheta
  if (cos(theta.new) < 0) theta.new = theta.new + pi
  return(c(s.new*cos(theta.new), s.new*sin(theta.new), tx+dtx, ty+dty))
}

## function to update
## input x, X, P, b, 
update = function(X, index, b, xbar = mean.face, P = Principal.vector, lambda = Principal.value){
  x = xbar + t(P %*% b)
  ## 1. compute s.X
  s.X = new.shape(X)
  
  ## 2. align X to s.X, compute the index (ds, dtheta, dt)
  d.index = alignment(X, s.X)
#  X.new = aligned.face(d.index, X) # aligned face
  
  ## 3. compute new.index
  new.index = update.index(index, d.index)
  
  ## 4. compute dx
  
  ### suppose to have:
#  aligned.face(new.index, (x+dx)) = s.X 
  
  ### use inv.shift to get the new.index's inverse index
  inv.index = inv.shift(new.index)
  ## suppse to have
  dx = inv.aligned.face(new.index, s.X) - x
  db = t(P) %*% t(dx)
  
  ## update parameters
  new.b = b + db
  for (i in 1: length(new.b)){
    if (new.b[i]^2 > 9*lambda[i])
      new.b[i] = sign(new.b[i]) * 3 * sqrt(lambda[i])
  }
  
  ## updata new.X
  new.X = aligned.face(index, (xbar + t(P %*% new.b)))
  
  ## updata parameters
  b = new.b
  index = new.index
  X = new.X
  
  ## should return new.X, new.index, new.b
  return(list(X = new.X, index = new.index, b = new.b))
}

### compute the error of new.X and X
dist = function(X, new.X){
  return(mean(sqrt((dxy(X, new.X)^2))))
}


### iteration:
### compute the initial suggested face shape, use the average face of training set
s.X0 = t(data.frame(apply(d.train1, 2, mean)))
Principal.vector = P
Principal.value = eigen.values[1:dim(Principal.vector)[2]]
b0 = rep(0, dim(Principal.vector)[2]) ## first k principal components
### initial shape model x0 = xbar(e.g., mean.face) + P*b0
x0 = mean.face + t(P %*% b0)
index0 = alignment(x0, s.X0) ## get index: ax0, ay0, tx0, ty0
X0 = aligned.face(index0, x0) ## get aligned X0

### compute the model 
X = X0
index = index0
b = b0

d = 100
while(d > 0.1){
  updated = update(X, index, b)
  d = dist(X, updated$X)
  d
  if (d < 0.1) 
    break
  X= updated$X
  index = updated$index
  b = updated$b
}

ps(X)

## test 

g = gradient(d.train1, im.train1)
new.image = im.test[2,]
im <- matrix(data=rev(im.test[2,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))
for (i in 1:15){
  points(96-ps(X)[i,1],         96-ps(X)[i,2],         col="green")  
}


