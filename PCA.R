### PCA ###

### already have the aligned face.set###
### Use pca for demension reduction ###

### compute the mean.face ###
mean.face = average.shape(face.set)

### compute the corvariance matrix s
s = cov(face.set)
#??????????????????????????????????????????????
s = matrix(0,dim(face.set)[2],dim(face.set)[2])
for (i in 1:dim(face.set)[2]){
  s = s + as.matrix(t(face.set[i,] - mean.face)) %*% as.matrix(face.set[i,] - mean.face)
}
s = s / dim(face.set)[2]

### compute the eigen values and eigen vectors
eigen.values = eigen(s)$values
eigen.vectors = eigen(s)$vectors

### take 95% of eigen values
k = 1
while (sum(eigen.values[1:k]) / sum(eigen.values) < 0.95)
  k = k + 1

### compute P and b ###
### b: weights. initial value 0
### P: matrix of basis vectors
P = eigen.vectors[,1:k]
b0 = rep(0, k)
Principal.vector = P
Principal.value = eigen.values[1:dim(Principal.vector)[2]]
