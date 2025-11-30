### Optimized ASM Implementation ###
### This script contains optimizations and improvements to the original ASM

source("ASM.R")
source("Gray.R")

### Optimization 1: Add regularization to covariance matrices to prevent singularity ###
### This is crucial for numerical stability in Mahalanobis distance calculation

# Improved Mahalanobis distance with regularization
m.d.robust = function(new.g.scale.g, g.scale.g, cov.m, reg = 1e-6){
  # Add regularization to prevent singular matrices
  cov.m.reg = cov.m + diag(reg, nrow(cov.m))

  # Use tryCatch to handle potential errors
  md = tryCatch({
    t(new.g.scale.g - g.scale.g) %*% solve(cov.m.reg) %*% (new.g.scale.g - g.scale.g)
  }, error = function(e) {
    # Fallback to Euclidean distance if still singular
    sum((new.g.scale.g - g.scale.g)^2)
  })

  return(md)
}


### Optimization 2: Adaptive search with larger range ###
### Increase search parameters for better feature detection

new.position.optimized = function(j, X, m = 5, l = 9, im = new.image, gradient = g){
  # Increased from m=3, l=5 to m=5, l=9
  # This gives search range of ±4 pixels instead of ±2

  local = local.gray(j, X, im, l)
  gray.l = local$gray

  ### Get 2*(l-m)+1 local gradient gray scale
  md = c()
  for (i in -(l-m):(l-m)){
    gray.m = gray.l[(l+1+i-m):(l+1+i+m-1)]
    k = i + (l-m) + 1
    # Use robust Mahalanobis distance
    md[k] = m.d.robust(gray.m, gradient$gray[j,], gradient$cov[[j]])
  }

  return(local$points[which.min(md),])
}


### Optimization 3: Multi-scale search ###
### Search at different scales for better convergence

new.shape.multiscale = function(X, im = new.image, gradient = g, scale = 1){
  # scale = 1: normal, scale = 2: coarse search, scale = 0.5: fine search
  pts = ps(X)
  d2 = dim(X)[2]/2

  if (scale > 1) {
    # Coarse search: larger range
    m = 5
    l = 11
  } else if (scale < 1) {
    # Fine search: smaller range
    m = 3
    l = 5
  } else {
    # Normal search
    m = 5
    l = 9
  }

  for (j in 1:d2){
    pts[j,] = new.position.optimized(j, X, m, l, im, gradient)
  }

  return(vec(pts))
}


### Optimization 4: Better initialization using image statistics ###

initialize.shape = function(im, mean.face, d.train1) {
  # Simple initialization based on image center
  # More sophisticated methods could use face detection

  im.matrix = matrix(data=rev(im), nrow=96, ncol=96)

  # Find approximate face center by looking for darker regions (eyes, nose)
  # Calculate row and column means
  row.means = apply(im.matrix, 1, mean)
  col.means = apply(im.matrix, 2, mean)

  # Estimate center (darker regions)
  center.y = which.min(row.means)
  center.x = which.min(col.means)

  # Calculate offset from default center
  default.center = c(48, 48)  # Center of 96x96 image
  offset = c(96 - center.x, 96 - center.y) - default.center

  # Adjust mean face by offset
  pts = ps(mean.face)
  pts[,1] = pts[,1] + offset[1]
  pts[,2] = pts[,2] + offset[2]

  return(vec(pts))
}


### Optimization 5: Improved update function with adaptive step size ###

update.optimized = function(X, index, b, xbar = mean.face, im = new.image,
                           P = Principal.vector, lambda = Principal.value,
                           step_size = 1.0){
  x = xbar + t(P %*% b)

  ## 1. Multi-scale search: start with coarse, then refine
  s.X = new.shape.multiscale(X, im = im, gradient = g, scale = 1)

  ## 2. Align X to s.X
  d.index = alignment(X, s.X)

  ## 3. Compute new index
  new.index = update.index(index, d.index)

  ## 4. Compute dx with adaptive step size
  inv.index = inv.shift(new.index)
  dx = inv.aligned.face(new.index, s.X) - x
  db = t(P) %*% t(dx) * step_size  # Apply step size

  ## Update parameters with constraints
  new.b = b + db
  for (i in 1:length(new.b)){
    if (new.b[i]^2 > 9*lambda[i])
      new.b[i] = sign(new.b[i]) * 3 * sqrt(lambda[i])
  }

  ## Update new.X
  new.X = aligned.face(index, (xbar + t(P %*% new.b)))

  return(list(X = new.X, index = new.index, b = new.b))
}


### Optimization 6: Improved convergence loop with monitoring ###

fit.asm.optimized = function(new.image, mean.face, P, lambda, d.train1, im.train1,
                            max_iterations = 50, tolerance = 0.1, verbose = TRUE) {

  # Better initialization
  s.X0 = initialize.shape(new.image, mean.face, d.train1)

  b0 = rep(0, dim(P)[2])
  x0 = mean.face + t(P %*% b0)
  index0 = alignment(x0, s.X0)
  X0 = aligned.face(index0, x0)

  # Initialize
  X = X0
  index = index0
  b = b0

  # Compute gradient model if not already done
  if (!exists("g")) {
    if (verbose) cat("Computing gradient model...\n")
    g <<- gradient(d.train1, im.train1)
  }

  # Convergence loop
  d = 100
  iteration = 0
  d.history = c()

  # Adaptive step size: start with larger steps, decrease over time

  while(d > tolerance && iteration < max_iterations){
    step_size = max(0.5, 1.0 - iteration / max_iterations * 0.5)

    updated = update.optimized(X, index, b, mean.face, new.image, P, lambda, step_size)
    d = dist(X, updated$X)
    d.history = c(d.history, d)

    if (verbose && iteration %% 10 == 0) {
      cat(sprintf("Iteration %d: distance = %.4f\n", iteration, d))
    }

    X = updated$X
    index = updated$index
    b = updated$b
    iteration = iteration + 1

    # Early stopping if oscillating
    if (iteration > 5 && abs(d - d.history[iteration-1]) < 0.001) {
      if (verbose) cat("Converged (oscillation detected)\n")
      break
    }
  }

  if (verbose) {
    cat(sprintf("Converged after %d iterations with distance: %.4f\n", iteration, d))
  }

  return(list(
    X = X,
    index = index,
    b = b,
    iterations = iteration,
    final_distance = d,
    distance_history = d.history
  ))
}


### Helper function to visualize results ###

visualize.result = function(im, X, title = "ASM Result") {
  im.matrix <- matrix(data=rev(im), nrow=96, ncol=96)
  image(1:96, 1:96, im.matrix, col=gray((0:255)/255), main=title)

  pts = ps(X)
  for (i in 1:15){
    points(pts[i,1], pts[i,2], col="red", pch=19, cex=1.2)
  }

  # Draw connections to visualize face structure
  # Eyes
  lines(pts[1:2,1], pts[1:2,2], col="blue", lwd=2)
  # Eyebrows
  lines(pts[3:4,1], pts[3:4,2], col="green", lwd=2)
  lines(pts[5:6,1], pts[5:6,2], col="green", lwd=2)
  # Nose
  lines(pts[7:10,1], pts[7:10,2], col="yellow", lwd=2)
  # Mouth
  lines(pts[12:15,1], pts[12:15,2], col="purple", lwd=2)
}


cat("Optimized ASM functions loaded successfully!\n")
cat("Use fit.asm.optimized() to fit ASM to new images\n")
