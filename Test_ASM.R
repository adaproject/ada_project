### Comprehensive Test Script for ASM Implementation ###
### This script tests the bug fixes and optimizations

cat("="*70, "\n")
cat("ASM Testing and Evaluation Script\n")
cat("="*70, "\n\n")

### Step 1: Load the data ###
cat("Step 1: Loading data...\n")

if (file.exists('data1.Rd')) {
  load('data1.Rd')
  cat("  - Loaded existing data from data1.Rd\n")
} else {
  cat("  - data1.Rd not found. Running CleanData.R...\n")
  source("CleanData.R")
}

cat(sprintf("  - Training images: %d\n", nrow(im.train1)))
cat(sprintf("  - Test images: %d\n", nrow(im.test)))
cat(sprintf("  - Keypoints per image: %d\n", ncol(d.train1)/2))

### Step 2: Run Procrustes Analysis ###
cat("\nStep 2: Running Procrustes analysis...\n")

if (!file.exists("procrustes_result.Rd")) {
  cat("  - Running Procrustes alignment (this may take a while)...\n")
  source("Procrustes.R")
  save(face.set, mean.face, weight.matrix, file="procrustes_result.Rd")
  cat("  - Procrustes analysis complete and saved\n")
} else {
  cat("  - Loading existing Procrustes results...\n")
  load("procrustes_result.Rd")
  cat("  - Procrustes results loaded\n")
}

### Step 3: Run PCA ###
cat("\nStep 3: Running PCA...\n")

if (!file.exists("pca_result.Rd")) {
  cat("  - Computing PCA...\n")
  source("PCA.R")
  save(P, Principal.vector, Principal.value, eigen.values, file="pca_result.Rd")
  cat(sprintf("  - PCA complete. Using %d principal components (95%% variance)\n", ncol(P)))
} else {
  cat("  - Loading existing PCA results...\n")
  load("pca_result.Rd")
  cat(sprintf("  - PCA loaded. Using %d principal components\n", ncol(Principal.vector)))
}

# Print variance explained
variance.explained = sum(Principal.value) / sum(eigen.values)
cat(sprintf("  - Variance explained: %.2f%%\n", variance.explained * 100))

### Step 4: Compute Gradient Model ###
cat("\nStep 4: Computing gradient appearance model...\n")

if (!file.exists("g.Rd")) {
  cat("  - Computing gradient model (this may take a while)...\n")
  library(foreach)
  g = gradient(d.train1, im.train1)
  save(g, file='g.Rd')
  cat("  - Gradient model computed and saved\n")
} else {
  cat("  - Loading existing gradient model...\n")
  load('g.Rd')
  cat("  - Gradient model loaded\n")
}

### Step 5: Load optimized functions ###
cat("\nStep 5: Loading optimized ASM functions...\n")
source("ASM_Optimized.R")

### Step 6: Test on sample images ###
cat("\nStep 6: Testing on sample images...\n")

# Test on a few test images
test_indices = c(1, 5, 10, 21, 50)
results = list()

for (idx in test_indices) {
  cat(sprintf("\n  Testing on image %d...\n", idx))

  result = fit.asm.optimized(
    new.image = im.test[idx,],
    mean.face = mean.face,
    P = Principal.vector,
    lambda = Principal.value,
    d.train1 = d.train1,
    im.train1 = im.train1,
    max_iterations = 50,
    tolerance = 0.1,
    verbose = FALSE
  )

  results[[as.character(idx)]] = result

  cat(sprintf("    - Converged in %d iterations\n", result$iterations))
  cat(sprintf("    - Final distance: %.4f\n", result$final_distance))

  # Check convergence quality
  if (result$final_distance < 0.5) {
    cat("    - Quality: EXCELLENT\n")
  } else if (result$final_distance < 1.0) {
    cat("    - Quality: GOOD\n")
  } else if (result$final_distance < 2.0) {
    cat("    - Quality: FAIR\n")
  } else {
    cat("    - Quality: POOR (may need more iterations or different initialization)\n")
  }
}

### Step 7: Visualize results ###
cat("\nStep 7: Generating visualizations...\n")

# Create a plot with multiple results
par(mfrow=c(2, 3))
for (idx in test_indices) {
  result = results[[as.character(idx)]]
  visualize.result(
    im.test[idx,],
    result$X,
    title = sprintf("Test Image %d (d=%.3f)", idx, result$final_distance)
  )
}

# Plot convergence history
par(mfrow=c(2, 3))
for (idx in test_indices) {
  result = results[[as.character(idx)]]
  plot(result$distance_history, type='l', col='blue', lwd=2,
       main=sprintf("Convergence: Image %d", idx),
       xlab="Iteration", ylab="Distance",
       ylim=c(0, max(result$distance_history)))
  abline(h=0.1, col='red', lty=2)
  grid()
}

### Step 8: Summary Statistics ###
cat("\nStep 8: Summary Statistics\n")
cat("="*70, "\n")

all_iterations = sapply(results, function(r) r$iterations)
all_distances = sapply(results, function(r) r$final_distance)

cat(sprintf("  Average iterations: %.1f\n", mean(all_iterations)))
cat(sprintf("  Average final distance: %.4f\n", mean(all_distances)))
cat(sprintf("  Min final distance: %.4f\n", min(all_distances)))
cat(sprintf("  Max final distance: %.4f\n", max(all_distances)))

success_rate = sum(all_distances < 1.0) / length(all_distances) * 100
cat(sprintf("  Success rate (distance < 1.0): %.1f%%\n", success_rate))

cat("\n")
cat("="*70, "\n")
cat("Testing complete!\n")
cat("="*70, "\n")

### Save test results ###
save(results, test_indices, file="test_results.Rd")
cat("\nTest results saved to test_results.Rd\n")
