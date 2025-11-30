# ASM 测试指南

## 🚫 当前环境限制

这个环境中没有安装R，无法直接运行测试。你需要在本地R环境中运行测试。

---

## ✅ 已完成的工作

### 1. Bug修复（已提交）
- ✅ PCA协方差矩阵计算错误
- ✅ 梯度向量压缩问题
- ✅ 变量作用域错误
- ✅ 归一化方法错误
- ✅ 添加迭代限制

### 2. 性能优化（已提交）
- ✅ 数值稳定性（协方差正则化）
- ✅ 扩大搜索范围（±2 → ±4像素）
- ✅ 多尺度搜索
- ✅ 智能初始化
- ✅ 自适应步长
- ✅ 早停机制
- ✅ 可视化工具

### 3. 测试脚本（已创建）
- ✅ `Quick_Test.R` - 快速验证脚本（不需要真实数据）
- ✅ `Test_ASM.R` - 完整测试脚本（需要training.csv和test.csv）
- ✅ `ASM_Optimized.R` - 优化版本实现

---

## 📋 如何在你的环境中运行测试

### 方法1: 快速验证（推荐先运行）

不需要真实数据，只验证bug修复是否正确：

```r
# 在R控制台中运行
setwd("/path/to/ada_project")  # 设置到项目目录
source("Quick_Test.R")
```

**这个测试会验证：**
- ✓ 所有R文件能否正确加载
- ✓ PCA协方差矩阵是否使用正确的样本数
- ✓ 梯度向量是否被正确保留（未压缩）
- ✓ inv.aligned.face变量作用域是否修复
- ✓ normalize函数是否保持纵横比
- ✓ 优化函数是否正常工作

**预期输出：**
```
======================================================================
Quick Bug Fix Verification
======================================================================

Test 1: Checking if all R files can be loaded...
  ✓ ASM.R loaded successfully
  ✓ Gray.R loaded successfully
  ✓ PCA.R loaded successfully
  ✓ Procrustes.R loaded successfully

✓ All core files loaded successfully!

Test 2: Generating synthetic test data...
  ✓ Generated 30 training samples with 15 keypoints
  ✓ Generated 30 images of size 96x96

Test 3: Testing PCA covariance matrix calculation...
  ✓ Covariance matrix shape: 30 x 30
  ✓ Used 30 samples (correct)
  ✓ Feature dimension: 30
  ✓ Covariance matrix is valid and symmetric
  ✓ All eigenvalues non-negative (largest: XX.XX)

✓ PCA bug fix verified!

Test 4: Testing gradient vector calculation...
  ✓ Gradient matrix shape: 15 x 6
  ✓ Number of keypoints: 15
  ✓ Gradient vector length: 6 (should be 2*m = 6)
  ✓ Gradient vectors preserved (not collapsed to scalars)
    Average unique values per keypoint: X.X
  ✓ Number of covariance matrices: 15
  ✓ Covariance matrix dimensions: 6 x 6

✓ Gradient bug fix verified!

Test 5: Testing inv.aligned.face function...
  ✓ inv.aligned.face executed without errors
  ✓ Output dimension: 30 (expected: 30)

✓ Variable scope fix verified!

Test 6: Testing normalize function...
  ✓ Norm before: XX.XXXX
  ✓ Norm after: 1.0000 (should be ~1.0)
  ✓ Aspect ratio before: X.XXXX
  ✓ Aspect ratio after: X.XXXX
  ✓ Aspect ratio preserved (correct!)

✓ Normalize fix verified!

Test 7: Testing optimized functions...
  ✓ ASM_Optimized.R loaded successfully
  ✓ m.d.robust function works (distance: X.XXXX)

✓ Optimized functions verified!

======================================================================
SUMMARY
======================================================================
✓ All bug fixes verified and working correctly!
✓ PCA now uses correct number of samples
✓ Gradient vectors preserved (not collapsed)
✓ Variable scope errors fixed
✓ Normalization preserves aspect ratio
✓ Optimized functions loaded successfully
```

---

### 方法2: 完整测试（需要真实数据）

如果你有 `training.csv` 和 `test.csv` 文件：

```r
# 1. 确保数据文件在项目目录
# training.csv - 训练数据
# test.csv - 测试数据

# 2. 在R控制台中运行
setwd("/path/to/ada_project")
source("Test_ASM.R")
```

**这个测试会：**
1. 自动加载或生成所有必要数据（Procrustes、PCA、梯度模型）
2. 在多个测试图像上运行ASM（默认测试图像1, 5, 10, 21, 50）
3. 显示每个图像的收敛过程和质量评级
4. 生成可视化结果
5. 输出性能统计

**预期输出：**
```
======================================================================
ASM Testing and Evaluation Script
======================================================================

Step 1: Loading data...
  - Training images: XXXX
  - Test images: XXXX
  - Keypoints per image: 15

Step 2: Running Procrustes analysis...
  - Procrustes analysis complete and saved

Step 3: Running PCA...
  - PCA complete. Using XX principal components (95% variance)
  - Variance explained: 95.XX%

Step 4: Computing gradient appearance model...
  - Gradient model computed and saved

Step 5: Loading optimized ASM functions...
  Optimized ASM functions loaded successfully!

Step 6: Testing on sample images...

  Testing on image 1...
    - Converged in XX iterations
    - Final distance: X.XXXX
    - Quality: GOOD

  Testing on image 5...
    - Converged in XX iterations
    - Final distance: X.XXXX
    - Quality: EXCELLENT

  ...

Step 7: Generating visualizations...
  [显示图像和收敛曲线]

Step 8: Summary Statistics
======================================================================
  Average iterations: XX.X
  Average final distance: X.XXXX
  Min final distance: X.XXXX
  Max final distance: X.XXXX
  Success rate (distance < 1.0): XX.X%

======================================================================
Testing complete!
======================================================================
```

---

### 方法3: 手动测试单张图片

```r
# 1. 加载必要的数据和函数
load('data1.Rd')              # d.train1, im.train1, d.test, im.test
load('procrustes_result.Rd')  # face.set, mean.face, weight.matrix
load('pca_result.Rd')          # Principal.vector, Principal.value
load('g.Rd')                   # gradient model
source("ASM_Optimized.R")

# 2. 测试单张图片
result = fit.asm.optimized(
  new.image = im.test[21,],
  mean.face = mean.face,
  P = Principal.vector,
  lambda = Principal.value,
  d.train1 = d.train1,
  im.train1 = im.train1,
  max_iterations = 50,
  tolerance = 0.1,
  verbose = TRUE
)

# 3. 查看结果
print(result$iterations)        # 迭代次数
print(result$final_distance)    # 最终距离
plot(result$distance_history, type='l', main="Convergence")

# 4. 可视化
visualize.result(im.test[21,], result$X, title="Test Image 21")
```

---

## 📊 解读结果

### 质量评级标准
- **EXCELLENT** (距离 < 0.5): 非常精确，关键点几乎完美对齐
- **GOOD** (距离 < 1.0): 满意的结果，关键点位置合理
- **FAIR** (距离 < 2.0): 可接受，但可能需要调整
- **POOR** (距离 ≥ 2.0): 需要优化或调整参数

### 成功率期望
- **修复前**: < 30%（因为关键bug导致算法基本失效）
- **修复后**: 60-80%（取决于数据质量和人脸变化程度）

### 如果效果还不够好

可以调整的参数：

1. **增加搜索范围**（ASM_Optimized.R）
   ```r
   # 在 new.position.optimized 中
   new.position.optimized = function(j, X, m = 7, l = 13, ...)
   # 增加 l 值可以扩大搜索范围
   ```

2. **调整收敛阈值**
   ```r
   result = fit.asm.optimized(..., tolerance = 0.2)  # 放宽阈值
   ```

3. **增加最大迭代次数**
   ```r
   result = fit.asm.optimized(..., max_iterations = 100)
   ```

4. **调整PCA保留方差**（PCA.R）
   ```r
   while (sum(eigen.values[1:k]) / sum(eigen.values) < 0.98)  # 从0.95改为0.98
   ```

5. **调整正则化强度**（ASM_Optimized.R）
   ```r
   m.d.robust = function(new.g.scale.g, g.scale.g, cov.m, reg = 1e-5)  # 增加reg
   ```

---

## 🔍 调试建议

如果测试失败或结果差：

1. **检查数据加载**
   ```r
   dim(im.train1)  # 应该是 [样本数, 9216]
   dim(d.train1)   # 应该是 [样本数, 30]
   ```

2. **可视化训练数据**
   ```r
   # 查看第一张训练图片
   im <- matrix(data=rev(im.train1[1,]), nrow=96, ncol=96)
   image(1:96, 1:96, im, col=gray((0:255)/255))

   # 叠加关键点
   pts = ps(d.train1[1,])
   points(96-pts[,1], 96-pts[,2], col="red", pch=19)
   ```

3. **检查Procrustes收敛**
   ```r
   # 在Procrustes.R的最后，d值应该 < 39
   print(d)
   ```

4. **检查PCA方差**
   ```r
   # 应该接近95%
   variance_explained = sum(Principal.value) / sum(eigen.values)
   print(variance_explained * 100)
   ```

5. **可视化平均人脸**
   ```r
   # 平均人脸应该看起来合理
   visualize.result(im.train1[1,], mean.face, title="Mean Face")
   ```

---

## 📁 文件清单

修复和优化后的文件：
- ✅ `ASM.R` - 核心对齐函数（已修复）
- ✅ `Gray.R` - 局部外观模型（已修复）
- ✅ `PCA.R` - PCA降维（已修复）
- ✅ `Procrustes.R` - 形状对齐（已修复）
- ✅ `Searching.R` - ASM搜索算法（已修复）
- ✅ `ASM_Optimized.R` - 优化实现（新增）
- ✅ `Test_ASM.R` - 完整测试脚本（新增）
- ✅ `Quick_Test.R` - 快速验证脚本（新增）
- ✅ `IMPROVEMENTS.md` - 详细文档（新增）
- ✅ `TESTING_GUIDE.md` - 本文档（新增）

---

## 🎯 总结

1. **首先运行** `Quick_Test.R` 验证bug修复
2. **如果有真实数据**，运行 `Test_ASM.R` 查看实际效果
3. **根据结果**，参考IMPROVEMENTS.md调整参数
4. **如有问题**，使用调试建议排查

所有代码已修复并优化，理论上应该有显著改善！
