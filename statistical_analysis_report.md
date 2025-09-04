# Statistical Analysis Report: Soil Property Prediction Pipeline
## Performance Degradation Investigation (R² from 0.94 to 0.69 for Clay)

---

## Executive Summary

The soil property prediction pipeline shows signs of multiple statistical issues that could explain the severe performance degradation from R² ~0.94 to 0.69. Key concerns include:

1. **PCA Overfitting**: Using 95% variance threshold results in excessive components (~50+) relative to sample size
2. **Data Leakage**: Potential information leakage through global PCA fitting before train/test split
3. **Validation Strategy Issues**: Single hold-out validation without proper nested CV for hyperparameter tuning
4. **Mahalanobis Distance Instability**: Numerical instability in covariance matrix calculations
5. **Sample Selection Bias**: Stratified Kennard-Stone may create overly optimistic training sets

---

## 1. Statistical Methodology Assessment

### 1.1 PCA Implementation Issues

**Current Approach:**
```r
# Line 600-640 in covariates-data.R
FactoMineR::PCA(X = spectral_matrix,
                scale.unit = TRUE,
                graph = FALSE,
                ncp = min(100, ncol(spectral_matrix) - 1))
```

**Statistical Concerns:**

1. **Variance Threshold Too High (95%)**
   - For MIR spectroscopy with ~1700 wavenumbers, this typically results in 50-100 components
   - Rule of thumb: n_components should be << sqrt(n_samples)
   - With 6000 training samples, this suggests max ~77 components, but practical limit is closer to 20-30
   - High dimensionality increases overfitting risk

2. **No Cross-Validation for PCA Parameters**
   - PCA is fit on entire OSSL dataset without validation
   - Variance threshold is fixed, not optimized
   - No assessment of reconstruction error on held-out data

3. **Scale Normalization After SNV**
   - SNV (Standard Normal Variate) already normalizes spectra
   - Additional scaling in PCA may distort spectral relationships
   - Double normalization can amplify noise

**Recommendation:**
- Reduce variance threshold to 80-85%
- Implement CV-based selection of optimal components
- Consider PLS (Partial Least Squares) instead of unsupervised PCA

### 1.2 Train/Test Splitting Problems

**Current Approach:**
```r
# Line 138-145 in covariates-similarity.R
rsample::initial_split(selected_ossl, prop = 0.8)
```

**Statistical Concerns:**

1. **Single Random Split**
   - No stratification by target variable (clay content)
   - Single split provides unstable performance estimates
   - 80/20 split may be too aggressive for complex models

2. **PCA Before Split**
   - PCA model is fit on entire OSSL dataset
   - Test samples influence PCA transformation
   - Violates independence assumption

3. **No Temporal or Spatial Considerations**
   - Soil samples may have spatial/temporal correlations
   - Random split doesn't account for autocorrelation
   - Can lead to overly optimistic validation metrics

**Recommendation:**
- Use stratified splits based on target variable distribution
- Implement nested cross-validation
- Fit PCA only on training data

### 1.3 Cross-Validation Strategy

**Current Approach:**
```r
# Line 128 in fit_cubist_model.R
rsample::vfold_cv(Train_Data, v = 10, strata = "Response")
```

**Statistical Concerns:**

1. **CV Only for Hyperparameter Tuning**
   - 10-fold CV used only within training set
   - No outer CV loop for unbiased performance estimation
   - Final model evaluation on single validation set

2. **Information Leakage in Similarity Selection**
   - Similarity-based selection uses entire dataset structure
   - Selected "similar" samples may be too homogeneous
   - Creates artificially high performance

3. **No Repeated CV**
   - Single 10-fold split may be unstable
   - Repeated CV (e.g., 5x10-fold) provides better estimates
   - Variance of performance estimates not assessed

**Recommendation:**
- Implement nested CV: outer loop for performance, inner for tuning
- Use repeated CV for stability
- Consider Monte Carlo CV for large datasets

---

## 2. Potential Sources of Overfitting

### 2.1 Dimensionality Curse

**Issue:** High PCA dimensions relative to sample size
```r
# Typical scenario:
# - 6000 OSSL samples selected
# - 4800 training samples (80%)
# - 50-100 PCA components (95% variance)
# - Ratio: ~50-100 samples per dimension
```

**Statistical Impact:**
- Cubist model can memorize training patterns
- Insufficient samples to estimate covariance reliably
- Validation performance degrades significantly

### 2.2 Similarity-Based Selection Bias

**Current Implementation:**
```r
# Lines 72-93 in covariates-similarity.R
distances_to_centroid <- stats::mahalanobis(
    x = ossl_matrix,
    center = unknown_center,
    cov = unknown_cov
)
```

**Problems:**
1. **Covariance Matrix Instability**
   - Unknown samples may be too few for stable covariance estimation
   - High-dimensional space exacerbates the problem
   - Mahalanobis distance becomes unreliable

2. **Selection Bias**
   - Selecting "most similar" OSSL samples creates homogeneous training set
   - Reduces variability needed for robust model training
   - Model performs poorly on slightly different samples

### 2.3 Model Complexity

**Cubist Configuration:**
```r
# Lines 211-214 in fit_cubist_model.R
committees = tune::tune()  # Range: 2-20
neighbors = tune::tune()   # Range: 2-9
max_rules = tune::tune()   # Default range
```

**Concerns:**
- No regularization on rule complexity
- Committee models can overfit with high-dimensional input
- Neighbor correction adds another layer of potential overfitting

---

## 3. Validation Strategy Issues

### 3.1 Lack of Proper Test Set

**Current Flow:**
1. Select 6000 OSSL samples based on similarity
2. Split 80/20 for train/validation
3. Use validation set for final metrics

**Problems:**
- Validation set is not truly independent (selected based on similarity)
- No separate test set for final unbiased evaluation
- Performance metrics likely overestimated

### 3.2 Metric Selection

**Current Metrics:**
- R² and RMSE computed on validation set
- No confidence intervals
- No analysis of residual patterns

**Missing Assessments:**
- Prediction intervals
- Residual analysis (heteroscedasticity, normality)
- Bias assessment across clay content range
- Cross-validation standard errors

---

## 4. PCA and Dimensionality Reduction Analysis

### 4.1 Variance Threshold Problem

**Mathematical Analysis:**
For MIR spectra with p = 1700 wavenumbers and n = 6000 samples:

- Theoretical max rank: min(n-1, p) = 1700
- Practical dimensionality: n/10 to n/20 = 300-600
- 95% variance typically requires: 50-100 components
- Effective samples per dimension: 6000/100 = 60

**Statistical Power:**
- Rule of thumb: 10-20 samples per predictor minimum
- Current: ~60 samples per PC (marginal)
- After train/test split: ~48 samples per PC (insufficient)

### 4.2 Alternative Approaches

**Better Options:**
1. **Supervised PCA or PLS**
   - Use target variable to guide dimension reduction
   - More relevant components for prediction

2. **Sparse PCA**
   - Enforces sparsity in loadings
   - Reduces overfitting risk

3. **Autoencoders**
   - Non-linear dimension reduction
   - Better capture of spectral features

---

## 5. Recommendations for Statistical Improvements

### 5.1 Immediate Fixes (High Priority)

1. **Reduce PCA Variance Threshold**
```r
# Change from 95% to 85%
variance_threshold = 0.85  # Will likely reduce to 20-30 components
```

2. **Implement Nested Cross-Validation**
```r
# Outer loop: 5-fold CV for performance estimation
# Inner loop: 10-fold CV for hyperparameter tuning
outer_cv <- rsample::vfold_cv(data, v = 5, strata = "clay")
```

3. **Fix PCA Data Leakage**
```r
# Fit PCA only on training data
pca_model <- FactoMineR::PCA(train_spectra, ...)
test_pca <- predict(pca_model, test_spectra)
```

### 5.2 Medium-Term Improvements

1. **Replace Mahalanobis with Robust Alternatives**
   - Use robust covariance estimation (MCD, MVE)
   - Consider Euclidean distance in PCA space
   - Implement regularized Mahalanobis

2. **Stratified Sampling Strategy**
   - Stratify by target variable, not just spectral similarity
   - Ensure representative coverage of clay content range
   - Use deterministic sampling (e.g., Latin Hypercube)

3. **Model Regularization**
   - Add L1/L2 penalties to Cubist rules
   - Limit model complexity based on sample size
   - Implement early stopping

### 5.3 Long-Term Recommendations

1. **Ensemble Approaches**
   - Combine multiple models (RF, XGBoost, Cubist)
   - Use stacking with cross-validated predictions
   - Implement uncertainty quantification

2. **Bayesian Modeling**
   - Gaussian Process regression for uncertainty
   - Hierarchical models for multi-property prediction
   - Proper prior specification for regularization

3. **Validation Framework**
   - Separate holdout test set (20% of data)
   - Time-based or spatial CV if applicable
   - Bootstrap confidence intervals for metrics

---

## 6. Statistical Red Flags Summary

### Critical Issues:
1. ✗ **95% PCA variance threshold too high** - Major overfitting risk
2. ✗ **PCA before train/test split** - Data leakage
3. ✗ **Single validation split** - Unstable performance estimates
4. ✗ **Mahalanobis in high dimensions** - Numerical instability
5. ✗ **No regularization** - Model complexity unconstrained

### Warning Signs:
1. ⚠ **Large R² drop (0.94 → 0.69)** - Classic overfitting signature
2. ⚠ **Similarity-based selection** - Creates biased training sets
3. ⚠ **No confidence intervals** - Uncertainty not quantified
4. ⚠ **Single model approach** - No ensemble benefits
5. ⚠ **Fixed hyperparameters** - Not adapted to data characteristics

---

## 7. Recommended Testing Protocol

To verify these hypotheses, conduct the following experiments:

### Experiment 1: PCA Components
```r
# Test different variance thresholds
thresholds <- c(0.70, 0.75, 0.80, 0.85, 0.90, 0.95)
results <- map(thresholds, function(t) {
  # Rerun pipeline with threshold t
  # Record validation R²
})
```

### Experiment 2: Cross-Validation
```r
# Implement proper nested CV
cv_results <- nested_cv(
  outer_folds = 5,
  inner_folds = 10,
  data = ossl_data,
  target = "clay"
)
```

### Experiment 3: Sample Size Analysis
```r
# Learning curves
sample_sizes <- c(1000, 2000, 3000, 4000, 5000, 6000)
learning_curve <- map(sample_sizes, function(n) {
  # Train with n samples
  # Evaluate on fixed test set
})
```

---

## Conclusion

The performance degradation from R² 0.94 to 0.69 is consistent with severe overfitting due to:
1. Excessive PCA dimensions (95% variance)
2. Data leakage through global PCA fitting
3. Biased sample selection through similarity metrics
4. Insufficient validation strategy

**Priority Actions:**
1. Immediately reduce PCA variance threshold to 85%
2. Implement proper train/test separation before PCA
3. Add nested cross-validation
4. Monitor training vs validation performance gap

The current methodology violates several fundamental statistical principles, particularly the independence of training and test sets, and the bias-variance tradeoff in high-dimensional settings. Addressing these issues should significantly improve model generalization and restore performance closer to expected levels.