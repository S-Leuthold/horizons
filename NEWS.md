# horizons 0.9.0

## Major Changes

* **Soil covariate prediction now uses OSSL-centric PCA clustering**
  - Trains PCA on global OSSL reference library (12K+ samples)
  - Projects unknown samples into OSSL spectral space
  - **Performance**: 8-10% R² improvement over previous approach
  - Default configuration: 1st derivative + k-means clustering
  - Achieves R² > 0.92 for most soil properties

## Breaking Changes

* None - API remains fully backward compatible
* `n_similar` parameter deprecated (ignored, uses all available OSSL)

## New Features

* Advanced clustering options via experimental parameters:
  - `derivative_order`: 0 (smoothing), 1 (1st derivative), 2 (2nd derivative)
  - `clustering_method`: "kmeans" or "ward"
  - `use_mahalanobis`: TRUE/FALSE distance metric selection
  - `distance_percentile`: Threshold for OSSL cluster assignment

## Bug Fixes

* Increased `future.globals.maxSize` to 8GB to handle large OSSL datasets
* Fixed CLI tree structure for nested verbose output
* Added strategic `gc()` calls to reduce memory pressure

## Performance

* Default config: R² = 0.925 (clay) in ~22 minutes
* Maximum accuracy config (Ward + 2nd derivative): R² = 0.953 in ~68 minutes
* Memory usage: ~4GB per parallel worker

---

# horizons 0.8.2

Previous version - see Git history for details.
