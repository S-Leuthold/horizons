## ===========================================================================
## Overnight run, 2026-09-21: shared parameters for scripts 12, 13 and 14
## ===========================================================================
##
## Declared BEFORE the runs, so tomorrow reads results against a bar set
## today. Changing a value here changes the verdict tables without refitting.
##
## Hypotheses under test:
##   12  The laptop cannot run the full-library workflow. (Measured, not
##       enforced: peak RSS of the sequential pipeline on the clay core.)
##   13  The library cannot be thinned without losing accuracy. (A thinning
##       competition, judged by noninferiority against the full core.)
##   14  Off-library, selection's gain is bias correction in disguise.
##       (Global prediction plus a local residual correction, against the
##       best selection arm from 10.)

OVERNIGHT <- list(

  ## Budgets and margins ------------------------------------------------------
  artifact_mb        = 50,      # shipped library size budget, MB (qs2 on disk)
  laptop_ram_gb      = 16,      # the laptop the product is for; 32 is the upper case
  margin_rmse        = 0.05,    # a subset passes if RMSE <= (1 + margin) * reference RMSE on the test split
  margin_tail        = 0.10,    # and on every tail bin
  tail_bins          = list(low = c(0, 10), high = c(50, Inf)),   # clay %, test-split bins

  ## Fixed design --------------------------------------------------------------
  property           = "clay",
  transformation     = "none",
  resample           = 4,       # cm-1; matches 10 and 11
  k                  = 100L,    # neighbour count for scripts 13 (stratified cells) and 14
  sdev_floor         = 0.10,    # the similarity space as it ships
  mask               = rbind(c(1580, 1720), c(3100, 3700)),
  sizes              = c(2000L, 6000L, 12000L),
  random_seeds       = c(307L, 308L, 309L),
  selectors          = c("random", "kennard_stone", "duplex", "kmedoids", "stratified"),
  learners           = c("rf", "cubist"),
  preprocessing      = "snv",
  feature_selection  = "pca",
  seed               = 307L,

  ## Fixtures ----------------------------------------------------------------
  ames               = c(lat = 42.03, lon = -93.62),
  iowa_n             = 300L,
  moys_opus          = "/data/workshop/projects/ai-leaf/data/processed/MOYS/opus_files",
  moys_csv           = "/data/workshop/projects/ai-leaf/data/raw/MOYS.csv"
)
