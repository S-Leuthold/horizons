# ensemble() - the meta-learner as one unit / says in its own words that the meta-learner could not be built, and when CV+ could not be calibrated

    Code
      ensemble(fitted, method = "penalized", optimize = FALSE, compute_uq = FALSE)
    Condition
      Error in `ensemble()`:
      ! The penalized ensemble could not be built: its meta-learner failed.
      x Member weights failed: the weights broke
      i The failure is on this condition as results: `rlang::last_error()$results`.

---

    Code
      invisible(ensemble(fitted, method = "weighted", optimize = FALSE))
    Condition
      Warning:
      Prediction intervals (CV+) were not computed: CV+ fold models failed: a fold refit failed

# ensemble() - the summary it prints / shows the top members, the score, an improvement over the best member, the UQ and the runtime

    Code
      render_ensemble_summary(better, "rpd")
    Output
      │  Top members (by weight)
      │  ├─ cubist_log: 0.5
      │  ├─ plsr_log: 0.3
      │  └─ plsr_sqrt: 0.2
      │
      │  Summary
      │  ├─ Ensemble rpd: 1.87
      │  ├─ Improvement over best member: +0.0412
      │  ├─ UQ: CV+ conformal (n_calib = 24)
      │  │  └─ UQ coverage: 83.3% on 12 held-out test rows (nominal 90%, mean width 3.04)
      │  └─ Runtime: 12.4s
      │
      └─ Class: horizons_fit → horizons_ensemble

# ensemble() - the summary it prints / prints the line evaluate() and fit() print for a draw that did not stratify as asked, under the members and under the UQ

    Code
      render_ensemble_summary(summary_contract(improvement = 0.0412, coef = c(0.5,
        0.3, 0.2)), "rpd", lines)
    Output
      │  Top members (by weight)
      │  ├─ cubist_log: 0.5
      │  ├─ plsr_log: 0.3
      │  └─ plsr_sqrt: 0.2
      │  Stratified meta-learner CV on fewer strata: too little data for 4
      │
      │  Summary
      │  ├─ Ensemble rpd: 1.87
      │  ├─ Improvement over best member: +0.0412
      │  ├─ UQ: CV+ conformal (n_calib = 24)
      │  │  ├─ Stratified CV+ folds failed, retrying without strata
      │  │  └─ UQ coverage: 83.3% on 12 held-out test rows (nominal 90%, mean width 3.04)
      │  └─ Runtime: 12.4s
      │
      └─ Class: horizons_fit → horizons_ensemble

# ensemble() - the summary it prints / orders members by the size of their weight, and flags an ensemble worse than its best member

    Code
      render_ensemble_summary(worse, "rpd")
    Output
      │  Top members (by weight)
      │  ├─ plsr_log: -0.5
      │  ├─ plsr_sqrt: 0.3
      │  └─ cubist_log: 0.2
      │
      │  Summary
      │  ├─ Ensemble rpd: 1.87
      │  ├─ Improvement over best member: -0.0123
      │  ├─ UQ: not computed
      │  └─ Runtime: 2.1 min
      │
      └─ Class: horizons_fit → horizons_ensemble

