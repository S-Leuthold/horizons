# ensemble() - the meta-learner as one unit / says in its own words that the meta-learner could not be built, and when CV+ could not be calibrated

    Code
      ensemble(ens_fitted(), method = "penalized", optimize = FALSE, compute_uq = FALSE)
    Condition
      Error in `ensemble()`:
      ! The penalized ensemble could not be built: its meta-learner failed.
      x Member weights failed: the weights broke
      i The failure is on this condition as results: `rlang::last_error()$results`.

---

    Code
      invisible(ensemble(ens_fitted(), method = "weighted", optimize = FALSE))
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
      │  ├─ UQ: CV+ conformal (level 0.9, n_calib = 24)
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

