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

