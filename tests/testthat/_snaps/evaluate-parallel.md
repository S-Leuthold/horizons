# monitor_evaluate() / renders every line of its display

    Code
      .render_monitor(stats, manifest)
    Output
      ──────────────────────────────────────────────────
        evaluate() monitor — <time>
        Parallel:  over configs on multisession (4 workers)
        Data:      120 training rows of SOC, hash 0123456789ab
        Settings:  grid_size = 10, outcome_range = -Inf, Inf
      ──────────────────────────────────────────────────
      
        Progress:  3 / 4 (75%)
        Ignored:   3 checkpoints (1 other training data, 2 earlier schema)
        Unreadable: cfg_009.rds
        Rate:      12.3 models/hr
        ETA:       5 min
        Best:      cfg_002 — RPD = 2.346
      
        Recent completions:
          • cfg_002 (rpd 2.35)
          • cfg_003 (rpd 1.90)
      
      ──────────────────────────────────────────────────

