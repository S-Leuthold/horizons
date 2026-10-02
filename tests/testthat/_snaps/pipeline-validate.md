# validate() passed logic and CLI output / CLI output shows PASSED when all clean

    Code
      validate(hd)
    Output
      ├─ Validating...
      │  ├─ ✓ Samples: 100 (≥ 50)
      │  ├─ ✓ CV feasibility: 100 ≥ 10
      │  ├─ ✓ Outcome variance: 26.61
      │  ├─ ✓ Missing: 0/100 (0%)
      │  ├─ ✓ Near-zero variance: 0
      │  ├─ ✓ Cubist feasibility: 100 x 50 cells, 0 config(s) flagged
      │  ├─ ℹ Spectral outliers: 0
      │  ├─ ℹ Response outliers: 1
      │  └─ Status: PASSED
      │

# validate() passed logic and CLI output / CLI output shows FAILED when ERROR present

    Code
      validate(hd)
    Condition
      Warning:
      IQR is zero or near-zero — skipping response outlier detection.
    Output
      ├─ Validating...
      │  ├─ ✓ Samples: 100 (≥ 50)
      │  ├─ ✓ CV feasibility: 100 ≥ 10
      │  ├─ ✗ Outcome variance: 0
      │  ├─ ✓ Missing: 0/100 (0%)
      │  ├─ ✓ Near-zero variance: 0
      │  ├─ ✓ Cubist feasibility: 100 x 50 cells, 0 config(s) flagged
      │  ├─ ℹ Spectral outliers: 0
      │  ├─ ℹ Response outliers: 0
      │  └─ Status: FAILED
      │

# validate() passed logic and CLI output / CLI output shows outlier removal summary when removal happens

    Code
      validate(hd, remove_outliers = TRUE)
    Output
      ├─ Validating...
      │  ├─ ✓ Samples: 100 (≥ 50)
      │  ├─ ✓ CV feasibility: 100 ≥ 10
      │  ├─ ✓ Outcome variance: 26.61
      │  ├─ ✓ Missing: 0/100 (0%)
      │  ├─ ✓ Near-zero variance: 0
      │  ├─ ✓ Cubist feasibility: 100 x 50 cells, 0 config(s) flagged
      │  ├─ ℹ Spectral outliers: 3
      │  ├─ ℹ Response outliers: 1 on the whole table, not removed
      │  │  └─ Trim requested: evaluate() fences its training partition (1.5 x IQR) and trims training rows only; test rows untouched
      │  ├─ Removed 3 spectral outliers
      │  ├─ 97 samples remaining
      │  └─ Status: PASSED
      │

