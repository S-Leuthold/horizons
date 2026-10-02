library(testthat)
library(horizons)

## Record which tier this run is (tests/testthat/helper-tier.R reads the same
## variable). CI checks this line, and the skip reasons, to prove the slow tier
## ran when it should have.
slow_tier <- isTRUE(as.logical(Sys.getenv("HORIZONS_SLOW_TESTS", "false")))
cat("horizons slow tier: ", if (slow_tier) "on" else "off", "\n", sep = "")

test_check("horizons")
