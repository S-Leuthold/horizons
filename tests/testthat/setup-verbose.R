## The verbs print their progress trees unless told not to. The tests quiet
## them all here; a test of console output turns it back on with
## control_horizons(verbose = TRUE), or verbose = TRUE for the other verbs.
withr::local_options(horizons.verbose = FALSE, .local_envir = testthat::teardown_env())
