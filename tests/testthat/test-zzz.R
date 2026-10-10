## ===========================================================================
## startup_message(): development builds say so and point at the release
## ===========================================================================

describe("startup_message()", {

  it("marks a development version and names the release install", {

    msg <- startup_message(package_version("0.10.0.9000"))

    expect_match(msg, "horizons 0.10.0.9000 (development version)", fixed = TRUE)
    expect_match(msg, 'remotes::install_github("S-Leuthold/horizons@main")', fixed = TRUE)
    expect_match(msg, "github.com/S-Leuthold/horizons/issues", fixed = TRUE)

  })

  it("keeps a release version short", {

    msg <- startup_message(package_version("0.11.0"))

    expect_match(msg, "^horizons 0.11.0\\. ")
    expect_no_match(msg, "development", fixed = TRUE)
    expect_no_match(msg, "@main", fixed = TRUE)

  })

  it("treats a fourth component below 9000 as a release", {

    expect_no_match(startup_message(package_version("0.11.0.1")), "development", fixed = TRUE)

  })

})


## ===========================================================================
## .onLoad(): opt-in thread control, and the engines it loads
## ===========================================================================

describe(".onLoad()", {

  thread_vars <- c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS")

  it("sets one thread for workers when HORIZONS_THREAD_CONTROL is TRUE, and nothing otherwise", {

    withr::local_options(ranger.num.threads = NULL)
    withr::local_envvar(HORIZONS_THREAD_CONTROL = "TRUE", OMP_NUM_THREADS = NA,
                        OPENBLAS_NUM_THREADS = NA, MKL_NUM_THREADS = NA)

    .onLoad(NULL, "horizons")

    expect_identical(unname(Sys.getenv(thread_vars)), rep("1", 3))
    expect_equal(getOption("ranger.num.threads"), 1)

    withr::local_envvar(HORIZONS_THREAD_CONTROL = NA, OMP_NUM_THREADS = NA,
                        OPENBLAS_NUM_THREADS = NA, MKL_NUM_THREADS = NA)
    options(ranger.num.threads = NULL)

    .onLoad(NULL, "horizons")

    expect_identical(unname(Sys.getenv(thread_vars, unset = NA)), rep(NA_character_, 3))
    expect_null(getOption("ranger.num.threads"))

  })

  it("loads rules, so an installed horizons can fit a cubist config", {

    ## load_all() loads every Import itself, so only an installed build
    ## shows a missing load
    skip_if_dev_package()
    skip_on_cran()
    skip_if_not_installed("callr")

    has_cubist <- callr::r(function() {
      library(horizons)
      "Cubist" %in% parsnip::show_engines("cubist_rules")$engine
    })

    expect_true(has_cubist)

  })

})

