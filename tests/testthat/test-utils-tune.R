## ---------------------------------------------------------------------------
## Tests: the helpers that gather what tune and the fold preps report
## ---------------------------------------------------------------------------

describe("capture_fold_warnings()", {

  it("records another package's warning with its fold and muffles it, and lets a horizons warning go on", {

    seen <- character(0)

    out <- withCallingHandlers(
      capture_fold_warnings({
        warning("a step warned", call. = FALSE)
        warn_in_unit("quality", "A horizons notice.")
        "the prep"
      }, "Fold2"),
      warning = function(w) {
        seen <<- c(seen, conditionMessage(w))
        rlang::cnd_muffle(w)
      }
    )

    expect_identical(out$value, "the prep")
    expect_identical(out$arrivals$cause, "a step warned")
    expect_identical(out$arrivals$fold, "Fold2")
    expect_identical(seen, "A horizons notice.")

  })

})
