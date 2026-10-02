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
