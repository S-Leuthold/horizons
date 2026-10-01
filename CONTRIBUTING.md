# Contributing to horizons

Contributions to horizons are more than welcome. If you work with mid-infrared spectra, I'd encourage you to try these methods on your own data and tell me where they break, what doesn't make sense, or which methods might be better suited to a given step. Feel free to open an issue, ask a question or open a pull request, or email me at sam.leuthold@colostate.edu.

## Issues

[Open an issue](https://github.com/S-Leuthold/horizons/issues) for bugs, questions, confusing behaviour or ideas. For a bug, it helps to include what you ran, what you expected and what happened, plus `sessionInfo()`. A small reproducible example (the [reprex](https://reprex.tidyverse.org) package makes one easy) is great but not required.

## Pull requests

Open them against the `development` branch; `main` holds releases. For something large, a quick issue first saves us both from surprises. A few practical notes:

- Add a line to `NEWS.md` for anything a user would notice, and tests for what you changed.
- New files in `R/` need adding to the `Collate` field in `DESCRIPTION`, or they are left out of the installed package.
- Run the tests with `NOT_CRAN` set, as CI does: `Sys.setenv(NOT_CRAN = "true"); devtools::test()`.
- Style follows the existing code: explicit `pkg::` namespaces, roxygen2 for every function, and `cli` for messages.

Don't worry about getting all of that right. I'm happy to help get a pull request over the line.

## Conduct

Be kind and assume good faith.
