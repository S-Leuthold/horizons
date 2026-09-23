## The pipeline board's accuracy gate (dev/specs/v1-refactor/pipeline-board.md).
##
## Reads vignettes/board/board-data.json and fails when it has drifted from the
## package: a pipeline verb without a node, an argument whose name or default
## differs from formals(), an argument without a "when to touch it" sentence,
## a source pointer that does not resolve, or a design-ahead entry the code has
## already caught up with. Skips when the source tree is not beside the tests
## (an installed-only check has no vignettes/ or R/ to read).

pkg_root <- testthat::test_path("..", "..")
json_path <- file.path(pkg_root, "vignettes", "board", "board-data.json")

skip_if_no_board <- function() {
  testthat::skip_if_not(file.exists(json_path), "board-data.json not beside the tests")
  testthat::skip_if_not(dir.exists(file.path(pkg_root, "R")), "package sources not beside the tests")
  testthat::skip_if_not_installed("jsonlite")
}

read_board <- function() jsonlite::fromJSON(json_path, simplifyVector = FALSE)

## The verbs the board must carry.
PIPELINE_VERBS <- c("spectra", "standardize", "average", "add_response", "select_training",
                    "configure", "validate", "evaluate", "fit", "ensemble",
                    "predict.horizons_fit", "predict.horizons_ensemble")

## Resolve `path#Lnn` or `path#heading-slug` against the source tree.
slugify <- function(h) {
  h <- tolower(trimws(gsub("^#+\\s*", "", h)))
  h <- gsub("[^a-z0-9 -]", "", h)
  h <- gsub("\\s+", "-", h)
  gsub("-+", "-", h)
}

resolves <- function(pointer) {
  if (is.null(pointer) || is.na(pointer) || !nzchar(pointer)) return("empty pointer")
  parts <- strsplit(pointer, "#", fixed = TRUE)[[1]]
  path  <- file.path(pkg_root, parts[1])
  if (!file.exists(path)) return(paste("missing file:", parts[1]))
  if (length(parts) == 1) return(TRUE)
  anchor <- parts[2]
  lines  <- readLines(path, warn = FALSE)
  if (grepl("^L[0-9]+$", anchor)) {
    n <- as.integer(sub("^L", "", anchor))
    if (n > length(lines)) return(paste0("line ", n, " past end of ", parts[1], " (", length(lines), " lines)"))
    return(TRUE)
  }
  heads <- slugify(lines[grepl("^#{1,6}\\s", lines)])
  if (!anchor %in% heads) return(paste0("no heading '", anchor, "' in ", parts[1]))
  TRUE
}

test_that("every pipeline verb has a node", {
  skip_if_no_board()
  b <- read_board()
  have <- vapply(b$verbs, function(v) v$verb, character(1))
  expect_setequal(intersect(PIPELINE_VERBS, have), PIPELINE_VERBS)
})

test_that("argument names and defaults match formals()", {
  skip_if_no_board()
  b <- read_board()
  ns <- asNamespace("horizons")
  for (v in b$verbs) {
    fm <- formals(get(v$verb, envir = ns))
    got_names <- vapply(v$args, function(a) a$name, character(1))
    expect_identical(got_names, names(fm), info = paste("argument order for", v$verb))
    for (a in v$args) {
      ## the empty symbol cannot be bound to a name without raising "missing"
      no_default <- is.symbol(fm[[a$name]]) && !nzchar(as.character(fm[[a$name]]))
      expected <- if (no_default) NA_character_ else paste(deparse(fm[[a$name]]), collapse = " ")
      got <- if (is.null(a$default)) NA_character_ else a$default
      expect_identical(got, expected, info = paste0(v$verb, "(", a$name, ") default"))
    }
  }
})

test_that("every argument carries a touch sentence and Rd text", {
  skip_if_no_board()
  b <- read_board()
  for (v in b$verbs) for (a in v$args) {
    expect_true(is.character(a$text) && nzchar(a$text), info = paste0(v$verb, "(", a$name, ") text"))
    if (a$name != "...") expect_true(is.character(a$touch) && nzchar(a$touch), info = paste0(v$verb, "(", a$name, ") touch"))
  }
})

test_that("every source pointer resolves", {
  skip_if_no_board()
  b <- read_board()
  pointers <- character()
  for (v in b$verbs) {
    for (a in v$args) if (!is.null(a$source)) pointers <- c(pointers, a$source)
    for (f in c("mechanics", "records", "refuses", "issues", "notes", "design_ahead")) {
      for (it in v[[f]]) if (!is.null(it$source)) pointers <- c(pointers, it$source)
    }
    for (e in v$evidence) if (!is.null(e$where)) pointers <- c(pointers, e$where)
  }
  for (d in b$divergences) if (!is.null(d$source)) pointers <- c(pointers, d$source)
  bad <- Filter(function(p) !isTRUE(resolves(p)), unique(pointers))
  msgs <- vapply(bad, resolves, character(1))
  expect_length(bad, 0)
  if (length(bad)) message(paste(msgs, collapse = "\n"))
})

test_that("design-ahead entries still name something the code lacks", {
  skip_if_no_board()
  b <- read_board()
  ns <- asNamespace("horizons")
  n <- 0L
  for (v in b$verbs) for (d in v$design_ahead) {
    if (is.null(d$absent)) next
    n <- n + 1L
    fm <- names(formals(get(v$verb, envir = ns)))
    expect_false(d$absent %in% fm,
                 info = paste0(v$verb, ": '", d$absent, "' is now in formals(); remove the design-ahead entry"))
  }
  if (n == 0L) succeed("no design-ahead entries carry an `absent` symbol")
})

test_that("the generated data was built from the current content.yml", {
  skip_if_no_board()
  b <- read_board()
  content <- file.path(pkg_root, "vignettes", "board", "content.yml")
  ## A content hash, not an mtime: git stamps every file's mtime at checkout,
  ## so the mtime comparison failed on any fresh clone or worktree (2026-09-23).
  expect_identical(b$content_md5, unname(tools::md5sum(content)),
                   info = "content.yml changed after the last build; run dev/board/build-board.R")
})
