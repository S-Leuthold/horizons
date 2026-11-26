## ---------------------------------------------------------------------------
## Tests: the one writer of a file, whole or not at all
## ---------------------------------------------------------------------------

describe("write_file_whole()", {

  it("writes the file whole through the save function and returns NULL", {

    dir  <- withr::local_tempdir()
    path <- file.path(dir, "x.rds")

    expect_null(write_file_whole(list(a = 1), path, save = saveRDS))
    expect_identical(readRDS(path), list(a = 1))
    expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE), "x.rds")

  })

  it("saves under a temporary name beside the target, with the prefix and extension given", {

    dir   <- withr::local_tempdir()
    saved <- NULL

    expect_null(write_file_whole(list(a = 1), file.path(dir, "x.qs2"), tmp_prefix = ".lib-", tmp_ext = ".qs2.tmp",
                                 save = function(value, file) {
                                   saved <<- file
                                   saveRDS(value, file)
                                 }))

    expect_identical(normalizePath(dirname(saved)), normalizePath(dir))
    expect_match(basename(saved), "^\\.lib-.*\\.qs2\\.tmp$")

  })

  it("returns why a save that half-writes and stops failed, leaving nothing behind", {

    dir  <- withr::local_tempdir()
    path <- file.path(dir, "x.rds")

    expect_no_condition(why <- write_file_whole(list(a = 1), path, save = function(value, file) {
      writeLines("half a file", file)
      stop("No space left on device")
    }))

    expect_type(why, "character")
    expect_length(why, 1L)
    expect_match(why, "No space left on device", fixed = TRUE)
    expect_length(list.files(dir, all.files = TRUE, no.. = TRUE), 0L)

  })

  it("returns a warning raised before the save fails ahead of the error, raising neither", {

    dir <- withr::local_tempdir()

    expect_no_condition(why <- write_file_whole(list(a = 1), file.path(dir, "x.rds"), save = function(value, file) {
      warning("cannot open file: No space left on device")
      stop("cannot open the connection")
    }))

    expect_match(why, "No space left on device.*; .*cannot open the connection")
    expect_length(list.files(dir, all.files = TRUE, no.. = TRUE), 0L)

  })

  it("returns NULL for a save that warns and succeeds, since the file is what was asked", {

    dir  <- withr::local_tempdir()
    path <- file.path(dir, "x.rds")

    expect_no_condition(why <- write_file_whole(list(a = 1), path, save = function(value, file) {
      warning("a remark on the way")
      saveRDS(value, file)
    }))

    expect_null(why)
    expect_identical(readRDS(path), list(a = 1))

  })

  it("returns why a rename was refused, leaving no temporary file behind", {

    dir <- withr::local_tempdir()
    dir.create(file.path(dir, "x.rds"))   # a directory where the file should go

    expect_no_condition(why <- write_file_whole(list(a = 1), file.path(dir, "x.rds"), save = saveRDS))
    expect_type(why, "character")
    expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE), "x.rds")

  })

})

describe("the writers of evaluate()'s output directory and of library mode's cache", {

  it("each write through write_file_whole() once, with its own save function and extension", {

    calls <- list()
    local_mocked_bindings(write_file_whole = function(value, path, save, tmp_prefix = "file", tmp_ext = ".tmp") {
      calls[[length(calls) + 1L]] <<- list(path = path, save = save, tmp_prefix = tmp_prefix, tmp_ext = tmp_ext)
      NULL
    })

    dir <- withr::local_tempdir()

    expect_null(write_output_file(list(a = 1), file.path(dir, "x.rds")))
    expect_length(calls, 1L)
    expect_identical(calls[[1]]$save, saveRDS)
    expect_identical(calls[[1]]$tmp_ext, ".rds.tmp")

    expect_null(write_cache_file(list(a = 1), file.path(dir, "cache", "x.qs2"), tmp_prefix = ".space-"))
    expect_length(calls, 2L)
    expect_identical(calls[[2]]$tmp_prefix, ".space-")
    expect_identical(calls[[2]]$tmp_ext, ".qs2.tmp")

    ## The cache's save makes the cache directory, then saves with qs2
    target <- file.path(dir, "cache", "x.qs2")
    calls[[2]]$save(list(a = 1), target)
    expect_identical(qs2::qs_read(target), list(a = 1))

  })

})
