## ---------------------------------------------------------------------------
## write_file_whole() — the one writer of a file, whole or not at all
## ---------------------------------------------------------------------------

#' Write a file whole or not at all
#'
#' @description
#' The one mechanism behind every R object horizons saves for itself:
#' [write_output_file()] for `evaluate()`'s `output_dir`, with `saveRDS()`,
#' and [write_cache_file()] for library mode's cache, with qs2. Library
#' mode's downloads and their decompression write their files directly. The
#' value is saved under a temporary name beside the target, since a rename
#' across filesystems is not atomic, and renamed into place, so a reader never
#' sees half a file.
#'
#' A save or a rename that fails leaves no temporary file behind, and what
#' was said about it is returned rather than raised, for the caller to report
#' as its store requires. Base R says why a write failed in a warning raised
#' before the error ("No space left on device", then "cannot open the
#' connection"), so every warning raised during the save and the rename is
#' collected for the reason. A warning raised by a save that then succeeds is
#' dropped: the file saved and renamed into place is what was asked.
#'
#' A process killed during the save runs no exit handler, so its temporary
#' file stays. The stores read only their own names, so it is never taken for
#' a finished file, and the callers clear what such a write leaves where they
#' can: `evaluate()`'s in `tmp/` ([clear_run_files()]), and a library's in the
#' cache once it is a day old ([clear_stale_builds()]).
#'
#' @param value The object to save.
#' @param path [Character(1).] The file to write.
#' @param save [Function.] Called as `save(value, file)`; writes `value` to
#'   `file`.
#' @param tmp_prefix,tmp_ext [Character(1).] The temporary file's name starts
#'   with `tmp_prefix` and ends with `tmp_ext`, so the store can tell it from
#'   its own files.
#' @return `NULL` when the file was written; otherwise Character(1), why it
#'   could not be: each distinct warning and error raised during the save and
#'   the rename, joined by `"; "`, or `"the file could not be renamed into
#'   place"` when the rename failed and nothing was said.
#' @keywords internal
#' @noRd
write_file_whole <- function(value, path, save, tmp_prefix = "file", tmp_ext = ".tmp") {

  tmp     <- tempfile(tmp_prefix, tmpdir = dirname(path), fileext = tmp_ext)
  reasons <- character(0)

  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)

  written <- tryCatch(
    withCallingHandlers(
      {
        save(value, tmp)
        file.rename(tmp, path)
      },
      warning = function(w) {
        reasons <<- c(reasons, condition_summary(w))
        rlang::cnd_muffle(w)
      }
    ),
    error = function(e) {
      reasons <<- c(reasons, condition_summary(e))
      FALSE
    }
  )

  if (isTRUE(written)) return(NULL)

  if (length(reasons) == 0) reasons <- "the file could not be renamed into place"

  paste(unique(reasons), collapse = "; ")

}
