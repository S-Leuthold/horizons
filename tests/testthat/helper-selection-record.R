# tests/testthat/helper-selection-record.R
# A synthetic x$selection record, shaped like the one select_training()
# attaches but cheap to build, so the class and subset tests can exercise the
# record without running the verb.


## ---------------------------------------------------------------------------
## make_selection_record() — A selection record over given pool ids
## ---------------------------------------------------------------------------

#' Build a selection record for a set of pool ids
#'
#' @description
#' Every pool id is drawn by one of `n_targets` targets for each property, so
#' `membership` has one row per id per property and `pool_sizes$drawn` is the
#' id count. Groups are formed by splitting the targets evenly, which gives
#' the `groups$pool_ids` / `groups$n_rows` pair something to be recomputed
#' from when rows leave.
#'
#' @param pool_ids [Character.] The ids in the object.
#' @param properties [Character.] Properties drawn for. Default:
#'   `c("clay", "oc")`.
#' @param n_targets [Integer.] Targets in the draw. Default: `4`.
#' @param n_groups [Integer.] Scope groups. Default: `2`.
#'
#' The field list and the table columns track the real record, because a
#' synthetic record that has drifted from it stops being evidence about
#' anything. `test-class-core.R` asserts both.
#'
#' @return [List.] A record with the same top-level names as the one
#'   `select_training()` attaches.
#' @noRd
make_selection_record <- function(pool_ids   = sprintf("P%03d", 1:10),
                                  properties = c("clay", "oc"),
                                  n_targets  = 4L,
                                  n_groups   = 2L) {

  target_ids <- sprintf("T%02d", seq_len(n_targets))

  ## Deal the ids round-robin across the targets ------------------------------

  target_of_id <- target_ids[(seq_along(pool_ids) - 1L) %% n_targets + 1L]
  group_of_id  <- (seq_along(target_ids) - 1L) %% n_groups + 1L
  names(group_of_id) <- target_ids

  membership <- dplyr::bind_rows(lapply(properties, function(p) {

    tibble::tibble(target_id = target_of_id,
                   property  = p,
                   space     = "all",
                   pool_id   = pool_ids,
                   distance  = seq_along(pool_ids) / 100,
                   rank      = seq_along(pool_ids),
                   retained  = TRUE)

  }))

  groups <- dplyr::bind_rows(lapply(sort(unique(group_of_id)), function(g) {

    t_ids <- names(group_of_id)[group_of_id == g]
    p_ids <- pool_ids[target_of_id %in% t_ids]

    tibble::tibble(group      = g,
                   n_targets  = length(t_ids),
                   n_rows     = length(p_ids),
                   target_ids = list(t_ids),
                   pool_ids   = list(p_ids))

  }))

  list(
    settings = list(k              = stats::setNames(rep(3L, length(properties)), properties),
                    scope          = "batch",
                    properties     = properties,
                    space          = "pca",
                    ncomp_retained = 12L,
                    metric         = "mahalanobis",
                    seed           = 1L),

    reconciliation   = list(action = "none"),
    pool             = list(n_rows = length(pool_ids), id_hash = "fixture"),
    membership       = membership,
    groups           = groups,

    pool_sizes       = tibble::tibble(property  = properties,
                                      available = length(pool_ids),
                                      drawn     = length(pool_ids)),

    target_distances = tibble::tibble(target_id = target_ids,
                                      property  = properties[1],
                                      space     = "all",
                                      nearest   = 0.01,
                                      mean_k    = 0.05),

    resemblance      = list(threshold   = 0.2,
                            n_reference = length(pool_ids),
                            beyond      = tibble::tibble(target_id = character(),
                                                         nearest   = numeric())),

    units            = list(pool_median   = 0.4,
                            target_median = 0.4,
                            pool_iqr      = 0.2,
                            target_iqr    = 0.2,
                            basis         = "median and iqr",
                            mismatch      = FALSE),

    exclusions       = tibble::tibble(property           = character(),
                                      target_id          = character(),
                                      pool_id            = character(),
                                      distance           = numeric(),
                                      rank               = integer(),
                                      reference_distance = numeric(),
                                      reason             = character()),

    n_excluded_union = 0L,

    short_draws      = tibble::tibble(target_id   = character(),
                                      property    = character(),
                                      k_requested = integer(),
                                      k_drawn     = integer(),
                                      reason      = character()),

    clustering       = NULL,
    timestamp        = Sys.time()
  )

}


## ---------------------------------------------------------------------------
## make_selected_object() — A horizons_data carrying a selection record
## ---------------------------------------------------------------------------

#' Build a small horizons_data with a selection record attached
#'
#' @description
#' A pool fixture trimmed to `n_rows` rows, with the per-row provenance
#' columns `select_training()` adds and a matching record. Cheap enough to
#' use in print, summary and subset tests.
#'
#' @param n_rows [Integer.] Rows in the object. Default: `12`.
#'
#' @return [horizons_data.] With `$selection` populated.
#' @noRd
make_selected_object <- function(n_rows = 12L) {

  fx  <- make_select_fixture(n_pool = 40)
  ids <- fx$pool$data$analysis$sample_id[seq_len(n_rows)]

  obj <- subset_rows(fx$pool, ids, record = FALSE)

  ## The three per-row meta columns the verb adds -----------------------------

  analysis <- dplyr::bind_cols(
    obj$data$analysis,
    tibble::tibble(.drawn_by     = rep(1L, n_rows),
                   .min_distance = seq_len(n_rows) / 100,
                   .group        = rep(c(1L, 2L), length.out = n_rows))
  )

  role_map <- dplyr::bind_rows(
    obj$data$role_map,
    tibble::tibble(variable = c(".drawn_by", ".min_distance", ".group"), role = "meta")
  )

  obj <- set_analysis(obj, analysis, role_map)

  obj$selection <- make_selection_record(pool_ids = ids)

  obj

}
