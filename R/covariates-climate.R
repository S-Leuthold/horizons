#' Retrieve and Summarize Climate Covariates from Daymet
#'
#' Fetches daily gridded climate data from the Daymet API for a set of geographic coordinates,
#' then computes key annual covariates including temperature, precipitation, PET (Hargreaves),
#' aridity index, growing degree days, and precipitation seasonality. Samples are grouped by
#' Daymet's 4-km grid resolution to minimize redundant downloads. The function is fault-tolerant
#' and provides progress updates and warnings via the `cli` package.
#'
#' @param input_data A `data.frame` or `tibble` with numeric `Longitude` and `Latitude` columns (in decimal degrees).
#'   Other columns (e.g., `Sample_ID`, `Layer_ID`) are preserved and joined to the output.
#' @param start_year Integer. First year of daily climate data to download (inclusive). Defaults to `2003`.
#' @param end_year Integer. Final year of daily climate data to download (inclusive). Defaults to `2024`.
#' @param gdd_base Numeric. Base temperature (°C) used to compute growing degree days (GDD). Defaults to `10`.
#' @param cache_dir File path. Directory where Daymet data will be cached as `.qs` files.
#'   Defaults to `tools::R_user_dir("horizons", "cache")`.
#' @param refresh Logical. If TRUE, forces re-download of data even if cached. Defaults to FALSE.
#'
#' @return A `tibble` with one row per input sample, containing the original columns plus six additional climate covariates:
#' \itemize{
#'   \item \strong{MAT}: Mean annual temperature (°C)
#'   \item \strong{MAP}: Mean annual precipitation (mm)
#'   \item \strong{PET}: Mean annual potential evapotranspiration (mm), estimated using the Hargreaves method
#'   \item \strong{AI}: Aridity Index (MAP / PET)
#'   \item \strong{GDD}: Mean annual Growing Degree Days (base defined by `gdd_base`)
#'   \item \strong{Precip_Seasonality}: Coefficient of variation (CV%) of monthly precipitation totals
#' }
#'
#' @details
#' To reduce redundant API calls, samples are aggregated to the nearest Daymet 4-km grid cell using
#' rounded coordinate matching. Climate data is then downloaded once per grid cell and summarized
#' annually over the specified date range. PET is calculated daily using the Hargreaves method.
#' Precipitation seasonality is quantified as the coefficient of variation in monthly means.
#'
#' Progress is displayed using tree-style formatting and warnings are surfaced with
#' tree structure indicators. Failures during download or summarization do not halt execution
#' but may result in missing covariates for affected locations.
#'
#' @examples
#' \dontrun{
#' fetch_climate_covariates(
#'   input_data = my_soil_data,
#'   start_year = 2010,
#'   end_year = 2020,
#'   gdd_base = 5
#' )
#' }
#'
#' @importFrom dplyr mutate group_by ungroup summarize select left_join bind_rows filter
#' @importFrom purrr map pluck
#' @importFrom tibble tibble
#' @importFrom lubridate month
#' @importFrom cli cli_text cli_abort
#' @importFrom glue glue
#' @importFrom daymetr download_daymet
#' @importFrom qs qsave qread
#' @export



fetch_climate_covariates <- function(input_data,
                                     start_year = 2003,
                                     end_year   = 2024,
                                     gdd_base   = 10,
                                     cache_dir  = tools::R_user_dir("horizons", "cache"),
                                     refresh    = FALSE){

  # TODO: Add a verbose option to control progress output.

  ## ---------------------------------------------------------------------------
  ## Step 0: Define helper functions
  ## ---------------------------------------------------------------------------

      ## -----------------------------------------------------------------------------
      ## Helper Function 1: Compute grid ID to minmize repeated downloads.
      ## -----------------------------------------------------------------------------

      compute_daymet_grid_id <- function(lon,
                                         lat) {

        grid_lon <- round(lon / DAYMET_RESOLUTION_DEG) * DAYMET_RESOLUTION_DEG
        grid_lat <- round(lat / DAYMET_RESOLUTION_DEG) * DAYMET_RESOLUTION_DEG

        paste0(grid_lon, "_", grid_lat)

      }

      ## -----------------------------------------------------------------------------
      ## Helper Function 2: Calculate Hargreaves PET given Tmin/max, DOY, and Lat
      ## -----------------------------------------------------------------------------

      compute_hargreaves_pet <- function(tmin,
                                         tmax,
                                         tmean,
                                         lat,
                                         doy) {

        solar_constant   <- 0.0820
        minutes_per_day  <- 24 * 60
        rad_to_deg       <- pi / 180
        deg_to_rad       <- 1 / rad_to_deg
        hargreaves_coef  <- 0.0023
        tmean_adjustment <- 17.8
        doy_offset       <- 173
        lat_rad          <- lat * deg_to_rad


        suppressWarnings({

          Ra <- solar_constant * (minutes_per_day / pi) * (acos(-tan(lat_rad) * tan(0.409 * cos(2 * pi * (doy - doy_offset) / 365))))

        })

        pet <- hargreaves_coef * (tmean + tmean_adjustment) * sqrt(pmax(tmax - tmin, 0)) * Ra

        pmax(pet, 0)

      }

      ## -----------------------------------------------------------------------------
      ## Helper Function 3: Summarize Daymet data for a given grid cell
      ## -----------------------------------------------------------------------------

      summarize_daymet <- function(daymet_df,
                                   lat,
                                   gdd_base = 10) {

        suppressMessages({

          ## Calculate summary annual values ---------------------------------------

          daymet_df %>%
           dplyr::mutate(Tmean = (tmin..deg.c. + tmax..deg.c.) / 2,
                         PET   = compute_hargreaves_pet(tmin..deg.c.,
                                                        tmax..deg.c.,
                                                        Tmean,
                                                        lat,
                                                        yday)) %>%
           dplyr::group_by(year) %>%
           dplyr::summarize(MAT = mean(Tmean, na.rm = TRUE),
                            MAP = sum(prcp..mm.day., na.rm = TRUE),
                            PET = mean(PET, na.rm = TRUE),
                            GDD = sum(pmax(Tmean - gdd_base, 0), na.rm = TRUE)) %>%
           dplyr::ungroup() %>%
           dplyr::summarize(MAT = mean(MAT, na.rm = TRUE),
                            MAP = mean(MAP, na.rm = TRUE),
                            PET = mean(PET, na.rm = TRUE),
                            GDD = mean(GDD, na.rm = TRUE)) %>%
           dplyr::mutate(AI = MAP / PET) -> annual_averages

         ## Calculate precip seasonality -------------------------------------------

         daymet_df %>%
           dplyr::mutate(date  = as.Date(paste0(year, "-", yday),
                                         format = "%Y-%j"),
                         month = lubridate::month(date)) %>%
           dplyr::group_by(year,
                           month) %>%
           dplyr::summarize(monthly_prcp = sum(prcp..mm.day., na.rm = TRUE)) %>%
           dplyr::ungroup() %>%
           dplyr::group_by(month) %>%
           dplyr::summarize(monthly_mean = mean(monthly_prcp, na.rm = TRUE)) %>%
           dplyr::summarize(precip_cv = sd(monthly_mean) / mean(monthly_mean) * 100) %>%
           dplyr::pull(precip_cv) -> precip_cv

         dplyr::bind_cols(annual_averages, tibble::tibble(Precip_Seasonality = precip_cv))

         }
        )
      }

  ## ---------------------------------------------------------------------------
  ## Step 1: Get grid cell IDs
  ## ---------------------------------------------------------------------------

  cli::cli_text("├─ Grouping input data to shared grid cells")

  safely_execute(expr = {compute_daymet_grid_id(input_data$Longitude,
                                                input_data$Latitude)},
                 default_value = NULL,
                 error_message = "Failed to compute input grid ID.") -> grid_ids_safe

  handle_results(safe_result   = grid_ids_safe,
                 error_title   = "Failed to compute Daymet grid IDs:",
                 error_hints   = c("Check that Longitude and Latitude are valid numeric columns",
                                   "Ensure coordinates are within Daymet coverage area (North America)",
                                   "Verify coordinate system is WGS84 (EPSG:4326)"),
                 abort_on_null = TRUE,
                 silent        = FALSE) -> grid_ids

  input_data %>%
    dplyr::mutate(Daymet_GridID = grid_ids)  -> input_data

  input_data %>%
    dplyr::group_by(Daymet_GridID) %>%
    dplyr::summarise(Longitude = mean(Longitude, na.rm = TRUE),
                     Latitude  = mean(Latitude, na.rm = TRUE)) -> processed_data

  ## ---------------------------------------------------------------------------
  ## Step 2: Download Daymet data for each grid cell (with caching)
  ## ---------------------------------------------------------------------------

  cli::cli_text("├─ Processing Daymet data for {length(unique(processed_data$Daymet_GridID))} unique grid cells")

  ## Look for and optionally create a cache directory -----------------------------

  if (!dir.exists(cache_dir)) {

    dir.create(cache_dir, recursive = TRUE)

  }

  purrr::map(processed_data$Daymet_GridID,
             function(grid_id){

              ## Check cache first ---------------------------------------------

              cache_file <- file.path(cache_dir, paste0("daymet_", grid_id, "_", start_year, "_", end_year, ".qs"))

              if (file.exists(cache_file) && !refresh) {

                cli::cli_text("│  ├─ Using cached data for grid ID {grid_id}")
                return(qs::qread(cache_file))

              }

              ## Download if not cached ----------------------------------------

              processed_data %>%
                filter(Daymet_GridID == grid_id) -> coords

              safely_execute(expr = {if(requireNamespace("R.utils", quietly = TRUE)) {

                                        R.utils::withTimeout({
                                            daymetr::download_daymet(lat      = coords$Latitude,
                                                                     lon      = coords$Longitude,
                                                                     start    = start_year,
                                                                     end      = end_year,
                                                                     silent   = TRUE,
                                                                     internal = TRUE)
                                          }, timeout = DAYMET_TIMEOUT)

                                        } else {

                                            daymetr::download_daymet(lat      = coords$Latitude,
                                                                     lon      = coords$Longitude,
                                                                     start    = start_year,
                                                                     end      = end_year,
                                                                     silent   = TRUE,
                                                                     internal = TRUE)
                                        }
                                      },

                             default_value      = NULL,
                             error_message      = "Daymet download for {grid_id} failed",
                             capture_conditions = TRUE,
                             capture_trace      = FALSE) -> daymet_data_safe

              handle_results(safe_result   = daymet_data_safe,
                             error_title   = glue::glue("Failed to download Daymet climate data for grid {grid_id}:"),
                             error_hints   = c("Check internet connection to Daymet servers",
                                               "Daymet coverage: Continental US, Canada, Mexico, Hawaii, Puerto Rico",
                                               "Check if year range {start_year}-{end_year} is available (1980-present)",
                                               "Timeout may need adjustment (current: {DAYMET_TIMEOUT}s)"),
                             abort_on_null = TRUE,
                             silent        = FALSE) -> daymet_data

              ## Summarize the data --------------------------------------------

              daymet_df <- if (is.list(daymet_data) && "data" %in% names(daymet_data)) {
                daymet_data$data
              } else {
                daymet_data
              }

              safely_execute(expr = {summarize_daymet(daymet_df = daymet_df,
                                                      lat       = coords$Latitude,
                                                      gdd_base  = gdd_base)},
                             default_value      = NULL,
                             error_message      = "Failed to summarize Daymet data for grid ID {grid_id}",
                             capture_conditions = TRUE) -> daymet_summary_safe


              handle_results(safe_result   = daymet_summary_safe,
                             error_title   = glue::glue("Failed to summarize Daymet data for grid ID {grid_id}:"),
                             error_hints   = NULL,
                             abort_on_null = FALSE,
                             silent        = FALSE) -> daymet_summary


              ## Create result and cache it ------------------------------------


              if(is.null(daymet_summary)){

                tibble::tibble(Daymet_GridID      = grid_id,
                               MAT                = NA_real_,
                               MAP                = NA_real_,
                               PET                = NA_real_,
                               AI                 = NA_real_,
                               GDD                = NA_real_,
                               Precip_Seasonality = NA_real_) -> result

                cli::cli_text("│  ├─ Climate data unavailable for grid {grid_id}.")

              } else {

                tibble::tibble(Daymet_GridID      = grid_id,
                               MAT                = daymet_summary$MAT,
                               MAP                = daymet_summary$MAP,
                               PET                = daymet_summary$PET,
                               AI                 = daymet_summary$AI,
                               GDD                = daymet_summary$GDD,
                               Precip_Seasonality = daymet_summary$Precip_Seasonality) -> result

                tryCatch({

                  qs::qsave(result, cache_file)
                  cli::cli_text("│  ├─ Cached climate data for grid {grid_id}.")

                }, error = function(e) {

                  cli::cli_text("│  ├─ Failed to cache climate data for grid {grid_id}: {e$message}.")

                })
              }

              result
            }) -> daymet_results


  ## ---------------------------------------------------------------------------
  ## Step 3: Return climate data (with error reporting)
  ## ---------------------------------------------------------------------------

  dplyr::bind_rows(daymet_results) -> climate_summary

  ## Join climate data back to original input ---------------------------------

  input_data %>%
    dplyr::left_join(climate_summary,
                     by = "Daymet_GridID") %>%
    dplyr::select(-Daymet_GridID) -> final_data

  ## Final summary message -----------------------------------------------------

  n_successful <- sum(!is.na(final_data$MAT))
  n_total      <- nrow(final_data)

  if (n_successful == n_total) {

    cli::cli_text("└─ ✓ Climate data retrieved successfully for all {n_total} samples.")

  } else {

    n_failed <- n_total - n_successful

    cli::cli_text("├─ Climate data retrieved for {n_successful}/{n_total} samples ({n_failed} failed).")
    cli::cli_text("└─ Failed samples have NA values in climate columns.")

  }

  return(final_data)

}



