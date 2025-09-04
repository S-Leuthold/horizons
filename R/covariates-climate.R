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
#' Progress is displayed using `cli::cli_progress_step()` and warnings are surfaced with
#' `cli_alert_warning()`. Failures during download or summarization do not halt execution
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
#' @importFrom cli cli_progress_step cli_alert_success cli_alert_warning cli_abort cli_progress_done
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

  ## ---------------------------------------------------------------------------
  ## Step 0: Input validation
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Starting to fetch climate covariate data.")

  required_cols <- c("Longitude", "Latitude")
  missing_cols <- setdiff(required_cols, colnames(input_data))

  if (length(missing_cols) > 0) {
    cli::cli_abort(glue::glue(
      "Input data is missing required columns:\n  {paste(missing_cols, collapse = ', ')}"
    ))
  }

  ## ---------------------------------------------------------------------------

  if (any(input_data$Longitude < -180 | input_data$Longitude > 180, na.rm = TRUE)) {
    cli::cli_abort("Longitude values must be between -180 and 180 degrees.")
  }

  if (any(input_data$Latitude < -90 | input_data$Latitude > 90, na.rm = TRUE)) {
    cli::cli_abort("Latitude values must be between -90 and 90 degrees.")
  }

  ## ---------------------------------------------------------------------------

  if (any(is.na(input_data$Longitude)) || any(is.na(input_data$Latitude))) {
    cli::cli_abort("Longitude and Latitude columns must not have missing (NA) values.")
  }

  ## ---------------------------------------------------------------------------

  if (!is.numeric(input_data$Longitude) || !is.numeric(input_data$Latitude)) {
    cli::cli_abort("Longitude and Latitude columns must be numeric.")
  }

  ## ---------------------------------------------------------------------------

  if (start_year > end_year) {
    cli::cli_abort("start_year cannot be greater than end_year.")
  }

  ## -----------------------------------------------------------------------------
  ## Constants
  ## -----------------------------------------------------------------------------
  
  DAYMET_RESOLUTION_DEG <- 1/24  # ~4km grid resolution
  DAYMET_TIMEOUT <- 60  # seconds timeout for API calls
  
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

  compute_hargreaves_pet <- function(tmin, tmax, tmean, lat, doy) {

    solar_constant <- 0.0820
    minutes_per_day <- 24 * 60
    rad_to_deg <- pi / 180
    deg_to_rad <- 1 / rad_to_deg
    hargreaves_coef <- 0.0023
    tmean_adjustment <- 17.8
    doy_offset <- 173
    lat_rad <- lat * deg_to_rad


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
    ## ---------------------------------------------------------------------------

    suppressMessages({
      daymet_df %>%
       dplyr::mutate(Tmean = (tmin..deg.c. + tmax..deg.c.) / 2,
                     PET = compute_hargreaves_pet(tmin..deg.c.,
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

     ## ---------------------------------------------------------------------------

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

  cli::cli_progress_step("Grouping input data to shared grid cells.")

  safely_execute(expr = {compute_daymet_grid_id(input_data$Longitude,
                                                input_data$Latitude)},
                 default_value = NULL,
                 error_message = "Failed to compute input grid ID.") -> grid_ids_safe

  grid_ids <- grid_ids_safe$result

  if(is.null(grid_ids)){
    return(NULL)
    cli::cli_abort(c("Failed to compute grid IDs for input data. Please check the input coordinates."))
  }

  ## ---------------------------------------------------------------------------

  input_data %>%
    dplyr::mutate(Daymet_GridID = grid_ids)  -> input_data

  input_data %>%
    dplyr::group_by(Daymet_GridID) %>%
    dplyr::summarise(Longitude = mean(Longitude, na.rm = TRUE),
                     Latitude  = mean(Latitude, na.rm = TRUE)) -> processed_data

  ## ---------------------------------------------------------------------------
  ## Step 2: Download Daymet data for each grid cell (with caching)
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Processing Daymet data for {length(unique(processed_data$Daymet_GridID))} unique grid cells.")
  
  # Create cache directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  failed_grids <- c()

  purrr::map(processed_data$Daymet_GridID,
            function(grid_id){

              ## ---------------------------------------------------------------
              ## Check cache first
              ## ---------------------------------------------------------------
              
              cache_file <- file.path(cache_dir, paste0("daymet_", grid_id, "_", start_year, "_", end_year, ".qs"))
              
              if (file.exists(cache_file) && !refresh) {
                cli::cli_alert_info("Using cached data for grid ID {grid_id}")
                return(qs::qread(cache_file))
              }
              
              ## ---------------------------------------------------------------
              ## Download if not cached
              ## ---------------------------------------------------------------

              processed_data %>%
                filter(Daymet_GridID == grid_id) -> coords

              safely_execute(expr          = {
                                              # Add timeout using R.utils if available
                                              if (requireNamespace("R.utils", quietly = TRUE)) {
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
                             default_value = NULL,
                             error_message = "Daymet download for {grid_id} failed") -> daymet_data_safe

              daymet_data <- daymet_data_safe$result %>% purrr::pluck(., "data")

              if(is.null(daymet_data)){
                cli::cli_alert_danger("Failed to download Daymet data for grid ID {grid_id} (lat: {round(coords$Latitude, 2)}, lon: {round(coords$Longitude, 2)})")
                failed_grids <<- c(failed_grids, grid_id)
                return(NULL)
              }

              ## ---------------------------------------------------------------

              safely_execute(expr          = {summarize_daymet(daymet_df = daymet_data,
                                                               lat       = coords$Latitude,
                                                               gdd_base  = gdd_base)},
                             default_value = NULL,
                             error_message = "Failed to summarize Daymet data for grid ID {grid_id}") -> daymet_summary_safe

              daymet_summary <- daymet_summary_safe$result

              if(is.null(daymet_summary)){
                cli::cli_alert_warning("Failed to summarize Daymet data for grid ID {grid_id}. Skipping.")
                return(NULL)
              }

              ## ---------------------------------------------------------------
              ## Create result and cache it
              ## ---------------------------------------------------------------
              
              result <- tibble::tibble(Daymet_GridID      = grid_id,
                                      MAT                = daymet_summary$MAT,
                                      MAP                = daymet_summary$MAP,
                                      PET                = daymet_summary$PET,
                                      AI                 = daymet_summary$AI,
                                      GDD                = daymet_summary$GDD,
                                      Precip_Seasonality = daymet_summary$Precip_Seasonality)
              
              # Save to cache
              safely_execute(
                expr = qs::qsave(result, cache_file),
                error_message = "Failed to cache climate data for grid {grid_id}"
              )
              
              result
              }
            ) -> daymet_results


  ## ---------------------------------------------------------------------------
  ## Step 3: Return climate data (with error reporting)
  ## ---------------------------------------------------------------------------

  climate_summary <- dplyr::bind_rows(daymet_results)
  
  # Report any failures
  if (length(failed_grids) > 0) {
    unique_failed <- unique(failed_grids)
    cli::cli_alert_warning("Failed to retrieve climate data for {length(unique_failed)} grid cell{?s}: {unique_failed}")
    
    # Check how many samples are affected
    affected_samples <- input_data %>%
      filter(Daymet_GridID %in% unique_failed) %>%
      nrow()
    
    if (affected_samples > 0) {
      cli::cli_alert_danger("{affected_samples} sample{?s} will have missing climate data")
    }
  }

  final_data <- input_data %>%
                  dplyr::left_join(climate_summary,
                                   by = "Daymet_GridID") %>%
                  dplyr::select(-Daymet_GridID)

  cli::cli_progress_done()
  
  # Final success/warning message
  if (length(failed_grids) == 0) {
    cli::cli_alert_success("Climate data retrieved successfully for all {nrow(input_data)} samples.")
  } else {
    successful_samples <- nrow(input_data) - sum(is.na(final_data$MAT))
    cli::cli_alert_warning("Climate data retrieved for {successful_samples}/{nrow(input_data)} samples. See warnings above for details.")
  }

  return(final_data)
}



