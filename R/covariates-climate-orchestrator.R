#' Fetch Climate Covariates Using Daymet and Hargreaves PET
#'
#' This function retrieves daily climate data from Daymet for a set of input coordinates and
#' computes key climate covariates using the Hargreaves method for PET (Potential Evapotranspiration).
#' It groups samples by Daymet 4-km grid cell to minimize redundant downloads and summarizes
#' climate variables (MAT, MAP, PET, AI, GDD, Precip_Seasonality) for each unique grid.
#' Loud, graceful failures are implemented using cli-based error handling.
#'
#' @import dplyr
#' @import purrr
#' @import tibble
#' @import lubridate
#' @importFrom cli cli_progress_step cli_alert_success cli_alert_warning cli_abort cli_progress_done
#' @importFrom glue glue
#' @importFrom daymetr download_daymet
#'
#' @param input_data A data frame with columns `Longitude` and `Latitude`. Additional columns (e.g., `Layer_ID`) are preserved for joining.
#' @param start_year The first year of climate data to retrieve (default: 2003).
#' @param end_year The final year of climate data to retrieve (default: 2024).
#' @param gdd_base Base temperature (°C) for Growing Degree Days calculation (default: 10).
#' @param cache_dir Directory for storing Daymet downloads (default: user cache directory).
#'
#' @return A data frame joining input data with climate covariates:
#'   \item{MAT}{Mean annual temperature (°C).}
#'   \item{MAP}{Mean annual precipitation (mm).}
#'   \item{PET}{Mean annual potential evapotranspiration (mm, estimated by Hargreaves).}
#'   \item{AI}{Aridity Index (MAP / PET).}
#'   \item{GDD}{Mean annual Growing Degree Days (base temperature defined by `gdd_base`).}
#'   \item{Precip_Seasonality}{Coefficient of variation (CV%) of monthly precipitation totals.}
#'
#' @details
#' The function uses the Daymet API to download daily climate data for each unique 4-km grid cell
#' represented by the input coordinates. Hargreaves PET is calculated for each day, and
#' annual summaries are averaged across the specified date range.
#' This approach minimizes API calls while ensuring data consistency.
#'
#' @export


fetch_climate_covariates <- function(input_data,
                                     start_year = 2003,
                                     end_year   = 2024,
                                     gdd_base   = 10,
                                     cache_dir  = tools::R_user_dir("horizons", "cache")){

  ## ---------------------------------------------------------------------------
  ## Step 0: Input validation
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Starting to fetch climate covatiate data.")

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
  ## Helper Function 1: Compute grid ID to minmize repeated downloads.
  ## -----------------------------------------------------------------------------

  daymet_resolution_deg <- 1/24

  compute_daymet_grid_id <- function(lon,
                                     lat) {

    grid_lon <- round(lon / daymet_resolution_deg) * daymet_resolution_deg
    grid_lat <- round(lat / daymet_resolution_deg) * daymet_resolution_deg

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
  ## Step 2: Download Daymet data for each grid cell
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Downloading Daymet data for each grid cell.")

  purrr::map(processed_data$Daymet_GridID,
            function(grid_id){

              ## ---------------------------------------------------------------

              processed_data %>%
                filter(Daymet_GridID == grid_id) -> coords

              safely_execute(expr          = {daymetr::download_daymet(lat      = coords$Latitude,
                                                                       lon      = coords$Longitude,
                                                                       start    = start_year,
                                                                       end      = end_year,
                                                                       silent   = TRUE,
                                                                       internal = TRUE)},
                             default_value = NULL,
                             error_message = "Daymet download for {grid_id} failed") -> daymet_data_safe

              daymet_data <- daymet_data_safe$result %>% purrr::pluck(., "data")

              if(is.null(daymet_data)){
                cli::cli_alert_warning("Daymet download for grid ID {grid_id} failed. Skipping this grid cell.")
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

              tibble::tibble(Daymet_GridID      = grid_id,
                             MAT                = daymet_summary$MAT,
                             MAP                = daymet_summary$MAP,
                             PET                = daymet_summary$PET,
                             AI                 = daymet_summary$AI,
                             GDD                = daymet_summary$GDD,
                             Precip_Seasonality = daymet_summary$Precip_Seasonality)
              }
            ) -> daymet_results


  ## ---------------------------------------------------------------------------
  ## Step 3: Return climate data
  ## ---------------------------------------------------------------------------

  climate_summary <- dplyr::bind_rows(daymet_results)

  final_data <- input_data %>%
                  dplyr::left_join(climate_summary,
                                   by = "Daymet_GridID") %>%
                  dplyr::select(-Daymet_GridID)

  cli::cli_progress_done()
  cli::cli_alert_success("Climate data retrieved successfully.")

  return(final_data)
}



