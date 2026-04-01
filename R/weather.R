#' Read weather covariates
#'
#' @param path Path to a CSV file.
#' @param site_col Raw site column name.
#' @param datetime_col Raw datetime column name.
#' @param tz Time zone for parsed datetimes.
#'
#' @return A standardized tibble.
#' @export

read_weather_data <- function(path,
                              site_col = "codigo_del_sitio",
                              datetime_col = "weather_ts",
                              tz = "UTC") {
  #data <- readr::read_csv(path, show_col_types = FALSE)
  data <- readxl::read_excel(path)
  
  validate_required_columns(data, c(site_col, datetime_col), data_name = "weather data")

  data |>
    dplyr::rename(site_id = !!rlang::sym(site_col)) |>
    dplyr::mutate(
      weather_datetime = as.POSIXct(lubridate::parse_date_time(.data[[datetime_col]],
                                                               orders = c("dmy HM", "dmy HMS", "ymd HMS", "ymd HM"),
                                                               tz = tz)),
      weather_date = as.Date(.data$weather_datetime)
    ) |>
    dplyr::filter(!is.na(.data$weather_date)) |>
    tibble::as_tibble()
}

#' Aggregate weather covariates to the replicate scale
#'
#' @param weather_data A standardized weather table.
#' @param covariate_cols Character vector of weather covariate names.
#' @param temporal_scale One of `"week"`, `"biweek"`, or `"month"`.
#' @param min_year Minimum year to retain.
#' @param summary_fun Summary function used within site-period.
#'
#' @return A tibble with one row per site-period.
#' @export
aggregate_weather_covariates <- function(weather_data,
                                         covariate_cols,
                                         temporal_scale = c("week", "biweek", "month"),
                                         min_year = NULL,
                                         summary_fun = stats::median) {
  temporal_scale <- rlang::arg_match(temporal_scale)
  validate_required_columns(weather_data, c("site_id", "weather_date"), data_name = "weather_data")
  validate_required_columns(weather_data, covariate_cols, data_name = "weather_data")

  out <- weather_data |>
    add_sampling_period(date_col = "weather_date", temporal_scale = temporal_scale, output_col = "sampling_period")

  if (!is.null(min_year)) {
    out <- out |>
      dplyr::filter(lubridate::year(.data$sampling_period) >= min_year)
  }

  out |>
    dplyr::group_by(.data$site_id, .data$sampling_period) |>
    dplyr::summarise(
      n_weather_days = dplyr::n(),
      dplyr::across(dplyr::all_of(covariate_cols), ~ summary_fun(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}
