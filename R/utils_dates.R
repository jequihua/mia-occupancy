#' Round a date to the beginning of its biweek
#'
#' @param x A `Date` vector.
#'
#' @return A `Date` vector.
#' @export
floor_biweek <- function(x) {
  x <- as.Date(x)
  month_start <- lubridate::floor_date(x, unit = "month")
  out <- ifelse(lubridate::day(x) <= 15,
                month_start,
                month_start + lubridate::days(15))
  as.Date(out, origin = "1970-01-01")
}

#' Add a sampling period to a data frame
#'
#' @param data A data frame.
#' @param date_col Name of the date column.
#' @param temporal_scale One of `"week"`, `"biweek"`, or `"month"`.
#' @param output_col Name of the output column.
#'
#' @return The input data frame with a sampling-period column added.
#' @export
add_sampling_period <- function(data,
                                date_col,
                                temporal_scale = c("week", "biweek", "month"),
                                output_col = "sampling_period") {
  temporal_scale <- rlang::arg_match(temporal_scale)
  date_vec <- as.Date(data[[date_col]])

  sampling_period <- switch(
    temporal_scale,
    week = lubridate::floor_date(date_vec, unit = "week", week_start = 1),
    biweek = floor_biweek(date_vec),
    month = lubridate::floor_date(date_vec, unit = "month")
  )

  dplyr::mutate(data, !!output_col := as.Date(sampling_period))
}

#' Build a visit key from unique sampling periods
#'
#' @param sampling_periods A vector of dates.
#' @param prefix A string used to name replicate visits.
#'
#' @return A tibble with `sampling_period` and `visit` columns.
#' @export
build_visit_key <- function(sampling_periods, prefix = "T") {
  tibble::tibble(sampling_period = sort(unique(as.Date(sampling_periods)))) |>
    dplyr::mutate(visit = sprintf("%s%02d", prefix, dplyr::row_number()))
}
