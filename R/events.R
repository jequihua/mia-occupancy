#' Define independent detection events
#'
#' @param data A detection table.
#' @param site_col Site identifier column.
#' @param species_col Species identifier column.
#' @param datetime_col Datetime column.
#' @param threshold_minutes Independence threshold in minutes.
#'
#' @return One row per independent event, retaining the first record in each event.
#' @export
define_independent_events <- function(data,
                                      site_col = "site_id",
                                      species_col = "scientific_name",
                                      datetime_col = "photo_datetime",
                                      threshold_minutes = 30) {
  data |>
    dplyr::arrange(.data[[site_col]], .data[[species_col]], .data[[datetime_col]]) |>
    dplyr::group_by(.data[[site_col]], .data[[species_col]]) |>
    dplyr::mutate(
      dt_minutes = as.numeric(difftime(.data[[datetime_col]], dplyr::lag(.data[[datetime_col]]), units = "mins")),
      is_new_event = dplyr::if_else(is.na(.data$dt_minutes) | .data$dt_minutes > threshold_minutes, 1L, 0L),
      event_id = cumsum(.data$is_new_event)
    ) |>
    dplyr::distinct(.data[[site_col]], .data[[species_col]], .data$event_id, .keep_all = TRUE) |>
    dplyr::ungroup()
}
