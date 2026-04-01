#' Filter detections to a target area
#'
#' @param data A data frame.
#' @param area Value to keep in `area_col`.
#' @param area_col Area column name.
#'
#' @return A filtered tibble.
#' @export
filter_target_area <- function(data, area, area_col = "state") {
  data |>
    dplyr::filter(.data[[area_col]] == area)
}
