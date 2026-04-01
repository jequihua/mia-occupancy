#' Count independent detections by species, site, and replicate
#'
#' @param data A detection table.
#' @param site_col Site identifier column.
#' @param species_col Species identifier column.
#' @param period_col Sampling-period column.
#'
#' @return A tibble with one row per species-site-period and an `abundance` column.
#' @export
count_detections <- function(data,
                             site_col = "site_id",
                             species_col = "scientific_name",
                             period_col = "sampling_period") {
  data |>
    dplyr::group_by(.data[[site_col]], .data[[species_col]], .data[[period_col]]) |>
    dplyr::summarise(abundance = dplyr::n(), .groups = "drop")
}

#' Complete the species-site-period grid where effort exists
#'
#' @param detections Count table from `count_detections()`.
#' @param effort Site-period effort table.
#' @param species_ids Optional vector of species to include.
#' @param site_col Site identifier column.
#' @param species_col Species identifier column.
#' @param period_col Sampling-period column.
#'
#' @return A tibble with abundance and binary detection columns.
#' @export
complete_detection_history <- function(detections,
                                       effort,
                                       species_ids = NULL,
                                       site_col = "site_id",
                                       species_col = "scientific_name",
                                       period_col = "sampling_period") {
  if (is.null(species_ids)) {
    species_ids <- sort(unique(detections[[species_col]]))
  }

  effort_grid <- effort |>
    tidyr::crossing(!!rlang::sym(species_col) := species_ids)

  effort_grid |>
    dplyr::left_join(detections, by = c(site_col, period_col, species_col)) |>
    dplyr::mutate(
      abundance = tidyr::replace_na(.data$abundance, 0L),
      detection = as.integer(.data$abundance > 0)
    )
}

#' Build a long detection table with visit identifiers
#'
#' @param completed_history Output of `complete_detection_history()`.
#' @param taxonomy Optional taxonomy table.
#' @param weather_by_period Optional site-period weather summary table.
#' @param period_col Sampling-period column.
#'
#' @return A long-format tibble.
#' @export
build_detection_long <- function(completed_history,
                                 taxonomy = NULL,
                                 weather_by_period = NULL,
                                 period_col = "sampling_period") {
  visit_key <- build_visit_key(completed_history[[period_col]])

  out <- completed_history |>
    dplyr::left_join(visit_key, by = setNames("sampling_period", period_col))

  if (!is.null(taxonomy)) {
    out <- out |>
      dplyr::left_join(taxonomy, by = c("scientific_name" = "species_id"))
  }

  if (!is.null(weather_by_period)) {
    out <- out |>
      dplyr::left_join(weather_by_period, by = c("site_id", period_col))
  }

  out |>
    dplyr::relocate(.data$visit, .after = dplyr::all_of(period_col))
}

#' Pivot a long detection table to a wide table
#'
#' @param detection_long A long detection table.
#' @param value_cols Columns to pivot across visits.
#' @param id_cols Identifier columns to preserve.
#'
#' @return A wide tibble.
#' @export
build_detection_wide <- function(detection_long,
                                 value_cols = c("detection", "abundance"),
                                 id_cols = c("state", "site_id", "scientific_name")) {
  detection_long |>
    tidyr::pivot_wider(
      names_from = .data$visit,
      values_from = dplyr::all_of(value_cols),
      names_glue = "{.value}_{visit}"
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(id_cols)))
}
