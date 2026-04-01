#' Infer sampling effort from available records
#'
#' This infers effort as site-by-period combinations with at least one retained
#' record. Use with caution when no independent deployment-effort table exists.
#'
#' @param data A detection table containing `site_id` and `sampling_period`.
#' @param site_col Site identifier column.
#' @param period_col Sampling-period column.
#' @param area_cols Optional area/grouping columns to preserve.
#'
#' @return A tibble with one row per active site-period.
#' @export
infer_sampling_effort <- function(data,
                                  site_col = "site_id",
                                  period_col = "sampling_period",
                                  area_cols = c("state")) {
  keep_cols <- unique(c(area_cols, site_col, period_col))
  data |>
    dplyr::select(dplyr::all_of(keep_cols)) |>
    dplyr::distinct() |>
    dplyr::arrange(.data[[site_col]], .data[[period_col]])
}
