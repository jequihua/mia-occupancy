#' Export a wide detection history and visit key to Excel
#'
#' @param detection_wide A wide detection-history table.
#' @param visit_key A visit-key table.
#' @param path Output xlsx path.
#'
#' @return Invisibly returns the output path.
#' @export
write_detection_history_excel <- function(detection_wide, visit_key, path) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required to export Excel files.", call. = FALSE)
  }

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "detection_history")
  openxlsx::writeData(wb, "detection_history", detection_wide)
  openxlsx::addWorksheet(wb, "visit_key")
  openxlsx::writeData(wb, "visit_key", visit_key)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}
