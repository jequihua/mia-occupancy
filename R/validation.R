#' Validate that required columns exist
#'
#' @param data A data frame.
#' @param required_cols Character vector of required column names.
#' @param data_name Name used in error messages.
#'
#' @return Invisibly returns `TRUE` if validation passes.
#' @export
validate_required_columns <- function(data, required_cols, data_name = deparse(substitute(data))) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "%s is missing required columns: %s",
        data_name,
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Validate a spOccupancy input object
#'
#' @param sp_input A list produced by `build_spoccupancy_input()`.
#'
#' @return Invisibly returns `TRUE` if validation passes.
#' @export
validate_spoccupancy_input <- function(sp_input) {
  required_names <- c("y", "coords")
  missing_names <- setdiff(required_names, names(sp_input))
  if (length(missing_names) > 0) {
    stop(sprintf("Missing top-level objects: %s", paste(missing_names, collapse = ", ")), call. = FALSE)
  }

  y <- sp_input$y
  coords <- sp_input$coords

  if (length(dim(y)) != 3) {
    stop("`y` must be a 3D array with dimensions species x sites x replicates.", call. = FALSE)
  }

  if (!is.matrix(coords) || ncol(coords) != 2) {
    stop("`coords` must be a matrix with two columns (longitude, latitude).", call. = FALSE)
  }

  if (dim(y)[2] != nrow(coords)) {
    stop("The number of sites in `y` must match the number of rows in `coords`.", call. = FALSE)
  }

  if (!is.null(sp_input$det.covs)) {
    det_covs <- sp_input$det.covs
    for (nm in names(det_covs)) {
      if (!all(dim(det_covs[[nm]]) == c(dim(y)[2], dim(y)[3]))) {
        stop(sprintf("Detection covariate '%s' must have dimensions sites x replicates.", nm), call. = FALSE)
      }
    }
  }

  if (!is.null(sp_input$occ.covs)) {
    if (!is.data.frame(sp_input$occ.covs) || nrow(sp_input$occ.covs) != dim(y)[2]) {
      stop("`occ.covs` must be a data frame with one row per site.", call. = FALSE)
    }
  }

  invisible(TRUE)
}
