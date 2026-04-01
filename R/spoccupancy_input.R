#' Build a site-coordinate matrix
#'
#' @param data A detection table.
#' @param site_col Site identifier column.
#' @param lon_col Longitude column.
#' @param lat_col Latitude column.
#'
#' @return A numeric matrix with row names set to site IDs.
#' @export
build_site_coordinates <- function(data,
                                   site_col = "site_id",
                                   lon_col = "longitude",
                                   lat_col = "latitude") {
  coords <- data |>
    dplyr::distinct(.data[[site_col]], .data[[lon_col]], .data[[lat_col]]) |>
    dplyr::arrange(.data[[site_col]])

  if (any(duplicated(coords[[site_col]]))) {
    stop("Multiple coordinate pairs were found for at least one site.", call. = FALSE)
  }

  mat <- as.matrix(coords[, c(lon_col, lat_col)])
  rownames(mat) <- coords[[site_col]]
  colnames(mat) <- c("X", "Y")
  storage.mode(mat) <- "double"
  mat
}

#' Build the species-by-site-by-replicate detection array
#'
#' @param detection_long A long detection table containing `scientific_name`,
#'   `site_id`, `visit`, and `detection`.
#' @param species_col Species identifier column.
#' @param site_col Site identifier column.
#' @param visit_col Visit identifier column.
#' @param detection_col Detection column.
#'
#' @return A 3D numeric array.
#' @export
build_detection_array <- function(detection_long,
                                  species_col = "scientific_name",
                                  site_col = "site_id",
                                  visit_col = "visit",
                                  detection_col = "detection") {
  species_ids <- sort(unique(detection_long[[species_col]]))
  site_ids <- sort(unique(detection_long[[site_col]]))
  visits <- sort(unique(detection_long[[visit_col]]))

  y <- array(
    data = NA_real_,
    dim = c(length(species_ids), length(site_ids), length(visits)),
    dimnames = list(species_ids, site_ids, visits)
  )

  row_index <- match(detection_long[[species_col]], species_ids)
  col_index <- match(detection_long[[site_col]], site_ids)
  rep_index <- match(detection_long[[visit_col]], visits)

  y[cbind(row_index, col_index, rep_index)] <- detection_long[[detection_col]]
  y
}

#' Split replicate-level covariates into a spOccupancy `det.covs` list
#'
#' @param detection_long A long detection table.
#' @param covariate_cols Character vector of replicate-level covariate names.
#' @param site_col Site identifier column.
#' @param visit_col Visit identifier column.
#'
#' @return A named list of matrices, one per detection covariate.
#' @export
split_det_covs <- function(detection_long,
                           covariate_cols,
                           site_col = "site_id",
                           visit_col = "visit") {
  site_ids <- sort(unique(detection_long[[site_col]]))
  visits <- sort(unique(detection_long[[visit_col]]))

  purrr::map(
    stats::setNames(covariate_cols, covariate_cols),
    function(cov_name) {
      wide <- detection_long |>
        dplyr::distinct(.data[[site_col]], .data[[visit_col]], .data[[cov_name]]) |>
        tidyr::pivot_wider(names_from = .data[[visit_col]], values_from = .data[[cov_name]]) |>
        dplyr::arrange(.data[[site_col]])

      mat <- as.matrix(wide[, visits, drop = FALSE])
      rownames(mat) <- wide[[site_col]]
      storage.mode(mat) <- "double"
      mat[site_ids, visits, drop = FALSE]
    }
  )
}

#' Build the final input list for spOccupancy
#'
#' @param detection_long A long detection-history table.
#' @param raw_site_data Raw or cleaned site-level data used to derive coordinates.
#' @param occ_covs Optional site-level occurrence covariates data frame.
#' @param det_covariate_cols Optional character vector of replicate-level covariate names.
#' @param site_col Site identifier column.
#' @param species_col Species identifier column.
#' @param visit_col Visit identifier column.
#' @param detection_col Detection column.
#' @param lon_col Longitude column in `raw_site_data`.
#' @param lat_col Latitude column in `raw_site_data`.
#'
#' @return A validated `spOccupancy` input list.
#' @export
build_spoccupancy_input <- function(detection_long,
                                    raw_site_data,
                                    occ_covs = NULL,
                                    det_covariate_cols = NULL,
                                    site_col = "site_id",
                                    species_col = "scientific_name",
                                    visit_col = "visit",
                                    detection_col = "detection",
                                    lon_col = "longitude",
                                    lat_col = "latitude") {
  y <- build_detection_array(
    detection_long = detection_long,
    species_col = species_col,
    site_col = site_col,
    visit_col = visit_col,
    detection_col = detection_col
  )

  coords <- build_site_coordinates(
    data = raw_site_data,
    site_col = site_col,
    lon_col = lon_col,
    lat_col = lat_col
  )

  occ_covs_out <- occ_covs
  if (!is.null(occ_covs_out)) {
    occ_covs_out <- occ_covs_out |>
      dplyr::arrange(.data[[site_col]]) |>
      dplyr::select(-dplyr::all_of(site_col))
    occ_covs_out <- as.data.frame(occ_covs_out)
  }

  det_covs_out <- NULL
  if (!is.null(det_covariate_cols) && length(det_covariate_cols) > 0) {
    det_covs_out <- split_det_covs(
      detection_long = detection_long,
      covariate_cols = det_covariate_cols,
      site_col = site_col,
      visit_col = visit_col
    )
  }

  sp_input <- list(
    y = y,
    occ.covs = occ_covs_out,
    det.covs = det_covs_out,
    coords = coords
  )

  validate_spoccupancy_input(sp_input)
  sp_input
}
