#' Build a canonical scientific name
#'
#' @param data A data frame.
#' @param genus_col Genus column name.
#' @param species_col Species column name.
#' @param output_col Output scientific-name column.
#'
#' @return The input data frame with an added scientific-name column.
#' @export
build_scientific_name <- function(data,
                                  genus_col = "genus",
                                  species_col = "species",
                                  output_col = "scientific_name") {
  scientific_name <- stringr::str_trim(paste(data[[genus_col]], data[[species_col]]))
  scientific_name <- dplyr::na_if(scientific_name, "NA NA")
  scientific_name <- dplyr::na_if(scientific_name, "NA")
  dplyr::mutate(data, !!output_col := scientific_name)
}

#' Remove records without usable species-level identification
#'
#' @param data A data frame.
#' @param scientific_name_col Scientific-name column.
#' @param species_col Species epithet column.
#' @param excluded_species Character vector of values to discard.
#'
#' @return A filtered tibble.
#' @export
filter_identified_records <- function(data,
                                      scientific_name_col = "scientific_name",
                                      species_col = "species",
                                      excluded_species = c("No CV Result")) {
  data |>
    dplyr::filter(!is.na(.data[[species_col]])) |>
    dplyr::filter(!(.data[[species_col]] %in% excluded_species)) |>
    dplyr::filter(!is.na(.data[[scientific_name_col]]))
}

#' Build a taxonomy table for modeled species
#'
#' @param detections A detection table.
#' @param species_col Species identifier column.
#'
#' @return A taxonomy tibble with one row per species.
#' @export
build_taxonomy_table <- function(detections, species_col = "scientific_name") {
  detections |>
    dplyr::mutate(species_id = .data[[species_col]]) |>
    dplyr::distinct(
      .data$species_id,
      .data$common_name,
      .data$taxon_class,
      .data$taxon_order,
      .data$taxon_family,
      .data$genus,
      .data$species
    ) |>
    dplyr::arrange(.data$species_id)
}
