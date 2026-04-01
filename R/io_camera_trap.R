#' Parse camera-trap timestamps
#'
#' @param data A data frame containing a timestamp column.
#' @param timestamp_col Name of the raw timestamp column.
#' @param output_col Name of the parsed datetime column.
#' @param orders Character vector of lubridate parsing orders.
#' @param tz Time zone for parsed datetimes.
#'
#' @return The input data frame with an added POSIXct datetime column.
#' @export
parse_camera_timestamps <- function(data,
                                    timestamp_col = "timestamp",
                                    output_col = "photo_datetime",
                                    orders = c("dmy HM", "dmy HMS", "ymd HMS", "ymd HM"),
                                    tz = "UTC") {
  parsed <- lubridate::parse_date_time(data[[timestamp_col]], orders = orders, tz = tz)
  dplyr::mutate(data, !!output_col := as.POSIXct(parsed, tz = tz))
}

#' Read and standardize camera-trap detection data
#'
#' @param path Path to a CSV file.
#' @param tz Time zone for parsed datetimes.
#'
#' @return A standardized tibble.
#' @export
read_camera_trap_data <- function(path, tz = "UTC") {
  data <- readr::read_csv(path, show_col_types = FALSE)

  required_cols <- c(
    "State", "camera_name", "Longitude", "Latitude", "class", "order",
    "family", "genus", "species", "common_name", "timestamp"
  )
  validate_required_columns(data, required_cols, data_name = "camera trap data")

  data |>
    dplyr::rename(
      state = .data$State,
      site_id = .data$camera_name,
      longitude = .data$Longitude,
      latitude = .data$Latitude,
      taxon_class = .data$class,
      taxon_order = .data$order,
      taxon_family = .data$family
    ) |>
    parse_camera_timestamps(timestamp_col = "timestamp", output_col = "photo_datetime", tz = tz) |>
    dplyr::mutate(photo_date = as.Date(.data$photo_datetime)) |>
    tibble::as_tibble()
}
