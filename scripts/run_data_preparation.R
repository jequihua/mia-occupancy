# End-to-end project workflow template

library(here)
library(devtools)
load_all(here::here())

# -----------------------------------------------------------------------------
# 1. Paths
# -----------------------------------------------------------------------------
camera_path <- get_project_path("data-raw", "20251211_images_2008653_FC.csv")
weather_path <- get_project_path("data-raw", "datos_climatológicos.xlsx")
output_dir <- get_project_path("outputs")

# -----------------------------------------------------------------------------
# 2. Read and standardize inputs
# -----------------------------------------------------------------------------
cam_raw <- read_camera_trap_data(camera_path, tz = "UTC")
cam_clean <- cam_raw |>
  build_scientific_name() |>
  filter_identified_records()
head(cam_clean)
# Optional area filter
# cam_clean <- filter_target_area(cam_clean, area = "Yucatan")

cam_events <- define_independent_events(cam_clean, threshold_minutes = 30)
cam_periods <- add_sampling_period(cam_events, date_col = "photo_date", temporal_scale = "month")
cam_periods <- dplyr::filter(cam_periods, lubridate::year(sampling_period) >= 2024)

effort <- infer_sampling_effort(cam_periods)
detection_counts <- count_detections(cam_periods)
taxonomy <- build_taxonomy_table(cam_periods)
completed_history <- complete_detection_history(detection_counts, effort)

# -----------------------------------------------------------------------------
# 3. Optional detection covariates from weather
# -----------------------------------------------------------------------------
weather_covariates <- c(
  "temp_median", "humidity_median", "dew_point_median", "wind_speed_median",
  "wind_chill_median", "heat_index_median", "bar_median", "rain_median",
  "rain_rate_median", "solar_rad_median", "solar_energy_median", "et_median"
)

weather_long <- read_weather_data(weather_path)
weather_periods <- aggregate_weather_covariates(
  weather_data = weather_long,
  covariate_cols = weather_covariates,
  temporal_scale = "month",
  min_year = 2024
)

# -----------------------------------------------------------------------------
# 4. Build long and wide detection histories
# -----------------------------------------------------------------------------
detection_long <- build_detection_long(
  completed_history = completed_history,
  taxonomy = taxonomy,
  weather_by_period = weather_periods
)

visit_key <- build_visit_key(detection_long$sampling_period)
detection_wide <- build_detection_wide(
  detection_long,
  value_cols = c("detection", "abundance", weather_covariates)
)

# -----------------------------------------------------------------------------
# 5. Build spOccupancy input
# -----------------------------------------------------------------------------
sp_input <- build_spoccupancy_input(
  detection_long = detection_long,
  raw_site_data = cam_periods,
  det_covariate_cols = weather_covariates
)

# -----------------------------------------------------------------------------
# 6. Save outputs
# -----------------------------------------------------------------------------
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(detection_long, file = file.path(output_dir, "detection_long.rds"))
saveRDS(detection_wide, file = file.path(output_dir, "detection_wide.rds"))
saveRDS(sp_input, file = file.path(output_dir, "spoccupancy_input.rds"))

if (requireNamespace("openxlsx", quietly = TRUE)) {
  write_detection_history_excel(
    detection_wide = detection_wide,
    visit_key = visit_key,
    path = file.path(output_dir, "detection_history.xlsx")
  )
}
