# Example using camera-trap detections only
# This script works without a weather table and builds `y` and `coords`.

library(here)
library(devtools)
load_all(here::here())

camera_path <- get_project_path("..", "20251211_images_2008653_FC.csv")
output_dir <- get_project_path("outputs")

cam_raw <- read_camera_trap_data(camera_path, tz = "UTC")
cam_clean <- cam_raw |>
  build_scientific_name() |>
  filter_identified_records()

cam_events <- define_independent_events(cam_clean, threshold_minutes = 30)
cam_periods <- add_sampling_period(cam_events, date_col = "photo_date", temporal_scale = "month")
cam_periods <- dplyr::filter(cam_periods, lubridate::year(sampling_period) >= 2024)

effort <- infer_sampling_effort(cam_periods)
detection_counts <- count_detections(cam_periods)
taxonomy <- build_taxonomy_table(cam_periods)
completed_history <- complete_detection_history(detection_counts, effort)
detection_long <- build_detection_long(completed_history = completed_history, taxonomy = taxonomy)
sp_input <- build_spoccupancy_input(detection_long = detection_long, raw_site_data = cam_periods)

visit_key <- build_visit_key(detection_long$sampling_period)
detection_wide <- build_detection_wide(detection_long)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(detection_long, file = file.path(output_dir, "camera_only_detection_long.rds"))
saveRDS(detection_wide, file = file.path(output_dir, "camera_only_detection_wide.rds"))
saveRDS(sp_input, file = file.path(output_dir, "camera_only_spoccupancy_input.rds"))
