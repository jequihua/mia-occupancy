# mia-occupancy

A package-style modular R project for preparing camera-trap detections for Bayesian occupancy modelling with `spOccupancy`.

## Project purpose

Under the ManglarIA mexican monitoring project there is the need to model species distributions in natural protected areas based on camera-trap data. 

This project offers a codebase built around small, single-purpose functions. The main goal is to convert raw camera-trap detections into:

- modular intermediate tables for inspection and QC,
- replicate-level detection histories,
- and the final `spOccupancy` input list with `y`, `occ.covs`, `det.covs`, and `coords`.

Note: this will eventually be packaged in a Rocker file for deployment and use on google cloud. 

## Design choices

We adopted a package-style structure because it offers the best balance between readability, flexibility, and clean code:

- **One task per function**: importing, filtering, event definition, effort inference, detection-history completion, and final object assembly are separated.
- **Portable paths with `here`**: orchestration scripts use `here::here()` via `get_project_path()` so the project can be moved without rewriting paths.
- **English naming throughout**: all functions, objects, and comments are in English, even though the original script was in Spanish.
- **Model-first outputs**: besides wide detection-history tables, the project produces the exact list structure expected by `spOccupancy`.
- **Validation first**: helper checks are included to catch missing columns and malformed final inputs early.
- **Weather data optional**: the current uploaded camera-trap CSV can build `y` and `coords`, while detection covariates can be added later from a separate weather table.

## Folder structure

```text
camoccupancyPrep/
├─ DESCRIPTION
├─ NAMESPACE
├─ README.md
├─ R/
├─ scripts/
├─ tests/testthat/
├─ data-raw/
└─ outputs/
```
Note: this project assumes all input data are located in the data-raw folder

## Workflow layers

1. **Input standardization**
   - `read_camera_trap_data()`
   - `read_weather_data()`
2. **Taxonomic cleaning**
   - `build_scientific_name()`
   - `filter_identified_records()`
3. **Survey design**
   - `define_independent_events()`
   - `add_sampling_period()`
   - `infer_sampling_effort()`
4. **Detection-history construction**
   - `count_detections()`
   - `complete_detection_history()`
   - `build_detection_long()`
   - `build_detection_wide()`
5. **spOccupancy assembly**
   - `build_detection_array()`
   - `build_site_coordinates()`
   - `split_det_covs()`
   - `build_spoccupancy_input()`

## Path handling

Best practice here is:

- package functions accept explicit paths or already-loaded data,
- project scripts use `get_project_path()` / `here::here()` to locate files,
- no absolute local-machine paths are hard-coded.

## Notes on effort

As stands, the survey effort is inferred from site-period combinations where at least one retained record exists. That assumption is preserved here as a modular step in `infer_sampling_effort()`, but it should be replaced by true deployment-effort metadata whenever available.

## Run

Open `scripts/run_data_preparation.R` to see the end-to-end workflow template, and `scripts/run_camera_only_example.R` for a camera-only example using a CSV with animal detections.
