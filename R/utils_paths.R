#' Build project-relative paths with `here`
#'
#' This helper keeps all project scripts portable. Package functions are written to
#' accept explicit file paths, while orchestration scripts use `here::here()` via
#' this helper.
#'
#' @param ... Path components passed to `here::here()`.
#'
#' @return A single character path.
#' @export
get_project_path <- function(...) {
  if (!requireNamespace("here", quietly = TRUE)) {
    stop("Package 'here' is required for get_project_path().", call. = FALSE)
  }
  here::here(...)
}
