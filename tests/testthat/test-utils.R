test_that("floor_biweek returns month start or day 16", {
  dates <- as.Date(c("2024-01-05", "2024-01-20"))
  out <- floor_biweek(dates)
  expect_equal(out, as.Date(c("2024-01-01", "2024-01-16")))
})

test_that("build_visit_key creates ordered labels", {
  periods <- as.Date(c("2024-02-01", "2024-01-01", "2024-02-01"))
  key <- build_visit_key(periods)
  expect_equal(key$visit, c("T01", "T02"))
})
