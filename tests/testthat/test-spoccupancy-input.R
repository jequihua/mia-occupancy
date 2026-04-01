test_that("build_detection_array returns species x sites x replicates array", {
  x <- tibble::tibble(
    scientific_name = c("A a", "A a", "B b", "B b"),
    site_id = c("S1", "S1", "S1", "S1"),
    visit = c("T01", "T02", "T01", "T02"),
    detection = c(1, 0, 0, 1)
  )

  y <- build_detection_array(x)
  expect_equal(dim(y), c(2, 1, 2))
  expect_equal(y["A a", "S1", "T01"], 1)
  expect_equal(y["B b", "S1", "T02"], 1)
})
