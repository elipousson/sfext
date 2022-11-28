test_that("get_scale works", {
  expect_identical(
    nrow(get_scale(scale = "1:20,000")),
    1L
  )

  expect_identical(
    nrow(get_scale(standard = "Engineering")),
    6L
  )

  expect_identical(
    nrow(get_scale(series = "Antarctica Maps")),
    2L
  )
})
