test_that("convert_dist_scale works with scale_factor and actual_unit", {
  expect_equal(
    convert_dist_scale(
      dist = 1,
      scale_unit = "in",
      scale_factor = 12,
      actual_unit = "ft"
    ),
    units::set_units(12, "ft")
  )

  expect_equal(
    convert_dist_scale(
      dist = c(1, 2),
      scale_unit = "in",
      scale_factor = 12,
      actual_unit = "ft"
    ),
    units::set_units(c(12, 24), "ft")
  )
})

test_that("convert_dist_scale converts scale_unit before applying scale_factor", {
  expect_equal(
    convert_dist_scale(
      dist = 10,
      scale_unit = "mm",
      scale_factor = 1,
      actual_unit = "cm"
    ),
    units::set_units(1, "cm")
  )

  expect_equal(
    convert_dist_scale(
      dist = 120,
      scale_unit = "px",
      dpi = 120,
      scale_factor = 1,
      actual_unit = "in"
    ),
    units::set_units(1, "in")
  )
})

test_that("convert_dist_scale works with a named scale", {
  dist <- convert_dist_scale(
    dist = 1,
    scale = "1:24,000",
    scale_unit = "in"
  )

  expect_equal(dist, units::set_units(2000, "ft"))
})

test_that("convert_dist_scale errors if scale matches more than 1 row", {
  expect_error(
    convert_dist_scale(
      dist = 1,
      scale = "1:20,000",
      scale_unit = "in"
    )
  )
})

test_that("convert_dist_scale warns if actual_unit is set with scale", {
  expect_warning(
    convert_dist_scale(
      dist = 1,
      scale = "1:24,000",
      scale_unit = "in",
      actual_unit = "mi"
    )
  )
})

test_that("convert_dist_scale works with paper and returns a data.frame", {
  paper <- convert_dist_scale(
    paper = "letter",
    scale = "1:24,000"
  )

  expect_s3_class(paper, "data.frame")
  expect_true(all(c("actual_width", "actual_height", "scale") %in% names(paper)))
  expect_equal(nrow(paper), 1)
})
