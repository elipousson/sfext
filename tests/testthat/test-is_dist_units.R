test_that("is_dist_units works", {
  expect_true(is_dist_units(units::as_units(5, "mi")))
  expect_true(is_dist_units(units::as_units(5, "acres")))
  expect_false(is_dist_units(5))
  expect_false(is_dist_units("mi"))
})

test_that("get_dist_units works", {
  expect_null(get_dist_units(NULL))
  expect_equal(get_dist_units("miles"), "miles")
  expect_equal(get_dist_units(units::as_units(5, "mi")), "mi")
  expect_equal(get_dist_units(units::as_units(5, "acres")), "acres")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  expect_equal(get_dist_units(nc), sf::st_crs(nc)$units_gdal)

  expect_null(get_dist_units(5, quiet = TRUE))
  expect_warning(get_dist_units(5, quiet = FALSE))

  expect_error(get_dist_units(TRUE))
  expect_error(get_dist_units(NULL, allow_null = FALSE))
})

test_that("as_dist_units works", {
  expect_equal(as_dist_units(5, units = "mi"), units::as_units(5, "mi"))
  expect_equal(as_dist_units(5, units = NULL, allow_null = TRUE), 5)
  expect_error(as_dist_units(5, units = NULL, allow_null = FALSE))
})

test_that("is_diff_dist works", {
  d1 <- units::as_units(5, "mi")
  d2 <- units::as_units(3, "mi")

  expect_equal(is_diff_dist(d1, d2), units::as_units(-2, "mi"))
  expect_equal(is_diff_dist(5, 3, units = "mi"), units::as_units(-2, "mi"))
  expect_error(is_diff_dist(5, 3))
})

test_that("is_longer and is_shorter work", {
  d1 <- units::as_units(5, "mi")
  d2 <- units::as_units(3, "mi")

  expect_true(is_longer(d1, d2))
  expect_false(is_shorter(d1, d2))
  expect_false(is_longer(d2, d1))
  expect_true(is_shorter(d2, d1))
})

test_that("is_same_dist works", {
  d1 <- units::as_units(5, "mi")
  d2 <- units::as_units(3, "mi")

  expect_true(isTRUE(is_same_dist(d1, d1)))
  expect_false(isTRUE(is_same_dist(d1, d2)))
  expect_equal(is_same_dist(d1, d2, diff = TRUE), units::as_units(-2, "mi"))

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 6543)
  expect_true(isTRUE(is_same_dist(nc, nc, dist = "xdist")))
})

test_that("is_diff_area and is_same_area work", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  diff_combined <- is_diff_area(nc[1, ], nc[2, ])
  expect_true(is_units(diff_combined))

  diff_uncombined <- is_diff_area(nc[1, ], nc[2, ], combine = FALSE)
  expect_true(is_units(diff_uncombined))

  expect_true(isTRUE(is_same_area(nc[1, ], nc[1, ])))
  expect_false(isTRUE(is_same_area(nc[1, ], nc[2, ])))
  expect_true(is_units(is_same_area(nc[1, ], nc[2, ], diff = TRUE)))
})

test_that("is_same_units works", {
  expect_true(is_same_units("mi", "mi"))
  expect_true(is_same_units("mi", "mile"))
  expect_false(is_same_units("mi", "km"))

  expect_true(
    is_same_units(units::as_units(5, "mi/h"), units::as_units(3, "mi/h"))
  )
  expect_false(
    is_same_units(units::as_units(5, "mi/h"), units::as_units(3, "km/h"))
  )

  expect_false(is_same_units(NULL, "mi"))
  expect_false(is_same_units("mi", NULL))
  expect_false(is_same_units(NULL, NULL))
})
