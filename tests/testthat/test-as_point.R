test_that("as_point works", {
  # Check numeric inputs
  expect_true(is_point(as_point(c(0, 1))))
  expect_true(is_point(as_points(c(0, 1), c(1, 0))))
  expect_true(is_multipoint(as_points(c(0, 1), c(1, 0), to = "MULTIPOINT")))
  # Check crs parameter
  expect_true(is.na(sf::st_crs(as_points(c(0, 1), c(1, 0), to = "MULTIPOINT"))))
  expect_true(!is.na(sf::st_crs(as_points(c(0, 1), c(1, 0), crs = 4326, to = "MULTIPOINT"))))

  # Check sf inputs and outputs
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc_crs <- sf::st_crs(nc)

  expect_true(is_point(as_point(nc)))
  expect_true(is_sfg(as_point(nc)))
  expect_true(is_point(as_points(nc)))
  expect_true(is_sfc(as_points(nc)))

  nc_pt_1 <- as_points(nc[1, ])
  nc_pt_2 <- as_points(nc[2, ])

  expect_true(is_line(as_line(nc_pt_1, nc_pt_2)))
  # FIXME: Should two points produce two lines with as_lines?
  expect_true(is_line(as_lines(nc_pt_1, nc_pt_2, crs = nc_crs)))
  # FIXME: If as_lines is provided with sfg and sfc objects it returns a difficult to interpret error
  expect_true(is_line(as_lines(c(nc_pt_1, nc_pt_2), c(nc_pt_2, nc_pt_1), crs = nc_crs)))
})
