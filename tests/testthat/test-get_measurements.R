test_that("get_measurements works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  expect_true(
    is_dist_units(get_area(nc[1:2, ], units = "acres")$area)
  )

  expect_true(
    is_dist_units(get_area(nc[1:2, ], units = "acres", .id = "acreage")$acreage)
  )

  expect_true(
    is_dist_units(get_dist(nc[1, ], to = c("xmax", "ymax"), units = "mile")$dist)
  )

  expect_true(
    is_dist_units(get_dist(nc[1, ], to = nc[30, ], units = "km")$dist)
  )

  nc_crs <- sf::st_crs(nc)

  nc_line <- as_line(c(as_point(nc[1, ]), as_point(nc[30, ])), crs = nc_crs)

  expect_s3_class(
    get_length(nc_line),
    "sf"
  )

  expect_s3_class(
    get_bearing(nc_line),
    "sf"
  )
})
