test_that("get_measurements works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  expect_true(
    is_dist_units(get_area(nc[1:2, ], units = "acres")$area)
  )

  expect_true(
    is_dist_units(get_area(nc[1:2, ], units = "acres", .id = "acreage")$acreage)
  )

  expect_true(
    is_dist_units(
      get_dist(nc[1, ], to = c("xmax", "ymax"), units = "mile")$dist
    )
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
})

test_that("get_length converts POINT geometry to LINESTRING", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  pts <- suppressWarnings(sf::st_centroid(nc[1:3, ]))

  expect_message(get_length(pts), "Converting")

  result <- suppressMessages(get_length(pts))
  expect_s3_class(result, "sf")
  expect_true("length" %in% names(result))
})

test_that("get_length uses lwgeom::st_perimeter for POLYGON geometry", {
  skip_if_not_installed("lwgeom")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc_poly <- sf::st_cast(nc[1:2, ], "POLYGON")

  expect_message(get_length(nc_poly), "perimeter")

  result <- suppressMessages(get_length(nc_poly))
  expect_true("perimeter" %in% names(result))
})

test_that("get_length errors for MULTIPOLYGON input", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc_multi <- sf::st_cast(nc[1:2, ], "MULTIPOLYGON")

  expect_error(get_length(nc_multi))
})

test_that("get_dist works with a sf object and a bbox point", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  result_sf <- get_dist(nc[1, ], to = nc[2, ], units = "km")
  expect_true(is_dist_units(result_sf$dist))

  result_bbox <- get_dist(nc[1, ], to = c("xmax", "ymax"))
  expect_true(is_dist_units(result_bbox$dist))
})

test_that("get_dist_units returns the units for a sf object", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  units <- get_dist_units(nc)

  expect_type(units, "character")
})

test_that("get_bearing works", {
  skip_if_not_installed("lwgeom")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc_crs <- sf::st_crs(nc)

  nc_line <- as_line(c(as_point(nc[1, ]), as_point(nc[30, ])), crs = nc_crs)

  expect_s3_class(
    get_bearing(nc_line),
    "sf"
  )

  expect_true(all(get_bearing(nc_line)$bearing >= 0))
  expect_true(all(get_bearing(nc_line, dir = TRUE)$bearing <= 180))
})

test_that("get_bearing supports a to argument", {
  skip_if_not_installed("geosphere")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  result <- get_bearing(nc[1, ], to = nc[30, ])
  expect_s3_class(result, "sf")
  expect_true("bearing" %in% names(result))
})

test_that("get_bearing converts non-line geometry when to is not supplied", {
  skip_if_not_installed("geosphere")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  pts <- suppressWarnings(sf::st_centroid(nc[1:2, ]))

  expect_message(get_bearing(pts), "Converting")

  result <- suppressMessages(get_bearing(pts))
  expect_s3_class(result, "sf")
  expect_true("bearing" %in% names(result))
})
