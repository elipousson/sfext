test_that("as_point works", {
  # Check numeric inputs
  expect_true(is_point(as_point(c(0, 1))))
  expect_true(is_point(as_points(c(0, 1), c(1, 0))))
  expect_true(is_multipoint(as_points(c(0, 1), c(1, 0), to = "MULTIPOINT")))
  # Check crs parameter
  expect_true(is.na(sf::st_crs(as_points(c(0, 1), c(1, 0), to = "MULTIPOINT"))))
  expect_true(
    !is.na(sf::st_crs(as_points(
      c(0, 1),
      c(1, 0),
      crs = 4326,
      to = "MULTIPOINT"
    )))
  )

  # Check sf inputs and outputs
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
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
  expect_true(is_line(as_lines(
    c(nc_pt_1, nc_pt_2),
    c(nc_pt_2, nc_pt_1),
    crs = nc_crs
  )))

  expect_s3_class(as_centroid(as_bbox(nc)), "sfc")
})

test_that("as_polygons works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_true(is_polygon(as_polygons(nc[1, ])))
  expect_true(is_polygon(as_polygons(sf::st_geometry(nc)[1])))
})

test_that("as_xy works", {
  expect_equal(as_xy(x = c(0, 1)), data.frame(x = 0, y = 1))

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  xy <- as_xy(x = c("xmin", "ymax"), bbox = as_bbox(nc))

  expect_equal(names(xy), c("x", "y"))
  expect_equal(nrow(xy), 1)
})

test_that("as_startpoint and as_endpoint work", {
  skip_if_not_installed("lwgeom")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc_line <- as_line(c(as_points(nc[1, ]), as_points(nc[10, ])))

  expect_s3_class(as_startpoint(nc_line), "sfc")
  expect_s3_class(as_endpoint(nc_line), "sfc")

  expect_error(as_startpoint(nc[1, ]))
  expect_error(as_endpoint(nc[1, ]))
})
