test_that("as_sf works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_s3_class(as_sf(nc), "sf")
  expect_s3_class(as_sf(sf::st_bbox(nc)), "sf")
  expect_s3_class(as_sf(sf::st_geometry(nc)[[1]]), "sf")
  expect_s3_class(as_sf(sf::st_geometry(nc)), "sf")

  df <- data.frame(lon = -80.1, lat = 35.1)
  expect_s3_class(as_sf(df), "sf")
})

test_that("as_bbox works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_s3_class(as_bbox(nc), "bbox")
  expect_s3_class(as_bbox(sf::st_bbox(nc)), "bbox")

  named_num <- c(xmin = -1, ymin = -1, xmax = 1, ymax = 1)
  bbox <- as_bbox(named_num)
  expect_s3_class(bbox, "bbox")
  expect_equal(as.numeric(bbox), as.numeric(named_num))

  unnamed_num <- c(-1, -1, 1, 1)
  expect_equal(as.numeric(as_bbox(unnamed_num)), unnamed_num)

  pt <- as_points(nc[1, ])
  expect_s3_class(as_bbox(pt), "bbox")

  expect_error(as_bbox(list(1, 2)))
})

test_that("as_sfc works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_s3_class(as_sfc(nc), "sfc")
  expect_s3_class(as_sfc(sf::st_geometry(nc)[[1]]), "sfc")
  expect_identical(as_sfc(sf::st_geometry(nc)), sf::st_geometry(nc))

  df <- data.frame(lon = -80.1, lat = 35.1)
  expect_s3_class(as_sfc(df), "sfc")
})

test_that("as_sf_class works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_identical(as_sf_class(nc, class = NULL), nc)
  expect_s3_class(as_sf_class(nc, class = "bbox"), "bbox")
  expect_s3_class(as_sf_class(nc, class = "sfc"), "sfc")
  expect_s3_class(as_sf_class(sf::st_geometry(nc), class = "sf"), "sf")
  expect_s3_class(as_sf_class(nc, class = "data.frame"), "data.frame")

  expect_error(as_sf_class(nc, class = "invalid"))
  expect_error(as_sf_class(sf::st_geometry(nc), class = "data.frame"))
})
