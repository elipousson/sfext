test_that("transform_sf works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_identical(transform_sf(nc, crs = NULL), nc)
  expect_identical(transform_sf(nc, crs = nc), nc)

  nc_4326 <- transform_sf(nc, crs = 4326)
  expect_identical(sf::st_crs(nc_4326), sf::st_crs(4326))

  nc_sfg <- sf::st_geometry(nc)[[1]]
  expect_s3_class(transform_sf(nc_sfg, crs = 4326), "sfc")

  nc_na_crs <- sf::st_set_crs(nc, NA)
  expect_identical(sf::st_crs(transform_sf(nc_na_crs, crs = 4326)), sf::st_crs(4326))
})

test_that("relocate_sf_col works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- dplyr::relocate(nc, "geometry", .before = "AREA")

  expect_equal(names(nc)[1], "geometry")

  relocated <- relocate_sf_col(nc)
  expect_equal(names(relocated)[length(names(relocated))], "geometry")
})

test_that("rename_sf_col works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  nc_renamed <- rename_sf_col(nc, sf_col = "geom")
  expect_equal(get_sf_col(nc_renamed), "geom")
  expect_true("geom" %in% names(nc_renamed))
})

test_that("get_sf_col works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_equal(get_sf_col(nc), "geometry")
  expect_null(get_sf_col(NULL))
  expect_null(get_sf_col(data.frame(x = 1)))
})

test_that("get_sf_colnames works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_equal(get_sf_colnames(nc), names(nc))

  withr::with_tempdir({
    sf::write_sf(nc, "nc.gpkg")
    expect_equal(get_sf_colnames(dsn = "nc.gpkg"), c(names(nc)[-length(names(nc))], "geom"))
  })
})
