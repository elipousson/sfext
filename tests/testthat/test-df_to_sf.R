test_that("df_to_sf works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc_df <- sf_to_df(nc)
  nc_sf <- df_to_sf(x = nc_df, coords = c("lon", "lat"))
  expect_s3_class(nc_sf, "sf")
  nc_df$xy <- paste(nc_df$lon, nc_df$lat, sep = ",")
  nc_sf_xy <- df_to_sf(nc_df, coords = "xy", into = c("lon", "lat"))
  expect_equal(sf::st_bbox(nc_sf), sf::st_bbox(nc_sf_xy))
})

test_that("df_to_sf works with a wkt column", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  wkt_df <- sf_to_df(nc[1:2, ], geometry = "wkt")

  nc_sf <- df_to_sf(wkt_df, coords = "wkt")
  expect_s3_class(nc_sf, "sf")
  expect_equal(nrow(nc_sf), 2)
})

test_that("df_to_sf works with a geometry column", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc_df <- as.data.frame(nc)

  nc_sf <- df_to_sf(nc_df)
  expect_s3_class(nc_sf, "sf")
})

test_that("df_to_sf works by joining to a sf object with y", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc_data <- data.frame(NAME = nc$NAME[1:3], extra = 1:3)

  joined <- df_to_sf(nc_data, y = nc, by = "NAME")
  expect_s3_class(joined, "sf")
  expect_equal(nrow(joined), 3)
})

test_that("df_to_sf errors for non-data.frame input", {
  expect_error(df_to_sf(1:5))
})
