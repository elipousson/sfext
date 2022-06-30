test_that("st_transform_ext works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

  # Transform crs as expected
  expect_identical(
    sf::st_crs(st_transform_ext(x = nc, crs = 4326)),
    sf::st_crs(4326)
  )

  nc_4326 <- sf::st_transform(nc, 4326)

  # Match crs as expected
  expect_identical(
    sf::st_crs(st_transform_ext(x = nc_4326, crs = nc)),
    sf::st_crs(nc)
  )

  # Error if x is not an sf object
  expect_error(
    st_transform_ext(x = "A")
  )
})
