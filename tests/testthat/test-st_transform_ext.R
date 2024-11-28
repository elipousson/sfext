test_that("st_transform_ext works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

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

  expect_s3_class(
    st_transform_ext(sf::st_drop_geometry(nc)),
    "data.frame"
  )

  # Check transform_sf helper
  expect_identical(
    transform_sf(nc, crs = NULL),
    nc
  )

  expect_identical(
    transform_sf(nc, crs = nc),
    nc
  )

  skip()
  omerc <- st_omerc(nc, 30)
  expect_identical(
    sf::st_crs(omerc)$input,
    "+proj=omerc +lat_0=35.559466717973 +lonc=-79.400416805857\n+datum=WGS84 +units=m +no_defs +gamma=30"
  )
})
