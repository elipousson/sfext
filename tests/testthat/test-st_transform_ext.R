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

  omerc <- st_omerc(nc, 30)
  proj4 <- sf::st_crs(omerc)$input

  expect_s3_class(omerc, "sf")
  expect_match(proj4, "\\+proj=omerc")
  expect_match(proj4, "\\+gamma=30")
  # Skipped: lat_0 depends on the NAD27 -> WGS84 datum shift PROJ picks at
  # runtime (79 candidate operations), which varies by installed grid files
  # and PROJ version, so the exact value is not reproducible across machines.
  # expect_match(proj4, "\\+lat_0=35\\.55946")
  expect_match(proj4, "\\+lonc=-79\\.40041")
})
