test_that("st_buffer_ext works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package="sf"))
  nc_list <- list(nc[1,], nc[2,], nc[3,])
  expect_true(
    is_sf_list(st_buffer_ext(nc_list, dist = 100))
    )
  expect_s3_class(
    st_buffer_ext(sf::st_bbox(nc), dist = 100),
    "sfc"
  )
  expect_message(
    st_buffer_ext(nc, dist = 100, dist_limits = c(50, 150))
  )
  expect_s3_class(
    st_buffer_ext(sf::st_transform(nc, 4326)),
    "sf"
  )
  expect_s3_class(
    st_buffer_ext(nc, dist = as_dist_units(1, "mile"), unit = "mile"),
    "sf"
  )
})
