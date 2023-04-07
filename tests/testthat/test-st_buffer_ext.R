test_that("st_buffer_ext works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- st_transform_ext(nc, 2804)
  nc_list <- list(nc[1, ], nc[2, ], nc[3, ])
  nc_line <- as_line(c(as_points(nc[1, ]), as_points(nc[10, ])))
  # sf list input returns sf list
  expect_true(
    is_sf_list(st_buffer_ext(nc_list, dist = 100))
  )
  # bbox input returns sfc
  expect_s3_class(
    st_buffer_ext(sf::st_bbox(nc), dist = 100),
    "sfc"
  )
  # dist set to nearest value from dist_limits
  expect_message(
    st_buffer_ext(nc, dist = 100, dist_limits = c(50, 150))
  )
  # sf with latlon CRS input works
  expect_s3_class(
    st_buffer_ext(sf::st_transform(nc, 4326)),
    "sf"
  )
  expect_s3_class(
    st_buffer_ext(nc, dist = as_dist_units(1, "mile"), unit = "mile"),
    "sf"
  )
  # Use end_style values
  expect_s3_class(
    st_buffer_ext(nc_line,
      dist = as_dist_units(4, "mile"), unit = "mile",
      end_style = "flat"
    ),
    "sf"
  )
  expect_s3_class(
    st_buffer_ext(nc_line,
      dist = as_dist_units(4, "mile"), unit = "mile",
      end_style = "flat"
    ),
    "sf"
  )
})
