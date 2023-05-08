test_that("st_make_grid_ext works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  expect_s3_class(
    st_make_grid_ext(nc),
    "sf"
  )

  expect_s3_class(
    st_make_grid_ext(as_bbox(nc)),
    "sf"
  )

  expect_s3_class(
    st_make_grid_ext(nc, ncol = 4, nrow = 1),
    "sf"
  )

  expect_s3_class(
    st_make_grid_ext(nc, n = 10),
    "sf"
  )

  expect_s3_class(
    st_make_grid_ext(nc, n = 10, style = "square"),
    "sf"
  )

  expect_s3_class(
    st_make_grid_ext(nc, n = 10, style = "circle"),
    "sf"
  )
})
