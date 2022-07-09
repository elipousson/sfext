test_that("st_make_grid_ext works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  expect_s3_class(
    st_make_grid_ext(nc),
    "sf"
  )

  expect_s3_class(
    st_make_grid_ext(nc, cols = 4, rows = 1),
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
})
