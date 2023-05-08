test_that("as_crs works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  sfc <- nc$geometry
  bbox <- sf::st_bbox(nc)
  expect_identical(
    as_crs(nc),
    sf::st_crs(nc)
  )
  expect_true(is_same_crs(nc, nc))
  expect_true(is_same_crs(nc, sfc))
  expect_true(is_same_crs(nc, bbox))
  expect_true(is_same_crs(nc, "NAD27"))
})
