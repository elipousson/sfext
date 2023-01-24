test_that("st_cast_ext works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package="sf"))
  expect_error(
    st_cast_ext(sf::st_bbox(nc))
  )
  expect_s3_class(
    st_cast_ext(nc, to = "MULTIPOINT"),
    "sf"
  )
})
