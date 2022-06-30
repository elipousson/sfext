test_that("st_inscribed_square works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc_6543 <- sf::st_transform(nc, 6543)
  expect_s3_class(st_inscribed_square(nc_6543), "sf")
  expect_error(st_inscribed_square("x"))
})
