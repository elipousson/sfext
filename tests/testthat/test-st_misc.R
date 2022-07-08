test_that("st_misc functions work", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc_6543 <- sf::st_transform(nc, 6543)

  expect_s3_class(st_square(nc_6543), "sf")
  expect_s3_class(st_square(nc_6543, inscribed = FALSE), "sf")
  expect_s3_class(st_inscribed_square(nc_6543), "sf")
  expect_error(st_inscribed_square("x"))

  expect_s3_class(st_circle(nc_6543, inscribed = TRUE), "sf")
  expect_s3_class(st_circle(nc_6543), "sf")
  expect_s3_class(st_circumscribed_circle(nc_6543), "sf")
})