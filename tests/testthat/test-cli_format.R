test_that("cli_format.sf formats sf objects", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  txt <- cli_format.sf(nc)

  expect_type(txt, "character")
  expect_match(txt, "100 features")
  expect_match(txt, "14 fields")
  expect_match(txt, "MULTIPOLYGON")
  expect_match(txt, "EPSG:4267")
})

test_that("cli_format.sf handles missing CRS", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_set_crs(nc, NA)

  txt <- cli_format.sf(nc)

  expect_false(grepl("in `", txt))
})
