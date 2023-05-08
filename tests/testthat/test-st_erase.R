test_that("st_erase works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)

  expect_s3_class(st_erase(nc, nc[2:3, ]), "sf")
  expect_s3_class(st_erase(nc, nc[2:3, ], flip = TRUE), "sf")
  expect_s3_class(st_erase(sf::st_bbox(nc), nc[2:3, ]), "sfc")
})

test_that("st_trim works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)

  expect_s3_class(st_trim(nc, nc[2:3, ]), "sf")
})
