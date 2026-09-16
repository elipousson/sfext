test_that("st_dissolve validates arguments", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  x <- nc[1:5, ]

  expect_error(st_dissolve(x, .dissolve_key = 5), class = "rlang_error")
  expect_error(st_dissolve(x, .keep = 1), class = "rlang_error")
  expect_error(st_dissolve(x, do_union = "yes"), class = "rlang_error")

  x_conflict <- x
  x_conflict$group.comp.id <- 1
  expect_error(st_dissolve(x_conflict))
})

test_that("st_dissolve dissolves geometry and nests attributes", {
  skip_if_not_installed("spdep")
  skip_if_not_installed("tidyr")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  x <- nc[1:5, ]

  result <- st_dissolve(x)

  expect_s3_class(result, "sf")
  expect_true("data" %in% names(result))
  expect_true(nrow(result) <= nrow(x))
})

test_that("st_dissolve works with sfc input and .by grouping", {
  skip_if_not_installed("spdep")
  skip_if_not_installed("tidyr")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  x <- nc[1:5, ]

  result_sfc <- st_dissolve(sf::st_geometry(x))
  expect_s3_class(result_sfc, "sfc")

  x$category <- rep(c("A", "B"), length.out = nrow(x))
  result_grouped <- st_dissolve(x, .by = category)
  expect_s3_class(result_grouped, "sf")
  expect_true("category" %in% names(result_grouped))
})

test_that("st_dissolve .keep argument can skip nesting", {
  skip_if_not_installed("spdep")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  x <- nc[1:5, ]

  result <- st_dissolve(x, .keep = "drop")
  expect_s3_class(result, "sf")
  expect_false("data" %in% names(result))
})
