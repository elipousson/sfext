test_that("st_join_ext works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc[["category"]] <- sample(c("A", "B", "C"), nrow(nc), TRUE)

  expect_s3_class(
    st_join_ext(nc, nc[1, ]),
    "sf"
  )
  expect_s3_class(
    st_join_ext(nc, nc, col = "category", .id = "NAME"),
    "sf"
  )
})

test_that("st_join_ext works with a bbox x", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)

  result <- st_join_ext(sf::st_bbox(nc[1:3, ]), nc[1, ])

  expect_s3_class(result, "sf")
})

test_that("st_join_ext accepts an explicit join function", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  pts <- sf::st_as_sf(sf::st_sample(nc, 5))

  result <- st_join_ext(pts, nc[, "NAME"], join = sf::st_nearest_feature)

  expect_s3_class(result, "sf")
  expect_true("NAME" %in% names(result))
})

test_that("st_join_ext defaults to st_intersects for polygon y", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  pts <- sf::st_as_sf(sf::st_sample(nc, 5))

  result <- suppressWarnings(st_join_ext(pts, nc))

  expect_s3_class(result, "sf")
  expect_true("NAME" %in% names(result))
})

test_that("st_join_ext errors if a sf list entry is missing .id", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)

  y_missing_id <- list(a = nc[1:2, c("NAME")], b = nc[3:4, "AREA"])

  expect_error(
    st_join_ext(nc, y_missing_id, .id = "NAME"),
    class = "rlang_error"
  )
})
