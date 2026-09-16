test_that("count_features works with a sf list", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)

  region <- sf::st_as_sf(
    sf::st_buffer(sf::st_centroid(sf::st_union(nc[1:10, ])), 80000)
  )
  region$name <- "in_region"

  result <- count_features(x = nc, y = list("in_region" = region))

  expect_s3_class(result, "sf")
  expect_true(all(c("in_region", "n") %in% names(result)))

  result_drop <- count_features(
    x = nc,
    y = list("in_region" = region),
    geometry = "drop"
  )

  expect_false(inherits(result_drop, "sf"))
  expect_true(all(c("in_region", "n") %in% names(result_drop)))

  result_x <- count_features(
    x = nc,
    y = list("in_region" = region),
    geometry = "x"
  )

  expect_s3_class(result_x, "sf")
})

test_that("count_features works with an existing column and count arg", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  result <- count_features(x = nc, y = NULL, count = "SID74")

  expect_s3_class(result, "sf")
  expect_true(all(c("SID74", "n") %in% names(result)))
})

test_that("count_features errors without count or a valid sf list", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_error(
    count_features(x = nc, y = NULL, nm = NULL),
    class = "rlang_error"
  )
})
