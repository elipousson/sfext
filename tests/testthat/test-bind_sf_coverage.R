test_that("bind_sf_coverage adds a coverage feature", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  x <- nc[1:3, ]
  x$name <- x$NAME

  coverage <- sf::st_as_sf(
    sf::st_buffer(sf::st_centroid(sf::st_union(x)), 200000)
  )

  result <- suppressMessages(bind_sf_coverage(x, coverage))

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(x) + 1)
  expect_true(all(x$name %in% result$name))

  result_named <- bind_sf_coverage(x, coverage, coverage_nm = "outer")
  expect_equal(tail(result_named$name, 1), "outer")
})

test_that("bind_sf_coverage errors if x is not covered", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  x <- nc[1:3, ]
  x$name <- x$NAME

  far_coverage <- sf::st_as_sf(
    sf::st_buffer(sf::st_centroid(sf::st_geometry(nc[90, ])), 5000)
  )

  expect_error(bind_sf_coverage(x, far_coverage))
})

test_that("bind_sf_coverage validates x and .id", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  x <- nc[1:3, ]

  coverage <- sf::st_as_sf(
    sf::st_buffer(sf::st_centroid(sf::st_union(x)), 200000)
  )

  expect_error(bind_sf_coverage(x, coverage), class = "rlang_error")
  expect_error(bind_sf_coverage("x", coverage), class = "rlang_error")
})

test_that("st_make_valid_union works with and without combine", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  x <- nc[1:3, ]

  expect_s3_class(st_make_valid_union(x), "sfc")
  expect_s3_class(st_make_valid_union(x, combine = TRUE), "sfc")
})

test_that("st_make_valid_coverage returns the difference between x and y", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  x <- nc[1:3, ]

  coverage <- sf::st_as_sf(
    sf::st_buffer(sf::st_centroid(sf::st_union(x)), 200000)
  )

  result <- st_make_valid_coverage(coverage, x)
  expect_s3_class(result, "sfc")
})
