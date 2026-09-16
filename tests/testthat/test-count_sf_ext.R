test_that("count_sf_ext works with y provided", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  pts <- suppressWarnings(sf::st_sample(nc, size = 30))

  result <- suppressWarnings(count_sf_ext(data = pts, y = nc, .id = "FIPS"))

  expect_s3_class(result, "sf")
  expect_true(all(c("FIPS", "n") %in% names(result)))
  expect_true(all(result$n > 0))
})

test_that("count_sf_ext works with x used to build a grid", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  pts <- suppressWarnings(sf::st_sample(nc, size = 30))

  result <- suppressWarnings(
    count_sf_ext(data = pts, x = nc, .id = "FIPS", geometry = FALSE)
  )

  expect_false(inherits(result, "sf"))
  expect_true(all(c("FIPS", "n") %in% names(result)))
})

test_that("count_sf_ext replace_na and keep_na work", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  pts <- suppressWarnings(sf::st_sample(nc, size = 5))

  result_drop <- suppressWarnings(count_sf_ext(data = pts, y = nc, .id = "FIPS"))
  expect_true(all(!is.na(result_drop$n)))

  result_keep <- suppressWarnings(
    count_sf_ext(data = pts, y = nc, .id = "FIPS", keep_na = TRUE)
  )
  expect_true(nrow(result_keep) >= nrow(result_drop))

  result_replace <- suppressWarnings(
    count_sf_ext(data = pts, y = nc, .id = "FIPS", replace_na = TRUE)
  )
  expect_true(all(!is.na(result_replace$n)))
  expect_equal(nrow(result_replace), nrow(nc))
})

test_that("count_sf_ext applies lims to bin counts", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  pts <- suppressWarnings(sf::st_sample(nc, size = 30))

  result <- suppressWarnings(
    count_sf_ext(data = pts, y = nc, .id = "FIPS", lims = c(1, 2))
  )

  expect_true(max(result$n) <= 2)
  expect_true(min(result$n) >= 1)
})

test_that("count_sf_ext errors with invalid y", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  pts <- suppressWarnings(sf::st_sample(nc, size = 5))

  expect_error(
    count_sf_ext(data = pts, y = nc, .id = "not_a_column"),
    class = "rlang_error"
  )
})
