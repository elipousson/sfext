test_that("st_filter_pct_area works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  x <- nc[1:3, ]
  y <- sf::st_as_sf(sf::st_buffer(sf::st_centroid(sf::st_union(x)), 60000))

  result <- st_filter_pct_area(x, y)

  expect_s3_class(result, "sf")
  expect_true(all(c("pct_area") %in% names(result)))
  expect_true(all(result$pct_area > 0 & result$pct_area <= 1 + 1e-6))

  filtered <- st_filter_pct_area(x, y, pct = 0.9)
  expect_true(all(filtered$pct_area >= 0.9))
  expect_lte(nrow(filtered), nrow(result))

  expect_error(
    st_filter_pct_area(x, sf::st_centroid(y)),
    class = "rlang_error"
  )

  expect_error(
    st_filter_pct_area(x, y, pct = 2),
    class = "rlang_error"
  )
})

test_that("st_filter_pct_length works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  x <- nc[1:3, ]
  y <- sf::st_as_sf(sf::st_buffer(sf::st_centroid(sf::st_union(x)), 60000))

  centroids <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(x)))
  lines <- sf::st_sfc(
    lapply(seq_len(nrow(x)), function(i) {
      sf::st_linestring(rbind(centroids[i, ], centroids[i, ] + c(20000, 20000)))
    }),
    crs = sf::st_crs(x)
  )
  lines_sf <- sf::st_sf(NAME = x$NAME, geometry = lines)

  result <- st_filter_pct_length(lines_sf, y)

  expect_s3_class(result, "sf")
  expect_true("pct_length" %in% names(result))
  expect_true(all(result$pct_length > 0 & result$pct_length <= 1))

  filtered <- st_filter_pct_length(lines_sf, y, pct = 0.9)
  expect_true(all(filtered$pct_length >= 0.9))

  expect_error(
    st_filter_pct_length(x, y),
    class = "rlang_error"
  )

  expect_error(
    st_filter_pct_length(lines_sf, y, pct = 2),
    class = "rlang_error"
  )
})

test_that("st_filter_pct dispatches on geometry type", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  x <- nc[1:3, ]
  y <- sf::st_as_sf(sf::st_buffer(sf::st_centroid(sf::st_union(x)), 60000))

  expect_s3_class(st_filter_pct(x, y, pct = 0.1), "sf")

  centroids <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(x)))
  lines <- sf::st_sfc(
    lapply(seq_len(nrow(x)), function(i) {
      sf::st_linestring(rbind(centroids[i, ], centroids[i, ] + c(20000, 20000)))
    }),
    crs = sf::st_crs(x)
  )
  lines_sf <- sf::st_sf(NAME = x$NAME, geometry = lines)

  expect_s3_class(st_filter_pct(lines_sf, y, pct = 0.1), "sf")

  expect_error(
    st_filter_pct(suppressWarnings(sf::st_centroid(x)), y),
    class = "rlang_error"
  )
})
