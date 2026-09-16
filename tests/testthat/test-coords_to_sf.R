test_that("check_coords works", {
  df <- data.frame(lon = -80.1, lat = 35.1)

  expect_equal(check_coords(df), c("lon", "lat"))
  expect_equal(check_coords(x = NULL), c("lon", "lat"))
  expect_equal(check_coords(x = NULL, default = c("x", "y")), c("x", "y"))
  expect_equal(check_coords(coords = c("lat", "lon"), rev = TRUE), c("lon", "lat"))

  expect_error(check_coords(coords = "lon"))
})

test_that("has_coords works", {
  df <- data.frame(name = "a", lon = -80.1, lat = 35.1)

  expect_equal(has_coords(df), c("lon", "lat"))
  expect_true(has_coords(df, value = FALSE))

  df_long <- data.frame(name = "a", longitude = -80.1, latitude = 35.1)
  expect_equal(has_coords(df_long), c("longitude", "latitude"))

  df_xy <- data.frame(name = "a", x = -80.1, y = 35.1)
  expect_equal(has_coords(df_xy), c("x", "y"))

  df_none <- data.frame(name = "a", value = 1)
  expect_identical(has_coords(df_none), character(0))

  expect_error(has_coords("not a data.frame"))
})

test_that("rev_coords works", {
  expect_equal(rev_coords(c("lat", "lon")), c("lon", "lat"))
  expect_equal(rev_coords(c("lon", "lat")), c("lon", "lat"))
  expect_equal(rev_coords(c("y", "x")), c("x", "y"))
})

test_that("format_coords works", {
  df <- data.frame(lon = c("-80.1", "-80.2"), lat = c("35.1", NA))

  formatted <- format_coords(df)
  expect_type(formatted$lon, "double")
  expect_equal(nrow(formatted), 1)

  expect_equal(nrow(format_coords(df, keep_missing = TRUE)), 2)

  expect_error(format_coords(df, coords = NULL))
  expect_error(format_coords(df, coords = c("lon", "missing")))

  df_allna <- data.frame(lon = NA_character_, lat = NA_character_)
  expect_error(format_coords(df_allna))
})

test_that("separate_coords works", {
  skip_if_not_installed("tidyr")
  skip_if_not_installed("readr")

  df <- data.frame(name = "a", latlon = "35.1,-80.1")

  separated <- separate_coords(df, coords = "latlon", into = c("lat", "lon"))

  expect_true(all(c("lat", "lon") %in% names(separated)))
  expect_equal(separated$lat, 35.1)
  expect_equal(separated$lon, -80.1)
})

test_that("coords_to_sf works", {
  df <- data.frame(name = c("a", "b"), lon = c(-80.1, -80.2), lat = c(35.1, 35.2))

  x <- coords_to_sf(df)
  expect_s3_class(x, "sf")
  expect_equal(nrow(x), 2)

  x_removed <- coords_to_sf(df, remove_coords = TRUE)
  expect_false(any(c("lon", "lat") %in% names(x_removed)))

  df_bad <- data.frame(name = "a", value = 1)
  expect_error(
    coords_to_sf(df_bad)
  )
})
