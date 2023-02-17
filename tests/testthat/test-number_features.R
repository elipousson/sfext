test_that("number_features works", {
  nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))

  expect_s3_class(
    number_features(
      nc
    ),
    "sf"
  )
  expect_s3_class(
    number_features(
      nc,
      sort = NULL,
      to = nc[1, ]
    ),
    "sf"
  )
  expect_s3_class(
    number_features(
      nc,
      sort = "dist_xmin_ymax",
      to = NULL
    ),
    "sf"
  )
  expect_equal(
    rlang::has_name(
      number_features(
        nc,
        .id = "feature_num"
      ),
      "feature_num"
    ),
    TRUE
  )
})

test_that("sort_features works", {
  nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))

  sort_lonlat <-
    sort_features(
      nc,
      sort = c("lon", "lat")
    )

  sort_minmax <-
    sort_features(
      nc,
      sort = c("xmin", "ymin")
    )

  expect_s3_class(
    sort_lonlat,
    "sf"
  )
  expect_s3_class(
    sort_minmax,
    "sf"
  )
  expect_false(
    identical(sort_lonlat, nc)
  )
  expect_false(
    identical(sort_minmax, nc)
  )
})
