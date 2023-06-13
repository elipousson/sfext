test_that("read_sf_ext works with file paths", {

  nc <- read_sf_ext(
    path = system.file("shape/nc.shp", package = "sf")
  )

  expect_s3_class(
    nc,
    "sf"
  )

  expect_s3_class(
    read_sf_ext(
      path = system.file("shape/nc.shp", package = "sf"),
      bbox = sf::st_bbox(nc[1, ])
    ),
    "sf"
  )

  expect_s3_class(
    read_sf_ext(
      path = system.file("shape/nc.shp", package = "sf"),
      bbox = sf_bbox_transform(sf::st_bbox(nc[1, ]), 3857)
    ),
    "sf"
  )

  expect_s3_class(
    read_sf_query(
      dsn = system.file("shape/nc.shp", package = "sf")
    ),
    "sf"
  )
})

test_that("read_sf_ext works with mapbaltimore package", {
  skip_if_not_installed("mapbaltimore")
  expect_s3_class(
    read_sf_ext(
      "trees",
      package = "mapbaltimore"
    ),
    "sf"
  )
})
