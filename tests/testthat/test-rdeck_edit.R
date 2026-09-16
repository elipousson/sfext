test_that("rdeck_edit works", {
  skip_if_not_installed("rdeck")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))[1:3, ]

  expect_s3_class(
    rdeck_edit(nc),
    "htmlwidget"
  )

  expect_s3_class(
    rdeck_edit(nc, initial_bounds = sf::st_bbox(st_wgs84(nc))),
    "htmlwidget"
  )
})

test_that("rdeck_edit requires features", {
  skip_if_not_installed("rdeck")

  expect_error(
    rdeck_edit()
  )
})

test_that("rdeck_select works", {
  skip_if_not_installed("rdeck")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))[1:3, ]

  expect_s3_class(
    rdeck_select(nc),
    "htmlwidget"
  )
})

test_that("editor_options works and converts features to WGS84", {
  skip_if_not_installed("rdeck")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))[1:3, ]
  expect_false(is_wgs84(nc))

  opts <- editor_options(mode = "select", features = nc)

  expect_s3_class(opts, "editor_options")
})
