test_that("st_scale_rotate works with sf, sfc, and bbox objects", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)

  expect_s3_class(st_scale_rotate(nc, scale = 0.75, rotate = 15), "sf")
  expect_s3_class(
    st_scale_rotate(sf::st_geometry(nc), scale = 0.75, rotate = 15),
    "sfc"
  )
  expect_s3_class(
    st_scale_rotate(sf::st_bbox(nc), scale = 0.5),
    "bbox"
  )
})

test_that("st_scale_rotate returns x unchanged if scale is 1 and rotate is 0", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  geometry <- sf::st_geometry(nc)

  expect_identical(st_scale_rotate(geometry), geometry)
})

test_that("st_scale_rotate scaling changes the bounding box size", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)
  geometry <- sf::st_union(sf::st_geometry(nc[1, ]))

  scaled <- st_scale_rotate(geometry, scale = 2)
  bbox_orig <- sf::st_bbox(geometry)
  bbox_scaled <- sf::st_bbox(scaled)

  orig_width <- bbox_orig[["xmax"]] - bbox_orig[["xmin"]]
  scaled_width <- bbox_scaled[["xmax"]] - bbox_scaled[["xmin"]]

  expect_equal(scaled_width, orig_width * 2, tolerance = 1e-6)
})

test_that("st_scale_rotate validates scale and rotate", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)

  expect_error(st_scale_rotate(nc, scale = -1), class = "rlang_error")
  expect_error(st_scale_rotate(nc, rotate = "a"), class = "rlang_error")
})
