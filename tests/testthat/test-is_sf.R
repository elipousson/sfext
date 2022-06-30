test_that("is_sf and as_sf functions work", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  sfc <- nc$geometry
  bbox <- sf::st_bbox(nc)

  expect_true(is_bbox(bbox))
  expect_true(is_sf(nc))
  expect_false(is_sf(bbox))
  expect_true(is_sf(bbox, ext = TRUE))

  null_ok <- NULL
  expect_true(is_sf(null_ok, ext = TRUE, null.ok = TRUE))

  expect_true(is_sfc(sfc))
  expect_true(is_sf(sfc, ext = TRUE))

  expect_true(is_sf_list(list(nc)))
  expect_false(is_sf_list(list(nc), named = TRUE))
  expect_true(is_sf_list(list("nc" = nc), named = TRUE))
  expect_s3_class(as_sf(nc), "sf")
  expect_s3_class(as_sf(bbox), "sf")
  expect_s3_class(as_sfc(bbox), "sfc")
  expect_s3_class(as_bbox(bbox), "bbox")
  expect_s3_class(as_bbox(nc), "bbox")
  expect_s3_class(as_bbox(sfc), "bbox")
  expect_true(is_same_crs(nc, nc))
  expect_true(is_same_crs(nc, bbox))
})
