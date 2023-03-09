test_that("is_sf and as_sf functions work", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  sfc <- nc$geometry
  bbox <- sf::st_bbox(nc)

  expect_true(is_bbox(bbox))
  expect_true(is_sf(nc))
  expect_false(is_sf(bbox))
  expect_true(is_sf(bbox, ext = TRUE))

  expect_true(is_sf(NULL, ext = TRUE, null.ok = TRUE))

  expect_true(is_sfc(sfc))
  expect_true(is_sf(sfc, ext = TRUE))

  expect_true(is_sf_list(list(nc)))
  expect_false(is_sf_list(list(nc), named = TRUE))
  expect_true(is_sf_list(list("nc" = nc), named = TRUE))

  expect_s3_class(as_sf(nc), "sf")
  expect_s3_class(as_sf(sfc), "sf")
  expect_s3_class(as_sf(bbox), "sf")

  expect_s3_class(as_sfc(nc), "sfc")
  expect_s3_class(as_sfc(sfc), "sfc")
  expect_s3_class(as_sfc(bbox), "sfc")

  expect_s3_class(as_bbox(nc), "bbox")
  expect_s3_class(as_bbox(sfc), "bbox")
  expect_s3_class(as_bbox(bbox), "bbox")

  nc_col <- nc
  nc_col[["col"]] <- rep(c("A", "B"), 50)
  expect_type(as_sf_list(nc_col, col = "col"), "list")

  expect_s3_class(as_sf_class(nc, class = "sfc"), "sfc")
  expect_s3_class(as_sf_class(nc, class = "bbox"), "bbox")
  expect_type(as_sf_class(nc, class = "list"), "list")
  expect_type(as_sf_class(nc_col, col = "col", class = "list"), "list")
  expect_s3_class(as_sf_class(nc, class = "data.frame"), "data.frame")
  expect_error(as_sf_class(bbox, class = "data.frame"))

  expect_true(is_same_crs(nc, nc))
  expect_true(is_same_crs(nc, bbox))

  expect_true(is_geo_coords(c(-81.47276, 36.23436)))

  null_test <- NULL

  expect_true(is_sf(null_test, null.ok = TRUE))
  expect_true(is_sf_list(null_test, null.ok = TRUE))
  expect_true(is_sp(null_test, null.ok = TRUE))
  expect_true(is_geo_coords(null_test, null.ok = TRUE))
})
