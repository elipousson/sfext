test_that("st_bbox_ext accepts valid parameters and rejects invalid parameters", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

  expect_s3_class(st_bbox_ext(nc, dist = 100), "bbox")
  expect_s3_class(st_bbox_ext(nc, diag_ratio = 0.5), "bbox")
  expect_s3_class(st_bbox_ext(nc, asp = "1:2"), "bbox")
  expect_s3_class(st_bbox_ext(nc, dist = 100, unit = "feet"), "bbox")
  expect_s3_class(st_bbox_asp(nc, asp = 0.25, class = "sf"), "sf")
  expect_error(st_bbox_ext(nc, dist = "1"))
  expect_error(st_bbox_ext(nc, asp = "X"))

  nc_list <- as_sf_list(nc, col = "NAME")

  expect_equal(length(st_bbox_ext(nc_list, dist = -1500)), 100)
  expect_equal(length(st_bbox_asp(nc_list, asp = 1)), 100)
})
