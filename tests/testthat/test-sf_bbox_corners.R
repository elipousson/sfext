test_that("sf_bbox_corners works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package="sf"))
  bbox_corners <- sf_bbox_corners(sf::st_bbox(nc[1, ]))
  sf_corners <- sf_bbox_corners(nc[1, ])
  sfc_corners <- sf_bbox_corners(nc[1, ], class = "sfc")

  expect_s3_class(
    bbox_corners,
    "sf"
  )
  expect_identical(
    ncol(bbox_corners),
    2L
  )
  expect_s3_class(
    sf_corners,
    "sf"
  )
  expect_s3_class(
    sfc_corners,
    "sfc"
  )
})

test_that("sf_bbox_corners errors", {
  expect_error(
    sf_bbox_corners("ABC")
  )
})



