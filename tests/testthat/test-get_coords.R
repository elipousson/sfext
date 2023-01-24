test_that("get_minmax works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package="sf"))

  expect_s3_class(
    get_minmax(nc[1,], drop = FALSE),
    "sf"
    )
  expect_s3_class(
    get_minmax(nc[1,], drop = TRUE),
    "tbl_df"
  )
  expect_s3_class(
    get_minmax(sf::st_bbox(nc[1,]), keep_all = TRUE),
    "data.frame"
  )
})
