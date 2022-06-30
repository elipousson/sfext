test_that("st_clip works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  # Bad inputs
  expect_error(st_clip(nc, clip = "XX"))
  expect_error(st_clip(nc, keep = "XX"))
  clip_top <- st_clip(sf::st_union(nc), clip = "top")
  keep_bottom <- st_clip(sf::st_union(nc), keep = "bottom")
  expect_s3_class(clip_top, "sf")
  expect_s3_class(keep_bottom, "sf")
})
