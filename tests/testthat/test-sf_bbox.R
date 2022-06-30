test_that("sf_bbox functions work", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc_bbox <- sf::st_bbox(nc)
  expect_s3_class(sf_bbox_to_sf(nc_bbox), "sf")
  expect_s3_class(sf_bbox_to_sf(nc), "sf")
  expect_match(sf_bbox_to_wkt(nc_bbox), "^POLYGON")
  expect_s3_class(sf_bbox_to_lonlat_query(nc_bbox), "character")
  expect_error(sf_bbox_to_lonlat_query("nc_bbox"))
  # Some functions also work with sf objects directly
  expect_length(sf_bbox_to_wkt(nc), 100)
})
