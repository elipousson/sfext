test_that("sf_bbox functions work", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc_bbox <- sf::st_bbox(nc)

  expect_s3_class(sf_bbox_to_sf(nc_bbox), "sf")
  expect_s3_class(sf_bbox_to_sf(nc), "sf")

  expect_equal(
    sf::st_crs(sf_bbox_transform(nc_bbox, crs = 3857)),
    sf::st_crs(sf::st_transform(nc, 3857))
  )

  expect_s3_class(sf_bbox_expand(nc_bbox, 50, 50), "bbox")
  expect_s3_class(sf_bbox_contract(nc_bbox, 50, 50), "bbox")
  expect_s3_class(sf_bbox_shift(nc_bbox, c(0, 10), c(0, 10)), "bbox")

  expect_type(sf_bbox_to_lonlat_query(nc_bbox), "character")
  expect_type(sf_bbox_to_lonlat_query(nc_bbox, coords = c("lat", "lon")), "character")
  expect_error(sf_bbox_to_lonlat_query("nc_bbox"))

  expect_match(sf_bbox_to_wkt(nc_bbox), "^POLYGON")
  expect_length(sf_bbox_to_wkt(nc), 100)

  expect_s3_class(sf_bbox_point(nc_bbox, point = c("xmid", "ymid"), crs = NA), "sfg")
  expect_s3_class(sf_bbox_point(nc_bbox, point = c("xmid", "ymid")), "sfc")
  expect_identical(
    sf_bbox_point(nc_bbox, point = c("xmax", "ymax")),
    sf_bbox_point(nc_bbox, point = c("ymax", "xmax"))
  )
})
