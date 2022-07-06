test_that("st_filter_ext works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc_county <- nc[1, ]
  nc_county_bbox <- st_bbox_ext(nc_county, dist = 5, unit = "mile")

  expect_s3_class(
    st_filter_ext(
      nc,
      nc_county_bbox
    ),
    "sf"
  )

  expect_s3_class(
    st_filter_ext(
      nc,
      nc_county_bbox,
      crop = TRUE
    ),
    "sf"
  )

  expect_s3_class(
    st_filter_ext(
      nc,
      nc_county_bbox,
      trim = TRUE
    ),
    "sf"
  )

  expect_s3_class(
    st_filter_ext(
      nc,
      nc_county_bbox,
      erase = TRUE
    ),
    "sf"
  )

  nc_col <- nc
  nc_col[["col"]] <- rep(c("A", "B"), 50)
  nc_list <- as_sf_list(nc_col, col = "col")

  expect_type(
    st_filter_ext(
      nc_list,
      nc_county_bbox
    ),
    "list"
  )
})
