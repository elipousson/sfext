test_that("sf_to_df works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_s3_class(
    sf_to_df(x = nc, coords = c("lon", "lat")),
    "data.frame"
  )

  expect_error(
    sf_to_df(x = sf::st_bbox(nc), coords = c("lon", "lat"))
  )
})

test_that("sf_to_df works with geometry options", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  dropped <- sf_to_df(nc, geometry = "drop")
  expect_false(any(c("lon", "lat", "wkt") %in% names(dropped)))
  expect_false(is_sf(dropped))

  wkt <- sf_to_df(nc, geometry = "wkt")
  expect_true("wkt" %in% names(wkt))
  expect_type(wkt$wkt, "character")

  surface_pt <- sf_to_df(nc, geometry = "surface point")
  expect_true(all(c("lon", "lat") %in% names(surface_pt)))

  expect_error(sf_to_df(nc, geometry = "invalid"))
})
