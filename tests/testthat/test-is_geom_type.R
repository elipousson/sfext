test_that("is_geom_type works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

  expect_identical(
    is_geom_type(nc, ext = FALSE),
    sf::st_geometry_type(nc, by_geometry = FALSE)
  )

  expect_identical(
    names(is_geom_type(nc)),
    c("TYPES", "POINTS", "POLYGONS", "LINESTRINGS", "COLLECTION", "OTHER")
  )
})
