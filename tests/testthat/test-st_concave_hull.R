test_that("st_concave_hull works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc$group <- sample(c("A", "B", "C"), size = nrow(nc), replace = TRUE)

  expect_s3_class(
    st_concave_hull(nc),
    "sf"
  )
  expect_s3_class(
    st_concave_hull(sf::st_geometry(nc)),
    "sfc"
  )
  expect_identical(
    nrow(st_concave_hull(nc, by = "group")),
    3L
  )
})
