test_that("st_erase works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  expect_s3_class(
    st_erase(
      nc,
      st_transform_ext(nc[2:3, ], 3857)
    ),
    "sf"
  )
})
