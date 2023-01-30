test_that("st_join_ext works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  pts <- sf::st_sample(nc, 25)
  nc[["category"]] <- sample(c("A", "B", "C"), nrow(nc), TRUE)

  expect_s3_class(
    st_join_ext(nc, nc[1, ]),
    "sf"
  )
  expect_s3_class(
    st_join_ext(nc, nc, col = "category", .id = "NAME"),
    "sf"
  )
})
