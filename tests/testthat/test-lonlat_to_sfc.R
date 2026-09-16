test_that("lonlat_to_sfc works with lon/lat order", {
  pt <- lonlat_to_sfc(c(-80.1, 35.1))

  expect_s3_class(pt, "sfc")
  expect_equal(unname(sf::st_coordinates(pt)[1, "X"]), -80.1)
  expect_equal(unname(sf::st_coordinates(pt)[1, "Y"]), 35.1)
  expect_equal(sf::st_crs(pt), sf::st_crs(4326))
})

test_that("lonlat_to_sfc reverses likely lat/lon order", {
  expect_message(
    pt <- lonlat_to_sfc(c(35.1, -80.1)),
    "Reversing"
  )

  expect_equal(unname(sf::st_coordinates(pt)[1, "X"]), -80.1)
  expect_equal(unname(sf::st_coordinates(pt)[1, "Y"]), 35.1)
})

test_that("lonlat_to_sfc quiet suppresses messages", {
  expect_no_message(
    lonlat_to_sfc(c(35.1, -80.1), quiet = TRUE)
  )
})

test_that("lonlat_to_sfc works with range = NULL", {
  pt <- lonlat_to_sfc(c(35.1, -80.1), range = NULL)

  expect_equal(unname(sf::st_coordinates(pt)[1, "X"]), 35.1)
  expect_equal(unname(sf::st_coordinates(pt)[1, "Y"]), -80.1)
})

test_that("lonlat_to_sfc errors with invalid coordinates", {
  expect_error(lonlat_to_sfc(c(1, 2, 3)))
  expect_error(lonlat_to_sfc("not coords"))
})
