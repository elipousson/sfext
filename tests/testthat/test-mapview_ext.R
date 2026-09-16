test_that("mapview_ext works with sf objects", {
  skip_if_not_installed("mapview")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_s4_class(
    mapview_ext(nc, zcol = "AREA"),
    "mapview"
  )

  expect_s4_class(
    mapview_ext(nc, zcol = "AREA", remove_na = TRUE),
    "mapview"
  )
})

test_that("mapview_ext works with non-sf objects", {
  skip_if_not_installed("mapview")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_s4_class(
    mapview_ext(sf::st_geometry(nc)),
    "mapview"
  )
})

test_that("mapview_ext drops list columns", {
  skip_if_not_installed("mapview")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc$list_col_1 <- replicate(nrow(nc), list(1), simplify = FALSE)
  nc$list_col_2 <- replicate(nrow(nc), list(2), simplify = FALSE)

  expect_message(
    mapview_ext(nc),
    "Dropping list columns"
  )
})

test_that("mapview_popup_img works", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("leafpop")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  images <- suppressWarnings(sf::st_centroid(nc[1:2, ]))
  images$path <- c("a.jpg", "b.jpg")
  images$img_width <- c(100, 100)
  images$img_height <- c(50, 50)

  expect_s3_class(
    suppressWarnings(mapview_popup_img(images)),
    "htmlwidget"
  )

  expect_s3_class(
    suppressWarnings(mapview_popup_img(images, popup = FALSE)),
    "htmlwidget"
  )
})

test_that("mapview_popup_img errors without required columns", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("leafpop")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  images <- suppressWarnings(sf::st_centroid(nc[1:2, ]))

  expect_error(
    suppressWarnings(mapview_popup_img(images))
  )
})

test_that("mapview_exif works with a supplied images object", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("leafpop")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  images <- suppressWarnings(sf::st_centroid(nc[1:2, ]))
  images$path <- c("a.jpg", "b.jpg")
  images$img_width <- c(100, 100)
  images$img_height <- c(50, 50)

  expect_s3_class(
    suppressWarnings(mapview_exif(images = images)),
    "htmlwidget"
  )
})
