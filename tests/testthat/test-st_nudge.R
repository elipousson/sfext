test_that("st_nudge works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  # sf output with sf input, numeric nudge_x, and unit
  expect_s3_class(
    st_nudge(nc[1, ], nudge_y = 100, unit = "mi"),
    "sf"
  )

  # sfc output with sfc input
  expect_s3_class(
    st_nudge(nc[1, ]$geometry, nudge_y = 100, unit = "mi"),
    "sfc"
  )

  # equivalent centroids for nudged sf object and to object
  expect_equal(
    suppressWarnings(sf::st_centroid(st_nudge(nc[1, ], to = nc[2, ]))$geometry),
    suppressWarnings(sf::st_centroid(nc[2, ])$geometry)
  )

  # sf to w/ multiple rows
  expect_s3_class(
    st_nudge(nc[1, ], to = nc[2:4, ]),
    "sf"
  )

  # bbox input
  expect_s3_class(
    st_nudge(sf::st_bbox(nc[1, ]), to = nc[2:4, ]),
    "sfc"
  )

  # numeric to value
  expect_equal(
    st_nudge(nc[1, ], to = c(1000, 1000)),
    st_nudge(nc[1, ], nudge_y = 1000, nudge_x = 1000)
  )

  # abort for character to value
  expect_error(
    st_nudge(nc[1, ], to = "NC"),
  )
})
