test_that("st_union_ext works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc$group <- sample(c("A", "B", "C"), size = nrow(nc), replace = TRUE)
  expect_identical(
    st_union_ext(nc[1, ], nc[2, ]),
    suppressWarnings(sf::st_union(nc[1, ], nc[2, ]))
  )
  expect_named(
    st_union_ext(nc, name_col = "NAME"),
    c("NAME", "geometry")
  )
  expect_named(
    st_union_ext(nc, name_col = NULL, label = "NC"),
    c("label", "geometry")
  )
  expect_identical(
    nrow(st_union_by(nc, group)),
    3L
  )
  expect_s3_class(
    st_union_ext(sf::st_bbox(nc[1, ]), nc[2, ]),
    "sfc"
  )
})
