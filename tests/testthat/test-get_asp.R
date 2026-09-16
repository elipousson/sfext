test_that("get_asp works with asp values", {
  expect_null(get_asp())
  expect_equal(get_asp(asp = 0.5), 0.5)
  expect_equal(get_asp(asp = "1:3"), 1 / 3)
})

test_that("get_asp works with paper", {
  expect_equal(get_asp(paper = "letter"), get_paper("letter")$asp)

  expect_warning(
    block_asp <- get_asp(paper = "letter", block_asp = TRUE, margin = "standard")
  )
  expect_type(block_asp, "double")
})

test_that("get_asp works with bbox", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  bbox <- sf::st_bbox(nc)

  expect_equal(get_asp(bbox = bbox), sf_bbox_asp(bbox))
})

test_that("get_asp errors if no valid input and allow_null is FALSE", {
  expect_error(get_asp(allow_null = FALSE))
})
