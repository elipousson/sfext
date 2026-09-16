test_that("get_margin works with default and named styles", {
  skip_if_not_installed("ggplot2")

  expect_s3_class(get_margin(), "ggplot2::margin")
  expect_equal(as.numeric(get_margin()), c(0, 0, 0, 0))

  expect_equal(as.numeric(get_margin("none")), c(0, 0, 0, 0))
  expect_equal(as.numeric(get_margin("standard")), c(1, 1, 1, 1))
  expect_equal(as.numeric(get_margin("wide")), c(1.5, 1.5, 1.5, 1.5))
  expect_equal(as.numeric(get_margin("extrawide")), c(2, 2, 2, 2))
  expect_equal(as.numeric(get_margin("narrow")), c(0.75, 0.75, 0.75, 0.75))

  expect_error(get_margin("invalid"))
})

test_that("get_margin works with dist", {
  skip_if_not_installed("ggplot2")

  expect_equal(as.numeric(get_margin(dist = 0.5)), c(0.5, 0.5, 0.5, 0.5))
  expect_equal(
    as.numeric(get_margin(dist = c(0.1, 0.2, 0.3, 0.4))),
    c(0.1, 0.2, 0.3, 0.4)
  )
})

test_that("get_margin works with mm units", {
  skip_if_not_installed("ggplot2")

  margin_mm <- get_margin("wide", unit = "mm")
  expect_equal(as.numeric(margin_mm), c(60, 60, 60, 60))
})

test_that("get_margin works with header and footer", {
  skip_if_not_installed("ggplot2")

  margin <- get_margin(margin = "none", header = 0.2, footer = 0.1)
  expect_equal(as.numeric(margin), c(0.2, 0, 0, 0.1))
})

test_that("get_margin works with paper and block_width", {
  skip_if_not_installed("ggplot2")

  margin <- get_margin(paper = "letter", block_width = 6.5)
  expect_s3_class(margin, "ggplot2::margin")
})
