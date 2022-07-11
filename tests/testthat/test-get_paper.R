test_that("get_paper works", {
  expect_identical(
    get_paper("letter")$name,
    "Letter"
  )

  expect_identical(
    get_paper(paper = NULL, standard = "ISO", series = "A", size = 4)$width,
    210
  )

  expect_identical(
    nrow(get_paper(width = 11, height = 17)),
    1L
  )

  expect_identical(
    get_paper(ncol = 2)$ncol,
    2
  )

  expect_identical(
    get_paper(paper = "letter", ncol = 2)$col_width,
    4.25
  )

  # Checks get_margins
  expect_identical(
    get_paper(paper = "letter", orientation = "landscape", margin = "extrawide")$block_asp,
    7 / 4.5
  )
})
