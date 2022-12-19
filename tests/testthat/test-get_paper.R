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

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc_bbox <- sf::st_bbox(nc)

  expect_identical(
    get_paper(paper = "letter", bbox = nc_bbox)$orientation,
    "landscape"
  )

  expect_identical(
    get_paper(paper = "8.5 in. x 7 in.", orientation = "portrait")$orientation,
    "portrait"
  )

  expect_identical(
    get_social_image(
      image = "Twitter single image post",
      orientation = "landscape"
    )$width,
    1200
  )

  expect_identical(
    get_social_image(
      platform = "Twitter"
    )$name,
    "Twitter single image post"
  )

  expect_identical(
    get_social_image(
      format = "post"
    )$name,
    "Instagram post"
  )
})
