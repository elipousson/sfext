test_that("write_sf_ext works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

  expect_error(
    write_sf_ext(nc)
  )

  withr::with_tempdir({
    write_sf_ext(
      nc,
      name = "nc.geojson"
    )

    expect_true(
      file.exists("nc.geojson")
    )

    write_sf_ext(
      nc,
      path = "nc.gpkg",
      description = "North Carolina SIDS data"
    )

    expect_true(
      file.exists("nc.gpkg")
    )

    write_sf_ext(
      nc,
      filename = "nc.csv"
    )

    expect_true(
      file.exists("nc.csv")
    )

    write_sf_ext(
      nc,
      filename = "nc.xlsx"
    )

    expect_true(
      file.exists("nc.xlsx")
    )

    write_sf_ext(
      nc,
      filename = "nc.rda"
    )

    expect_true(
      file.exists("nc.rda")
    )
  })
})

test_that("write_sf_svg works", {
  skip_if_not_installed("ggplot2")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

  withr::with_tempdir({
    write_sf_svg(
      nc,
      filename = "nc.svg"
    )

    expect_true(
      file.exists("nc.svg")
    )
  })
})
