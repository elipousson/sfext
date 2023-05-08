test_that("write_sf_ext works", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

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

    write_sf_list(
      list(
        "Ashe" = nc[1, ],
        "Alleghany" = nc[2, ]
      ),
      fileext = "geojson"
    )

    expect_true(
      file.exists("ashe.geojson") && file.exists("alleghany.geojson")
    )
  })
})

test_that("write_sf_svg works", {
  skip_if_not_installed("ggplot2")
  skip_on_ci()
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

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


test_that("write_sf_ext works with df objects", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc_df <- sf::st_drop_geometry(nc)

  withr::with_tempdir({
    write_sf_ext(
      nc_df,
      "nc_df.csv"
    )

    expect_true(
      file.exists("nc_df.csv")
    )

    write_sf_ext(
      nc_df,
      "nc_df.xlsx"
    )

    expect_true(
      file.exists("nc_df.xlsx")
    )
  })
})
