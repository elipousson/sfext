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
      filename = "nc.rda"
    )

    expect_true(
      file.exists("nc.rda")
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

test_that("write_sf_ext works with xlsx files", {
  skip_if_not_installed("openxlsx")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  withr::with_tempdir({
    write_sf_ext(
      nc,
      filename = "nc.xlsx"
    )

    expect_true(
      file.exists("nc.xlsx")
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
  })
})

test_that("write_sf_ext works with df objects and xlsx files", {
  skip_if_not_installed("openxlsx")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc_df <- sf::st_drop_geometry(nc)

  withr::with_tempdir({
    write_sf_ext(
      nc_df,
      "nc_df.xlsx"
    )

    expect_true(
      file.exists("nc_df.xlsx")
    )
  })
})

test_that("write_sf_ext warns before overwriting an existing file", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  withr::with_tempdir({
    write_sf_ext(nc, filename = "nc.geojson")

    expect_message(
      suppressWarnings(
        write_sf_ext(nc, filename = "nc.geojson", overwrite = FALSE)
      ),
      "already exists"
    )
  })
})

test_that("write_sf_cache writes to the specified cache directory", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  withr::with_tempdir({
    cache_dir <- file.path(getwd(), "cache")

    write_sf_cache(
      nc[1:2, ],
      filename = "nc_cache.geojson",
      data_dir = cache_dir
    )

    expect_true(file.exists(file.path(cache_dir, "nc_cache.geojson")))
  })
})

test_that("write_sf_ext caches a copy when cache = TRUE", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  withr::with_tempdir({
    cache_dir <- file.path(getwd(), "cache")
    withr::local_envvar(R_USER_CACHE_DIR = cache_dir)

    write_sf_ext(
      nc[1:2, ],
      filename = "nc.geojson",
      cache = TRUE
    )

    expect_true(file.exists("nc.geojson"))
    expect_true(length(list.files(cache_dir, pattern = "nc[.]geojson", recursive = TRUE)) >= 1)
  })
})
