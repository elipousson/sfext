test_that("read_sf_ext works with file paths", {
  nc <- read_sf_ext(
    path = system.file("shape/nc.shp", package = "sf")
  )

  expect_s3_class(
    nc,
    "sf"
  )

  expect_s3_class(
    read_sf_ext(
      path = system.file("shape/nc.shp", package = "sf"),
      bbox = sf::st_bbox(nc[1, ])
    ),
    "sf"
  )

  expect_s3_class(
    read_sf_ext(
      path = system.file("shape/nc.shp", package = "sf"),
      bbox = sf_bbox_transform(sf::st_bbox(nc[1, ]), 3857)
    ),
    "sf"
  )

  expect_s3_class(
    read_sf_query(
      dsn = system.file("shape/nc.shp", package = "sf")
    ),
    "sf"
  )
})

test_that("read_sf_ext works with mapbaltimore package", {
  skip_if_not_installed("mapbaltimore")
  expect_s3_class(
    read_sf_ext(
      "trees",
      package = "mapbaltimore"
    ),
    "sf"
  )
})

test_that("read_sf_path errors on a missing file", {
  expect_error(
    read_sf_path(path = "does/not/exist.shp")
  )
})

test_that("read_sf_path dispatches to read_sf_csv for csv paths", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  withr::with_tempdir({
    readr_installed <- requireNamespace("readr", quietly = TRUE)
    skip_if_not(readr_installed, "readr is not installed")

    readr::write_csv(sf::st_drop_geometry(nc[, c("NAME", "FIPS")]), "nc.csv")

    data <- read_sf_path("nc.csv", coords = NULL)

    expect_s3_class(data, "data.frame")
    expect_true(all(c("NAME", "FIPS") %in% names(data)))
  })
})

test_that("read_sf_csv can geocode coordinate columns to a sf object", {
  withr::with_tempdir({
    df <- data.frame(
      name = c("a", "b"),
      lon = c(-78.9, -80.1),
      lat = c(36.1, 35.9)
    )
    write.csv(df, "coords.csv", row.names = FALSE)

    data <- read_sf_csv("coords.csv")

    expect_s3_class(data, "sf")
    expect_equal(nrow(data), 2)
  })
})

test_that("read_sf_rdata reads a legacy rda file created with save()", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  withr::with_tempdir({
    save(nc, file = "nc_legacy.rda")

    data <- read_sf_rdata("nc_legacy.rda")

    expect_s3_class(data, "sf")
    expect_equal(nrow(data), nrow(nc))
  })
})

test_that("read_sf_rdata reads a rds file and filters by bbox", {
  skip_if_not_installed("readr")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  withr::with_tempdir({
    readr::write_rds(nc, "nc.rds")

    data <- read_sf_rdata("nc.rds")
    expect_s3_class(data, "sf")
    expect_equal(nrow(data), nrow(nc))

    filtered <- read_sf_rdata("nc.rds", bbox = sf::st_bbox(nc[1, ]))
    expect_s3_class(filtered, "sf")
    expect_lte(nrow(filtered), nrow(nc))
  })
})

test_that("read_sf_rdata warns when data is not a sf object and bbox is set", {
  withr::with_tempdir({
    save(mtcars, file = "mtcars.rda")

    expect_message(
      read_sf_rdata(
        "mtcars.rda",
        bbox = sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 1, ymax = 1), crs = 4326)
      ),
      "not a"
    )
  })
})

test_that("read_sf_pkg errors for an unavailable package", {
  expect_error(
    read_sf_pkg("x", package = "not_a_real_package_xyz")
  )
})

test_that("read_sf_pkg errors when data can't be found in the package", {
  expect_error(
    suppressWarnings(read_sf_pkg("not_a_real_dataset_xyz", package = "sf"))
  )
})

test_that("read_sf_url errors on an invalid url", {
  expect_error(
    read_sf_url("not a url")
  )
})

test_that("get_social_image returns paper info without network access", {
  image <- get_social_image()

  expect_s3_class(image, "data.frame")
  expect_equal(nrow(image), 1)
})

test_that("get_social_image respects platform and format filters", {
  platforms <- unique(as.character(sfext::paper_sizes$standard[
    sfext::paper_sizes$type == "social"
  ]))
  skip_if(length(platforms) == 0)

  image <- get_social_image(platform = platforms[[1]])

  expect_s3_class(image, "data.frame")
})

test_that("get_social_image errors for an invalid platform", {
  expect_error(
    get_social_image(platform = "not_a_real_platform")
  )
})

test_that("address_to_sf validates input type", {
  skip_if_not_installed("tidygeocoder")

  expect_error(
    address_to_sf(x = 123)
  )
})
