test_that("read_sf_exif reads EXIF metadata into a sf object", {
  skip_if_not_installed("exiftoolr")
  skip_if_not(
    nzchar(Sys.which("exiftool")) || file.exists(exiftoolr:::exiftool_path()),
    "exiftool binary is not configured"
  )

  img_dir <- system.file("images", package = "exiftoolr")
  skip_if(img_dir == "")

  data <- read_sf_exif(path = img_dir, geometry = FALSE, quiet = TRUE)

  expect_s3_class(data, "data.frame")
})

test_that("write_exif_from validates the tag argument", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_error(
    write_exif_from(
      path = data.frame(path = character(0)),
      from = nc,
      tag = "not_a_tag"
    )
  )
})
