test_that("get_data_dir works", {
  expect_true(
    is_null(get_data_dir())
  )
})

test_that("get_data_dir creates a new directory and returns the path", {
  dir <- withr::local_tempdir()
  new_path <- file.path(dir, "new_data_dir")

  expect_false(dir.exists(new_path))

  result <- suppressMessages(get_data_dir(path = new_path, create = TRUE))

  expect_true(dir.exists(new_path))
  expect_identical(result, new_path)

  # Existing directory is returned as-is without a message
  expect_identical(get_data_dir(path = new_path), new_path)
})

test_that("get_data_dir uses cache directory when cache = TRUE", {
  home <- withr::local_tempdir()
  withr::local_envvar(
    HOME = home,
    XDG_CACHE_HOME = NA,
    R_USER_CACHE_DIR = NA
  )

  cache_path <- suppressMessages(get_data_dir(cache = TRUE, create = TRUE))

  expect_true(is.character(cache_path))
  expect_true(grepl("sfext$", cache_path))
  expect_true(dir.exists(cache_path))
})

test_that("get_data_dir warns and errors", {
  expect_error(
    get_data_dir(allow_null = FALSE)
  )
  expect_warning(
    get_data_dir(
      path = "xyz",
      create = FALSE
    )
  )
})

test_that("get_path_filetype works", {
  expect_true(
    get_path_filetype(filetype = "jpeg") == "jpeg"
  )

  dir <- withr::local_tempdir()
  file.create(file.path(dir, c("a.csv", "b.csv")))

  expect_identical(get_path_filetype(dir), "csv")

  expect_error(
    get_path_filetype(file.path(dir, "missing.csv"))
  )
})

test_that("list_data_files works", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, c("a.csv", "b.geojson")))

  all_files <- list_data_files(path = dir)
  expect_length(all_files, 2)

  csv_files <- list_data_files(path = dir, fileext = "csv")
  expect_length(csv_files, 1)
  expect_true(grepl("a[.]csv$", csv_files))
})
