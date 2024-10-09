test_that("get_data_dir works", {
  expect_true(
    is_null(get_data_dir())
  )

  skip_on_ci()
  # FIXME: Disabled this failing test on 2024-10-09
  # expect_true(
  #   str_detect(get_data_dir(cache = TRUE), "sfext$")
  # )
})

test_that("get_path_filetype works", {
  expect_true(
    get_path_filetype(filetype = "jpeg") == "jpeg"
  )
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
