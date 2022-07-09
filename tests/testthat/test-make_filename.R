test_that("make_filename works", {
  expect_equal(make_filename(name = "data", filetype = "csv"), "data.csv")
  expect_equal(make_filename(filename = "data.csv"), "data.csv")
  expect_equal(make_filename(name = "Data", label = "Source", filetype = "geojson"), "source_data.geojson")
  expect_warning(
    make_filename(name = "data", filetype = "csv", filename = "data.csv"),
    "The provided `filename` can't be used if `name` is also provided."
  )
  expect_warning(make_filename(filename = "data.csv", path = "folder", create = FALSE))
})
