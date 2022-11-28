test_that("compare_dist works", {
  expect_equal(compare_dist(3, 1), 3)
  expect_true(compare_dist(3, 1, how = "longer"))
  expect_true(compare_dist(1, 3, how = "shorter"))
  expect_true(compare_dist(3, 3, how = "same"))
  expect_equal(compare_dist(9, 3, how = "fit"), 3)
})
