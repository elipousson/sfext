test_that("sf_bbox_dist functions work", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc_bbox <- sf::st_bbox(nc)

  tol <- 1e-5
  expect_equal(
    sf_bbox_dist(nc_bbox,
      from = c("xmin", "ymin"),
      to = c("xmax", "ymax"), drop = FALSE
    ),
    sf_bbox_diagdist(nc_bbox, drop = FALSE),
    tolerance = tol
  )
  expect_equal(sf_bbox_xdist(nc_bbox), 8.866875, tolerance = tol)
  expect_equal(sf_bbox_ydist(nc_bbox), 2.707657, tolerance = tol)
  expect_equal(sf_bbox_diagdist(nc_bbox), 9.271077, tolerance = tol)
  expect_equal(sf_bbox_diag_ratio_to_dist(nc_bbox, NULL), NULL)
  expect_equal(sf_bbox_diag_ratio_to_dist(nc_bbox, 0.5), 9.271077 / 2, tolerance = tol)
  expect_equal(sf_bbox_asp(nc_bbox), 3.274741, tolerance = tol)
  expect_equal(sf_bbox_orientation(nc_bbox), "landscape")
})
