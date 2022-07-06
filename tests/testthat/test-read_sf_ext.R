test_that("read_sf_ext works", {
  expect_s3_class(
    read_sf_ext(
      url = "https://sampleserver1.arcgisonline.com/ArcGIS/rest/services/Louisville/LOJIC_PublicSafety_Louisville/MapServer/1"
    ),
    "sf"
  )

  expect_s3_class(
    read_sf_ext(
      path = system.file("shape/nc.shp", package = "sf")
    ),
    "sf"
  )

  expect_s3_class(
    read_sf_query(
      dsn = system.file("shape/nc.shp", package = "sf")
    ),
    "sf"
  )

  expect_s3_class(
    read_sf_ext(
      url = "https://gist.githubusercontent.com/elipousson/77271144b3cb82efd732c4e30b273c5a/raw/68dc6008fe27cb7047aa1c5dfba1fa17f6f88f16/baltimore_city_tracts_acs5_2019_B02001_003.geojson"
    ),
    "sf"
  )


  expect_s3_class(
    read_sf_ext(
      url = "https://www.google.com/maps/d/u/0/viewer?mid=1CEssu_neU7lx_vAZs5qpufOBoUQ&ll=-3.81666561775622e-14%2C0&z=1"
    ),
    "sf"
  )
})