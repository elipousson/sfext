# test_that("number_features works", {
#   nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))
#
#   skip_on_ci()
#   expect_s3_class(
#     number_features(
#       nc
#     ),
#     "sf"
#   )
#   expect_s3_class(
#     number_features(
#       nc,
#       sort = NULL,
#       to = nc[1, ]
#     ),
#     "sf"
#   )
#   expect_equal(
#     rlang::has_name(
#       number_features(
#         nc,
#         .id = "feature_num"
#       ),
#       "feature_num"
#     ),
#     TRUE
#   )
# })
