#' rdeck editor
#'
#' @inheritParams rdeck::rdeck
#' @inheritDotParams rdeck::rdeck
#' @export
rdeck_edit <- function(features, mode = rdeck::cur_value(), initial_bounds = NULL, ...) {
  check_required(features)
  features <- st_wgs84(features)
  check_installed("rdeck")

  rdeck::rdeck(
    initial_bounds = initial_bounds %||% sf::st_bbox(features),
    editor = rdeck::editor_options(features = features, mode = mode),
    ...
  )
}

#' @export
#' @rdname rdeck_edit
rdeck_select <- function(features, ..., mode = "select") {
  rdeck_edit(features, mode = mode, ...)
}

#' Editor options with CRS conversion
#'
#' A wrapper for [rdeck::editor_options()] that automatically converts features
#' to WGS84.
#'
#' @name rdeck_editor_options
#' @rdname rdeck_edit
#' @inheritParams rdeck::editor_options
#' @export
editor_options <- function(mode = rdeck::cur_value(),
                           features = rdeck::cur_value()) {
  check_installed("rdeck")
  if (!is_wgs84(features)) {
    features <- st_wgs84(features)
  }

  rdeck::editor_options(mode, features)
}
