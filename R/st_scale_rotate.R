#' Scale and rotate a simple feature object, simple feature collection, or
#' bounding box
#'
#' Scale or rotate a simple feature or bounding box object using affine
#' transformations.
#'
#' @param x A `sf`, `sfc`, or `bbox` object or another object coercible to a
#'   simple feature collection with [as_sfc()].
#' @param scale numeric; scale factor, Default: 1
#' @param rotate numeric; degrees to rotate (-360 to 360), Default: 0
#' @name st_scale_rotate
#' @inheritParams rlang::args_error_context
#' @examples
#' nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
#' nc <- sf::st_transform(nc, crs = 3857)
#'
#' plot(st_scale_rotate(nc, scale = 0.75, rotate = 15), max.plot = 1)
#'
#' @export
st_scale_rotate <- function(x, scale = 1, rotate = 0, call = caller_env()) {
  UseMethod("st_scale_rotate")
}

#' @name st_scale_rotate
#' @export
st_scale_rotate.default <- function(x, ...) {
  st_scale_rotate.sfc(as_sfc(x), ...)
}

#' @name st_scale_rotate
#' @export
#' @importFrom sf st_crs st_centroid st_set_crs
st_scale_rotate.sfc <- function(x, scale = 1, rotate = 0, call = caller_env()) {
  check_number_decimal(scale, min = 0, call = call)
  check_number_decimal(rotate, call = call)

  if ((scale == 1) && (rotate == 0)) {
    return(x)
  }

  crs <- sf::st_crs(x)

  # rotate function (see here:
  # https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

  centroid <- suppressWarnings(sf::st_centroid(x))
  x <- (x - centroid) * rot(pi / (360 / (rotate * 2)))
  x <- x * scale + centroid

  sf::st_set_crs(x, crs)
}

#' @name st_scale_rotate
#' @export
#' @importFrom sf st_set_geometry
st_scale_rotate.sf <- function(x, ...) {
  sf::st_set_geometry(x, st_scale_rotate.default(x, ...))
}

#' @name st_scale_rotate
#' @export
#' @importFrom sf st_bbox
st_scale_rotate.bbox <- function(x, ...) {
  sf::st_bbox(st_scale_rotate.default(sf_bbox_to_sfc(x), ...))
}
