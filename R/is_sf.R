#' Is the class of this object any of the specified classes?
#'
#' @noRd
is_what <- function(x, what = NULL, allow_null = FALSE) {
  if (allow_null && is_null(x)) {
    return(TRUE)
  }

  inherits(x, what)
}

#' What is the class or spatial attributes of this feature?
#'
#' @param x An `sf`, `sfc`, or `bbox` object.
#' @param ext If `TRUE`, check if x is a `sf`, `sfc`, or `bbox` class object or
#'   not; defaults to `FALSE`. (used by [is_sf])
#' @param allow_null If `TRUE` and x is `NULL`, return `TRUE`; defaults to `FALSE`.
#' @param allow_list If `TRUE`, [is_sf] will return TRUE if x is a list of sf objects.
#' @details
#' - [is_sf]: is x a `sf` class object?
#' - [is_sfc]: is x is a `sfc` class object?
#' - [is_bbox]: is x is a `bbox` class object?
#' - [is_sf_list]: is x is a list of `sf` class objects (with or without names)?
#' - [is_raster]: is x a `Raster` class object?
#' - [is_sp]: is x a `Spatial` class object of any type?
#' - [is_geo_coords]: is x likely a geodetic coordinate pair (a length 2 numeric vector, with a max absolute value less than or equal to 180)?
#' - [is_wgs84]: is x using the [WSG84](https://en.wikipedia.org/wiki/World_Geodetic_System) coordinate reference system?
#'
#' @example examples/is_sf.R
#' @export
is_sf <- function(x, ext = FALSE, allow_null = FALSE, allow_list = FALSE) {
  what <- .what_sf_ext(ext)

  if (is_false(allow_list)) {
    return(is_what(x, what = what, allow_null = allow_null))
  }

  is_what(x, what = what, allow_null = allow_null) ||
    is_sf_list(x, ext = ext, allow_null = allow_null)
}

#' @name is_sfg
#' @rdname is_sf
#' @export
is_sfg <- function(x, allow_null = FALSE) {
  is_what(x, what = "sfg", allow_null = allow_null)
}

#' @name is_sfc
#' @rdname is_sf
#' @export
is_sfc <- function(x, allow_null = FALSE) {
  is_what(x, what = "sfc", allow_null = allow_null)
}

#' @name is_bbox
#' @rdname is_sf
#' @param allow_na If `TRUE`, [is_bbox()] ignores any `NA` values. Defaults to
#'   `FALSE`.
#' @export
is_bbox <- function(x, allow_null = FALSE, allow_na = FALSE) {
  is_what(x, what = "bbox", allow_null = allow_null) &&
    (!any(is.na(x)) || allow_na)
}

#' @name is_raster
#' @rdname is_sf
#' @export
is_raster <- function(x, allow_null = FALSE) {
  is_what(x, what = "RasterLayer", allow_null = allow_null)
}

#' @name is_sp
#' @rdname is_sf
#' @export
is_sp <- function(x, allow_null = FALSE) {
  if (allow_null && is_null(x)) {
    return(TRUE)
  }

  any(grepl("Spatial", class(x)))
}

#' @name is_coords
#' @rdname is_sf
#' @export
is_geo_coords <- function(x, allow_null = FALSE) {
  if (allow_null && is_null(x)) {
    return(TRUE)
  }

  is.numeric(x) && (length(x) == 2) && (max(abs(x)) <= 180)
}
