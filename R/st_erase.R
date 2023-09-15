#' Erase or trim geometry of a sf or sfc object
#'
#' This function extends [sf::st_difference()] by unioning the second parameter
#' by default, checking validity of inputs, and optionally (when `flip = TRUE`)
#' using [sf::st_intersection()] instead of [sf::st_difference]. [st_trim()] is
#' equivalent to [st_erase()] with flip set to `TRUE`.
#'
#' @param x A `sf`, `sfc`, or `bbox` object to erase or trim.
#' @param y A `sf`, `sfc`, or `bbox` object to use to erase or trim.
#' @param flip If `TRUE`, use [sf::st_intersection()] to "erase" geometry of x
#'   that intersects y; if `FALSE` use [sf::st_difference()] to trim x to y
#'   geometry, Default: `FALSE`.
#' @param union If `TRUE`, use [sf::st_combine()] and [sf::st_union()] on y
#'   before applying difference/intersection; defaults to `TRUE`.
#' @inheritParams sf::st_difference
#' @example examples/st_erase.R
#' @export
#' @importFrom sf st_union st_combine st_intersection st_difference
st_erase <- function(x, y, flip = FALSE, union = TRUE, combine = FALSE, ...) {
  check_sf(x, ext = TRUE)

  if (is_bbox(x)) {
    x <- sf_bbox_to_sfc(x)
  }

  x <- st_make_valid_ext(x)

  check_sf(y, ext = TRUE)

  y <- st_transform_ext(y, crs = x, class = "sfc")

  if (union) {
    # FIXME: Is this identical to sf::st_combine(sf::st_union(y)) ?
    if (combine) {
      y <- sf::st_combine(y)
    }

    y <- sf::st_union(y)
  }

  y <- st_make_valid_ext(y)

  if (flip) {
    return(suppressWarnings(sf::st_intersection(x, y, ...)))
  }

  suppressWarnings(sf::st_difference(x, y, ...))
}

#' @rdname st_erase
#' @name st_trim
#' @export
st_trim <- function(x, y, union = TRUE, combine = FALSE, ...) {
  st_erase(
    x = x,
    y = y,
    flip = TRUE,
    union = union,
    combine = combine,
    ...
  )
}

#' Checks if all geometries are already valid and make valid if not
#'
#' @inheritParams sf::st_make_valid
#' @export
#' @importFrom sf st_is_valid st_make_valid
st_make_valid_ext <- function(x, ...) {
  validity <- sf::st_is_valid(x)

  if (!any(is.na(validity)) && all(validity)) {
    return(x)
  }

  sf::st_make_valid(x, ...)
}
