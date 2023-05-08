#' Nudge a simple feature to the center of another feature and/or a set distance
#'
#' Nudge, move, or shift a `sf`, `sfc`, or `bbox` object to the center of
#' another feature and/or by a set distance.
#'
#' @param to sf object to use as new center for x or length 2 numeric vector
#'   with the nudge_y and nudge_x distance (in that order).
#' @param nudge_y,nudge_x Distance to nudge geometry in unit. If unit is `NULL`,
#'   distance is assumed to be in the same units as the coordinate reference
#'   system of the input object.
#' @param unit Units for nudge_y and nudge_x distance (also used if to is
#'   numeric).
#' @inheritParams as_sfc
#' @inheritParams st_scale_rotate
#' @export
st_nudge <- function(x,
                     to = NULL,
                     nudge_y = 0,
                     nudge_x = 0,
                     unit = NULL,
                     scale = 1,
                     rotate = 0,
                     crs = NULL) {
  UseMethod("st_nudge")
}

#' @name st_nudge
#' @export
st_nudge.default <- function(x,
                             to = NULL,
                             nudge_y = 0,
                             nudge_x = 0,
                             unit = NULL,
                             scale = 1,
                             rotate = 0,
                             crs = NULL) {
  check_sf(x, ext = TRUE)

  crs <- crs %||% sf::st_crs(x)

  geometry <- sf::st_transform(x, crs = crs)

  if (is_sf(to, ext = TRUE)) {
    to <- as_sfc(to, crs = crs)

    if (!has_length(geometry, length(to))) {
      to <- sf::st_union(to)
    }

    geometry <- geometry + (as_centroid(to) - as_centroid(sf::st_union(x)))
    geometry <- sf::st_set_crs(geometry, crs)
  } else if (is.numeric(to) && has_length(to, 2)) {
    nudge_y <- to[[1]]
    nudge_x <- to[[2]]
  } else if (!is_null(to)) {
    cli_abort(
      "{.arg to} must have a {.cls sf}, {.cls sfc}, or {.cls bbox} class
      or be a length 2 {.cls numeric} vector."
    )
  }

  if (any(is.numeric(c(nudge_y, nudge_x)))) {
    nudge <- c(nudge_y %||% 0, nudge_x %||% 0)

    if (!is_null(unit)) {
      nudge <-
        convert_dist_units(
          dist = nudge,
          from = unit,
          to = crs$units,
          drop = TRUE
        )
    }

    geometry <- sf::st_set_crs(geometry + nudge, crs)
  }

  st_scale_rotate(geometry, scale = scale, rotate = rotate)
}

#' @name st_nudge
#' @export
st_nudge.bbox <- function(x,
                          ...) {
  sf::st_bbox(st_nudge.default(sf_bbox_to_sfc(x), ...))
}

#' @name st_nudge
#' @export
st_nudge.sf <- function(x,
                        ...) {
  sf::st_set_geometry(x, st_nudge.default(sf::st_geometry(x), ...))
}
