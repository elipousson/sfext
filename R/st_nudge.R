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
  check_sf(x, ext = TRUE)

  if (is_bbox(x)) {
    x <- sf_bbox_to_sfc(x)
  }

  is_x_sfc <- is_sfc(x)

  crs <- crs %||% sf::st_crs(x)

  if (is_x_sfc) {
    geom <- sf::st_transform(x, crs = crs)
  } else {
    geom <- as_sfc(x, crs = crs)
  }

  if (is_sf(to, ext = TRUE)) {
    to <- as_sfc(to, crs = crs)

    if (length(geom) != length(to)) {
      to <- sf::st_union(to)
    }

    to <- st_center(to, "sfc")

    x_center <- st_center(sf::st_union(x), "sfc")

    geom <- sf::st_set_crs(geom + (to - x_center), crs)
  } else if (is.numeric(to) && (length(to) == 2)) {
    nudge_y <- to[[1]]
    nudge_x <- to[[2]]
  } else {
    cli_abort(
      "{.arg to} must have a {.cls {c('sf', 'sfc', 'bbox')}} or be a length 2
      {.cls numeric} vector."
    )
  }

  if (any(is.numeric(c(nudge_y, nudge_x)))) {
    nudge <- c(nudge_y %||% 0, nudge_x %||% 0)

    if (!is.null(unit)) {
      nudge <-
        convert_dist_units(
          dist = nudge,
          from = unit,
          to = crs$units,
          drop = TRUE
        )
    }

    geom <- sf::st_set_crs(geom + nudge, crs)
  }

  if (is_x_sfc) {
    x <- geom
  } else {
    x <- sf::st_set_geometry(x, geom)
  }

  st_scale_rotate(x, scale = scale, rotate = rotate)
}
