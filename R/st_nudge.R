#' Nudge a simple feature to the center of another feature and/or a set distance
#'
#' Nudge, move, or shift a bbox, sfc, or sf object to the center of another
#' feature or by a set distance.
#'
#' @param nudge_y,nudge_x Distance to nudge geometry in unit. If unit is NULL,
#'   distance is assumed to be in the same units as the coordinate reference
#'   system of the input object.
#' @param unit Units for nudge_y and nudge_x distance.
#' @param to sf object to use as new center for x
#' @inheritParams st_scale_rotate
#' @export
st_nudge <- function(x,
                     nudge_y = 0,
                     nudge_x = 0,
                     unit = NULL,
                     to = NULL,
                     scale = 1,
                     rotate = 0,
                     crs = NULL) {
  check_sf(x, ext = TRUE)

  if (is_bbox(x)) {
    x <- as_sfc(x)
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

    group_center <- NULL

    if (length(geom) > 1) {
      group_center <- st_center(sf::st_union(x), "sfc")
    }

    geom <-
      purrr::map_dfr(
        geom,
        ~ as_sf(
          as_sfc(.x, crs = crs) +
          (st_center(sf::st_union(to), "sfc") - (group_center %||% st_center(.x, "sfc"))),
          crs = crs
        )
      )

    geom <- sf::st_geometry(geom)
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

    geom <- geom + nudge
    geom <- as_sfc(geom, crs)
  }

  if (is_x_sfc) {
    x <- geom
  } else {
    x <- sf::st_set_geometry(x, geom)
  }

  st_scale_rotate(x, scale = scale, rotate = rotate)
}
