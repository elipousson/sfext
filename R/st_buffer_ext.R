#' Buffer a simple feature or bounding box object
#'
#' Return an sf object with a buffer based on `dist` or a proportion of the
#' diagonal distance defined by `diag_ratio`. If x uses geographic coordinates,
#' the coordinate reference system is transformed into EPSG:3857 and then
#' transformed back into the original CRS after the buffer has been applied.
#'
#' st_edge is a variation on st_buffer_ext where dist or diag_ratio is used to
#' define the width of the edge to return either outside the existing geometry
#' (for positive dist values) or inside the existing geometry (for negative dist
#' values).
#'
#' @param x sf or bbox object.
#' @param dist buffer distance in units. Optional.
#' @param diag_ratio ratio of diagonal distance of area's bounding box used as
#'   buffer distance. e.g. if the diagonal distance is 3000 meters and the
#'   "diag_ratio = 0.1"  a 300 meter will be used. Ignored when `dist` is
#'   provided.
#' @param unit Units for buffer. Supported options include "meter", "foot",
#'   "kilometer", and "mile", "nautical mile" Common abbreviations (e.g. "km"
#'   instead of "kilometer") are also supported. Distance in units is converted
#'   to units matching GDAL units for x; defaults to "meter"
#' @param dist_limits Numeric vector of any length (minimum and maximum values
#'   used as lower and upper limits on distance buffer). Units must match the
#'   provided units; defaults to NULL.
#' @param single_side If `TRUE`, single-sided buffers are returned for linear
#'   geometries, in which case negative dist values give buffers on the
#'   right-hand side, positive on the left.
#' @param ... additional parameters passed to [sf::st_buffer()]
#' @export
#' @importFrom sf st_is_longlat st_crs st_transform st_bbox st_buffer
#' @importFrom units set_units drop_units
st_buffer_ext <- function(x,
                          dist = NULL,
                          diag_ratio = NULL,
                          unit = "meter",
                          dist_limits = NULL,
                          single_side = FALSE,
                          ...) {
  if (is_sf_list(x, ext = TRUE)) {
    x_list <-
      purrr::map(
        x,
        ~ st_buffer_ext(x = .x, dist = dist, diag_ratio = diag_ratio, unit = unit, dist_limits = dist_limits, single_side = single_side)
      )

    return(x_list)
  }

  # If bbox, convert to sf
  x <- as_sf(x)

  # If dist is NULL and diag_ratio is NULL return x (with bbox converted to sf if no buffer applied)
  if (!is.null(dist) | !is.null(diag_ratio)) {

    # If longlat, save crs and transform to suggested crs
    is_lonlat <- sf::st_is_longlat(x)

    if (is_lonlat) {
      lonlat_crs <- sf::st_crs(x)
      x <- sf::st_transform(x, 3857)
    }

    # Get crs and rename gdal units to match options for set_units
    crs <- sf::st_crs(x)

    if (is_units(dist)) {
      if (is.null(unit)) {
        unit <- get_dist_units(dist)
      }
      dist <- units::drop_units(dist)
    } else if (is.null(dist) && !is.null(diag_ratio)) {
      # Use the bbox diagonal distance to make proportional buffer distance
      dist <- sf_bbox_diagdist(bbox = as_bbox(x), drop = TRUE) * diag_ratio
    }

    dist <- convert_dist_units(dist = dist, from = unit, to = crs$units_gdal)

    if (!is.null(dist_limits)) {
      dist_limits <-
        convert_dist_units(dist = dist_limits, from = unit, to = crs$units_gdal)

      dist <- limit_dist(dist = dist, dist_limits = dist_limits)
    }

    x <- sf::st_buffer(x = x, dist = dist, singleSide = single_side, ...)

    if (is_lonlat) {
      x <- sf::st_transform(x, lonlat_crs)
    }
  }

  x
}

#' Limit distance to the min/max values of dist_limits
#'
#' @noRd
#' @importFrom dplyr between
#' @importFrom cli cli_alert_info
limit_dist <- function(dist = NULL, dist_limits = NULL) {
  if (!is.null(dist_limits) && (length(dist_limits) >= 2) && is_class(dist_limits, "units")) {
    min_limit <- min(dist_limits)
    max_limit <- max(dist_limits)

    check_between_dist <-
      suppressWarnings(
        dplyr::between(
          dist,
          min_limit,
          max_limit
        )
      )

    if (check_between_dist) {
      cli::cli_alert_info("The buffer dist ({dist} {units_gdal}) is between the min/max distance limits.")
      dist <- dist_limits[[which.min(abs(dist_limits - dist))]]
      cli::cli_alert_info("Replacing with nearest distance limit ({dist} {units_gdal}).")
    } else if (dist < min_limit) {
      dist <- min_limit
      cli::cli_alert_info("Replacing buffer dist ({num_dist} {units_gdal}) with the minimum limit ({min_limit} {units_gdal}).")
    } else if (dist > max_limit) {
      dist <- max_limit
      cli::cli_alert_info("Replacing buffer dist ({num_dist} {units_gdal})with the maximum limit ({max_limit} {units_gdal}).")
    }
  }

  return(dist)
}

#' @rdname st_buffer_ext
#' @name st_edge
#' @importFrom sf st_difference
st_edge <- function(x,
                    dist = NULL,
                    diag_ratio = NULL,
                    unit = "meter",
                    ...) {

  # If bbox, convert to sf
  x <- as_sf(x)

  x_dist <-
    st_buffer_ext(x, dist = dist, diag_ratio = diag_ratio, unit = unit, ...)

  # FIXME: What if dist or diag_ratio = 0?
  if (dist > 0 | diag_ratio > 0) {
    x <- st_erase(x_dist, x)
  } else if (dist < 0 | diag_ratio < 0) {
    x <- st_erase(x, x_dist)
  }

  x
}
