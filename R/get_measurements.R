#' Get measurements for simple feature objects
#'
#' @details Wrapper functions for [sf::geos_measures]:
#'
#' - [get_area]: Wraps on [sf::st_area] but MULTIPOINT or MULTILINESTRING
#' geometry is converted to a polygon using [sf::st_polygonize] which is used to
#' determine the coverage area.
#' - [get_length]: Wraps to [sf::st_length] but POINT and MULTIPOINT geometry is
#' converted to LINESTRING using [as_lines]. If x has POLYGON geometry,
#' [lwgeom::st_perimeter] is used to return the perimeter instead of the length.
#' - [get_dist]: Wraps [sf::st_distance] but x is converted to a POINT using
#' [st_center] and "to" can be a POINT, a sf object that can be converted to a
#' POINT, or a character vector indicating a point on the overall bounding box
#' for x.
#'
#' @details Additional measurement functions:
#'
#' - [get_bearing]: Wraps [geosphere::bearing].
#'
#' @param x A `sf` or `sfc` object to measure.
#' @param units Units to return for area, length, perimeter, or distance;
#'   Default: `NULL`
#' @param keep_all If `TRUE`, return all columns from the original object,
#'   Default: `TRUE`
#' @param drop If `TRUE`, drop units from the line lengths, Default: `FALSE`
#' @param .id Column name to use for area, line length/perimeter, distance, or
#'   bearing.
#' @name get_measurements
#' @example examples/get_measurements.R
NULL

#' @rdname get_measurements
#' @name get_area
#' @export
#' @importFrom sf st_length
#' @importFrom units drop_units
#' @importFrom dplyr bind_cols
get_area <- function(x, units = NULL, keep_all = TRUE, drop = FALSE, .id = "area") {
  if (is_multipoint(x) | is_multiline(x)) {
    # FIXME: This probably only returns an sfc object
    convert_geom_type_alert(x, to = "POLYGON", with = "sf::st_polygonize")
    x_poly <- sf::st_polygonize(x)
  } else {
    x_poly <- x
  }

  if (!is_polygon(x_poly) && !is_multipolygon(x_poly)) {
    cli::cli_abort("{as.character(is_geom_type(x, ext = FALSE))} type objects are not supported by this function.")
  }

  x_area <- sf::st_area(x_poly)

  bind_units_col(
    x,
    x_area,
    units = units,
    drop = drop,
    keep_all = keep_all,
    .id = .id
  )
}

#' @name get_length
#' @rdname get_measurements
#' @export
#' @importFrom cli cli_alert_info
#' @importFrom sf st_length
get_length <- function(x, units = NULL, keep_all = TRUE, drop = FALSE, .id = "length") {
  if (is_point(x) | is_multipoint(x)) {
    convert_geom_type_alert(x, to = "LINE", with = "as_lines")
    x <- as_lines(x)
  }

  longlat <- FALSE

  if (is_polygon(x)) {
    is_pkg_installed("lwgeom")
    cli::cli_alert_info("For objects with POLYGON geometry, {.fun {'get_length'}} uses {.fun {'lwgeom::st_perimeter'}} to return the object perimeter.")
    .id <- "perimeter"

    if (sf::st_is_longlat(x)) {
      longlat <- TRUE
      crs <- sf::st_crs(x)
      x <- sf::st_transform(x, 3857)
    }

    x_len <- lwgeom::st_perimeter(x)
  }

  if (is_line(x) | is_multiline(x)) {
    x_len <- sf::st_length(x)
  }

  if (longlat) {
    x <- sf::st_transform(x, crs)
  }

  bind_units_col(
    x,
    x_len,
    units = units,
    drop = drop,
    keep_all = keep_all,
    .id = .id
  )
}

#' @param to A `sf`, `sfc`, or `bbox` object or a length 2 character vector. If
#'   "to" is an `sf` or `sfc` object, it must have either a single feature or
#'   the same number of features as x (if by_element is `TRUE`). If "to" is a
#'   character vector it must represent a valid xy pair using the following
#'   options: "xmin", "ymin", "xmax", "ymax", "xmid", "ymid".
#' @inheritParams sf::st_distance
#' @name get_dist
#' @rdname get_measurements
#' @family dist
#' @export
#' @importFrom sf st_crs st_distance
get_dist <- function(x, to, by_element = TRUE, units = NULL, drop = FALSE, keep_all = TRUE, .id = "dist", ...) {
  stopifnot(
    is_sf(x, ext = TRUE),
    is_sf(to, ext = TRUE) || is.character(to)
  )

  crs <- sf::st_crs(x)

  if (!is_point(x)) {
    from <- st_center(x, ext = TRUE)$sf
  } else {
    from <- x
  }

  if (is.character(to)) {
    to <-
      match.arg(
        to,
        c("xmin", "ymin", "xmax", "ymax", "xmid", "ymid"),
        several.ok = TRUE
      )

    to <- sf_bbox_point(as_bbox(x), point = to)
    to <- as_sf(to, crs = crs)
  }

  if (!is_point(to)) {
    to <- st_center(to, ext = TRUE)$sf
  }

  x_dist <-
    sf::st_distance(from, to, by_element = by_element, ...)

  bind_units_col(
    x,
    x_dist,
    units = units,
    drop = drop,
    keep_all = keep_all,
    .id = .id
  )
}

#' @param dir Logical indicator whether to include direction in bearing; If
#'   `FALSE`, return the absolute (positive) bearing value. If `TRUE`, return
#'   negative and positive bearing values. Default: `FALSE`.
#' @name get_bearing
#' @aliases st_bearing
#' @rdname get_measurements
#' @export
get_bearing <- function(x, dir = FALSE, keep_all = TRUE, .id = "bearing") {
  is_pkg_installed("geosphere")

  if (is_sfc(x)) {
    x <- as_sf(x)
  }

  if (!is_line(x)) {
    convert_geom_type_alert(x, to = "LINESTRING", with = "as_lines")
    x_lines <- as_lines(x)
  } else {
    x_lines <- x
  }

  start_pts <- get_coords(as_sf(as_startpoint(x_lines)), drop = FALSE)
  end_pts <- get_coords(as_sf(as_endpoint(x_lines)), drop = FALSE)

  x_bearing <-
    vapply(
      seq_len(length(start_pts$lon)),
      function(x) {
        geosphere::bearing(
          p1 = c(start_pts[x, ]$lon, start_pts[x, ]$lat),
          p2 = c(end_pts[x, ]$lon, end_pts[x, ]$lat)
        )
      }, 1.0
    )

  if (!dir) {
    x_bearing <- abs(x_bearing)
  }

  bind_units_col(
    x,
    x_bearing,
    drop = FALSE,
    units = NULL,
    keep_all = keep_all,
    .id = .id
  )
}

#' @noRd
#' @importFrom cli cli_alert_info
convert_geom_type_alert <- function(x, to = NULL, with = NULL) {
  cli::cli_alert_info("Converting {as.character(is_geom_type(x, ext = FALSE))} object to {to} with {.fun {'fn'}}.")
}
