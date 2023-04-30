#' Measure a bounding box using x, y, or diagonal distance
#'
#' @description
#' - Measure distances ([sf_bbox_dist()], [sf_bbox_xdist()], [sf_bbox_ydist()],
#' and [sf_bbox_diagdist()])
#' - Convert a diag_ratio value to a distance value if a bbox and diag_ratio are
#' provided ([sf_bbox_diag_ratio_to_dist()]); return `NULL` if bbox and
#' diag_ratio are `NULL`
#' - Get an aspect ratio or orientation ([sf_bbox_asp()]) (counts asp between
#' 0.9 and 1.1 as "square" when tolerance is 0.1) or ([sf_bbox_orientation()])
#'
#' @name sf_bbox_dist
#' @param bbox A `bbox` object.
#' @param from,to Length 2 character vectors with xy pairs (e.g. c("xmax",
#'   "ymax) defining points to measure distance from and to.
#' @param units The units to return for sf_bbox_dist. Defaults to NULL.
#' @param drop If `FALSE`, distance functions return with units. If `FALSE`
#'   (default), distance functions return numeric values.
#' @inheritParams sf::st_distance
#' @inheritParams rlang::args_error_context
#' @family dist
#' @export
#' @importFrom sf st_distance st_crs
#' @importFrom units drop_units as_units
#' @importFrom rlang caller_env check_required arg_match
sf_bbox_dist <- function(bbox,
                         from,
                         to,
                         units = NULL,
                         drop = TRUE,
                         by_element = TRUE,
                         call = caller_env(),
                         ...) {
  check_required(from)
  check_required(to)

  x <- sf_bbox_point(bbox, from, crs = NA)
  y <- sf_bbox_point(bbox, to, crs = NA)

  dist <-
    sf::st_distance(
      x = x,
      y = y,
      by_element = by_element,
      ...
    )

  # FIXME: Not sure why sf::st_distance is returning a named vector now.
  names(dist) <- NULL

  units_gdal <- sf::st_crs(bbox)$units_gdal
  units <- units %||% units_gdal

  units <-
    arg_match(
      units,
      c(units_gdal, dist_unit_options)
    )

  dist <-
    convert_dist_units(
      dist = dist,
      from = units_gdal,
      to = units
    )

  if (is_units(dist) && is_true(drop)) {
    return(units::drop_units(dist))
  }

  dist
}

#' @name sf_bbox_xdist
#' @rdname sf_bbox_dist
#' @export
sf_bbox_xdist <- function(bbox, units = NULL, drop = TRUE) {
  sf_bbox_dist(
    bbox = bbox,
    from = c("xmin", "ymin"),
    to = c("xmax", "ymin"),
    units = units,
    drop = drop
  )
}

#' @name sf_bbox_ydist
#' @rdname sf_bbox_dist
#' @export
sf_bbox_ydist <- function(bbox, units = NULL, drop = TRUE) {
  sf_bbox_dist(
    bbox = bbox,
    from = c("xmin", "ymin"),
    to = c("xmin", "ymax"),
    units = units,
    drop = drop
  )
}

#' @name sf_bbox_diagdist
#' @rdname sf_bbox_dist
#' @export
#' @importFrom sf st_bbox st_distance st_point
sf_bbox_diagdist <- function(bbox, units = NULL, drop = TRUE) {
  sf_bbox_dist(
    bbox = bbox,
    from = c("xmin", "ymin"),
    to = c("xmax", "ymax"),
    units = units,
    drop = drop
  )
}

#' @name sf_bbox_diag_ratio_to_dist
#' @rdname sf_bbox_dist
#' @param diag_ratio Proportion of the diagonal distance (ratio of distance to
#'   the full diagonal distance) across the bounding box.
#' @export
sf_bbox_diag_ratio_to_dist <- function(bbox,
                                       diag_ratio,
                                       units = NULL,
                                       drop = TRUE) {
  if (is_null(diag_ratio) || is_null(bbox)) {
    return(NULL)
  }

  sf_bbox_diagdist(bbox, units = units, drop = drop) * diag_ratio
}


#' @name sf_bbox_asp
#' @rdname sf_bbox_dist
#' @export
sf_bbox_asp <- function(bbox) {
  # Get width and height
  xdist <- sf_bbox_xdist(bbox, drop = TRUE)
  ydist <- sf_bbox_ydist(bbox, drop = TRUE)
  # Get width to height aspect ratio for bbox
  xdist / ydist
}

#' @name sf_bbox_orientation
#' @rdname sf_bbox_dist
#' @param tolerance Numeric value above or below 1 used by
#'   [sf_bbox_orientation()] to describe an aspect ratio as landscape or
#'   portrait; defaults to 0.1.
#' @export
sf_bbox_orientation <- function(bbox, tolerance = 0.1) {
  bbox_asp <- sf_bbox_asp(bbox)
  check_number_decimal(tolerance)

  if (bbox_asp > (1 + tolerance)) {
    return("landscape")
  }

  if (bbox_asp < (1 - tolerance)) {
    return("portrait")
  }

  "square"
}

#' @name sf_bbox_check_fit
#' @rdname sf_bbox_dist
#' @param dist Distance to compare with bounding box for [sf_bbox_check_fit()].
#'   If dist is length 1 compare to bounding box diagonal distance. If dist is
#'   length 2, compare to bounding box x and y distances. Return TRUE if dist
#'   fits within bounding box or FALSE if dist exceeds bounding box limits.
#' @export
sf_bbox_check_fit <- function(bbox,
                              dist) {
  units <- units %||% get_dist_units(bbox)
  check_required(dist)

  if (has_min_length(dist, 3) || has_length(dist, 0)) {
    cli_abort(
      "{.arg dist} must be length 1 (representing diagonal distance)
      or 2 (with a vector of x and y distance)."
    )
  }

  dist <- convert_dist_units(dist, to = bbox, drop = FALSE)

  if (has_length(dist, 1)) {
    bbox_diagdist <- sf_bbox_diagdist(bbox, drop = FALSE)
    return(as.numeric(diff(c(dist, bbox_diagdist))) > 0)
  }

  bbox_xdist <- sf_bbox_xdist(bbox, drop = FALSE)
  bbox_ydist <- sf_bbox_ydist(bbox, drop = FALSE)

  longer_x <- as.numeric(diff(c(dist[[1]], bbox_xdist))) > 0
  longer_y <- as.numeric(diff(c(dist[[2]], bbox_ydist))) > 0
  longer_x && longer_y
}
