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
#' @param call Passed as the error_call parameter for [rlang::arg_match] to
#'   improve error messages when function is used internally.
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

  dist <-
    sf::st_distance(
      x = sf_bbox_point(bbox, from, crs = NA),
      y = sf_bbox_point(bbox, to, crs = NA),
      by_element = by_element,
      ...
    )

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

  if (is_units(dist) && drop) {
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
  if (is.null(diag_ratio) || is.null(bbox)) {
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
#' @importFrom dplyr case_when
sf_bbox_orientation <- function(bbox, tolerance = 0.1) {
  bbox_asp <- sf_bbox_asp(bbox)

  dplyr::case_when(
    bbox_asp > 1 + tolerance ~ "landscape",
    bbox_asp < 1 - tolerance ~ "portrait",
    TRUE ~ "square"
  )
}