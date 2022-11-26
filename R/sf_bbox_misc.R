#' Measure, transform, and convert bounding boxes
#'
#' @description
#' Simple bounding box functions that you can use to:
#'
#' - Measure distances ([sf_bbox_dist()], [sf_bbox_xdist()], [sf_bbox_ydist()],
#' and [sf_bbox_diagdist()])
#' - Get an aspect ratio or orientation ([sf_bbox_asp()]) (counts asp between
#' 0.9 and 1.1 as "square" when tolerance is 0.1)
#' - Return a point from any of the corners, center, or midpoints
#' ([sf_bbox_point()])
#' - Transform the coordinate reference system ([sf_bbox_transform()])
#' - Convert a bounding box to a SQL style query ([sf_bbox_to_lonlat_query()]),
#' well known text ([sf_bbox_to_wkt()]), or a simple feature object
#' ([sf_bbox_to_sf()])
#' - Shift, expand, or contract a bounding box ([sf_bbox_shift()],
#' [sf_bbox_expand()], [sf_bbox_contract()])
#' - Convert a diag_ratio value to a distance value if a bbox and diag_ratio are
#' provided ([sf_bbox_diag_ratio_to_dist()]); return `NULL` if bbox and
#' diag_ratio are `NULL`
#' - Convert a point and a corresponding bounding box into into a npc
#' (normalised parent coordinates) value with [sf_bbox_to_npc()]
#'
#' @param bbox A bounding box object. Convert input objects to bounding boxes
#'   with [sf::st_bbox()] or [as_bbox()]
#' @param coords Column names with coordinates for query. e,g, c("X", "Y") or
#'   c("longitude", "latitude") (default). Used by [sf_bbox_to_lonlat_query()]
#'   only.
#' @param crs A coordinate reference system to use for
#'   [sf_bbox_to_lonlat_query()] (defaults to 4326) or for resulting bounding
#'   box ([sf_bbox_transform()]) or sfc object ([sf_bbox_point()]) (defaults to
#'   `NULL`).
#' @param from,to xy pairs (e.g. c("xmax", "ymax) defining points to measure
#'   distance from and to. Used by [sf_bbox_dist()] only.
#' @param drop If `FALSE`, distance functions return with units. If `FALSE`
#'   (default), distance functions return numeric values. Used by or passed to
#'   [sf_bbox_dist()].
#' @param call Passed as the error_call parameter for [rlang::arg_match] to
#'   improve error messages when function is used internally. Used by
#'   [sf_bbox_point()], [sf_bbox_dist()], and [sf_bbox_shift()].
#' @inheritParams sf::st_distance
#' @family dist
#' @name sf_bbox_misc
NULL

#' @name sf_bbox_asp
#' @rdname sf_bbox_misc
#' @export
sf_bbox_asp <- function(bbox) {
  # Get width and height
  xdist <- sf_bbox_xdist(bbox, drop = TRUE)
  ydist <- sf_bbox_ydist(bbox, drop = TRUE)
  # Get width to height aspect ratio for bbox
  xdist / ydist
}

#' @name sf_bbox_orientation
#' @rdname sf_bbox_misc
#' @param tolerance Numeric value above or below 1 used by
#'   [sf_bbox_orientation()] to describe an aspect ratio as landscape or
#'   portrait; defaults to 0.1.
#' @export
sf_bbox_orientation <- function(bbox, tolerance = 0.1) {
  bbox_asp <- sf_bbox_asp(bbox)

  dplyr::case_when(
    bbox_asp > 1 + tolerance ~ "landscape",
    bbox_asp < 1 - tolerance ~ "portrait",
    TRUE ~ "square"
  )
}

#' @name sf_bbox_point
#' @rdname sf_bbox_misc
#' @param point A length 2 character vector representing a coordinate pair at a
#'   corner, midpoint, or center of the bounding box. Options include "xmin",
#'   "ymin", "xmax", "ymax", "xmid", "ymid".
#' @export
#' @importFrom rlang caller_env arg_match
#' @importFrom sf st_crs st_point st_as_sfc
sf_bbox_point <- function(bbox, point = NULL, crs = NULL, call = caller_env()) {
  point <-
    arg_match(
      point,
      c("xmin", "ymin", "xmax", "ymax", "xmid", "ymid"),
      multiple = TRUE, error_call = call
    )

  # Get CRS from bbox (unless CRS is NA)
  crs <- crs %||% sf::st_crs(bbox)
  # Convert bbox to simple list
  bbox <- as.list(bbox)

  if (any(c("xmid", "ymid") %in% point)) {
    bbox <-
      c(
        bbox,
        list(
          xmid = mean(c(bbox$xmax, bbox$xmin)),
          ymid = mean(c(bbox$ymax, bbox$ymin))
        )
      )
  }

  stopifnot(
    # Check to make sure point is a pair
    length(point) == 2,
    # Check to make sure an x,y pair is provided - not xx or yy
    sum(grepl("^x", point)) + sum(grepl("^y", point)) == 2
  )

  point <- rev_coords(point)
  x <- as_point(c(bbox[[point[1]]], bbox[[point[2]]]))

  if (is.na(crs)) {
    return(x)
  }

  sf::st_as_sfc(list(x), crs = crs)
}

#' @name sf_bbox_dist
#' @rdname sf_bbox_misc
#' @param units The units to return for sf_bbox_dist. Defaults to NULL.
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
#' @rdname sf_bbox_misc
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
#' @rdname sf_bbox_misc
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
#' @rdname sf_bbox_misc
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
#' @rdname sf_bbox_misc
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


#' @name sf_bbox_transform
#' @rdname sf_bbox_misc
#' @export
#' @importFrom sf st_crs st_transform st_bbox
sf_bbox_transform <- function(bbox, crs = NULL) {
  if (is.null(crs) || is_same_crs(bbox, crs)) {
    return(bbox)
  }

  if (is_sf(crs, ext = TRUE)) {
    crs <- sf::st_crs(crs)
  }

  sf::st_bbox(
    sf::st_transform(
      sf_bbox_to_sfc(bbox),
      crs
    )
  )
}

#' @param bbox A bbox object.
#' @param sf_col name to use for geometry column after converting to simple
#'   feature object; defaults to "geometry".
#' @return sf object
#' @seealso
#'  - [sf::st_as_sf()],[sf::st_as_sfc()]
#'  - [sfx::st_extent()]
#' @rdname sf_bbox_misc
#' @export
#' @importFrom sf st_sf st_geometry
sf_bbox_to_sf <- function(bbox, sf_col = "geometry") {
  bbox_sf <- sf::st_sf(sf_bbox_to_sfc(bbox))

  if (!is.null(sf_col) && is_sf(bbox_sf)) {
    sf::st_geometry(bbox_sf) <- sf_col
  }

  bbox_sf
}

#' @rdname sf_bbox_misc
#' @export
#' @importFrom sf st_as_sfc
sf_bbox_to_sfc <- function(bbox) {
  sf::st_as_sfc(bbox)
}

#' @rdname sf_bbox_misc
#' @export
#' @importFrom sf st_as_text
sf_bbox_to_wkt <- function(bbox) {
  # Convert bbox to well known text
  sf::st_as_text(sf_bbox_to_sfc(bbox))
}

#' @rdname sf_bbox_misc
#' @export
sf_bbox_to_lonlat_query <- function(bbox, coords = c("longitude", "latitude"), crs = 4326) {
  bbox <- sf_bbox_transform(bbox, crs = crs)

  coords <- rev_coords(coords)

  paste0(
    c(
      sprintf("(%s >= %s)", coords[1], bbox$xmin),
      sprintf("(%s <= %s)", coords[1], bbox$xmax),
      sprintf("(%s >= %s)", coords[2], bbox$ymin),
      sprintf("(%s <= %s)", coords[2], bbox$ymax)
    ),
    collapse = " AND "
  )
}


#' @rdname sf_bbox_misc
#' @name sf_bbox_shift
#' @param x_nudge,y_nudge Length 1 or 2 numeric vector; unitless. Used by or
#'   passed to [sf_bbox_shift()]. Required for  [sf_bbox_shift()].
#' @param side one or more sides to shift: "top", "bottom", "left", "right", or
#'   "all". Required for [sf_bbox_shift()].
#' @param dir If "in", contract the `bbox` by x_nudge and y_nudge. If "out",
#'   expand the bbox by x_nudge and y_nudge. If dir is not `NULL`; absolute
#'   values are used for x_nudge and y_nudge. Defaults to `NULL`. Optional
#'   [sf_bbox_shift()].
#' @export
#' @importFrom rlang caller_env arg_match has_length
sf_bbox_shift <- function(bbox,
                          x_nudge = 0,
                          y_nudge = 0,
                          side = c("all", "top", "bottom", "left", "right"),
                          dir = NULL,
                          call = caller_env()) {
  side <- arg_match(side, multiple = TRUE, error_call = call)

  if (is.character(dir)) {
    dir <-
      switch(dir,
        "in" = c(1, -1),
        "out" = c(-1, 1)
      )
  } else if (is.numeric(dir)) {
    dir <- c(dir * -1, dir)
  }

  stopifnot(
    is.numeric(x_nudge) && is.numeric(y_nudge),
    (length(x_nudge) <= 2) && (length(y_nudge) <= 2)
  )

  if ((length(x_nudge) == 1) && (length(y_nudge) == 1)) {
    if (is.null(dir)) {
      x_nudge <- as.list(rep(x_nudge, 2))
      y_nudge <- as.list(rep(y_nudge, 2))
    } else if (is.numeric(dir)) {
      x_nudge <- as.list(dir * abs(x_nudge))
      y_nudge <- as.list(dir * abs(y_nudge))
    }
  } else if ((length(x_nudge) == 2) && (length(y_nudge) == 2)) {
    x_nudge <- as.list(x_nudge)
    y_nudge <- as.list(y_nudge)
  }

  nm <- c("min", "max")
  x_nudge <- rlang::set_names(x_nudge, nm)
  y_nudge <- rlang::set_names(y_nudge, nm)

  check_side <- function(x, y) {
    is_any_in(c(x, "all"), y)
  }

  nudge_bbox <- function(bb, nudge, dim = "x", side = "min") {
    dim <- paste0(dim, side)
    bb[[dim]] <- bb[[dim]] + nudge[[side]]
    bb
  }

  if (check_side("left", side)) {
    bbox <- nudge_bbox(bbox, x_nudge, "x", "min")
  }

  if (check_side("right", side)) {
    bbox <- nudge_bbox(bbox, x_nudge, "x", "max")
  }

  if (check_side("bottom", side)) {
    bbox <- nudge_bbox(bbox, y_nudge, "y", "min")
  }

  if (check_side("top", side)) {
    bbox <- nudge_bbox(bbox, y_nudge, "y", "max")
  }

  bbox
}


#' @rdname sf_bbox_misc
#' @name sf_bbox_contract
#' @export
sf_bbox_contract <- function(bbox,
                             x_nudge = 0,
                             y_nudge = 0) {
  sf_bbox_shift(
    bbox = bbox,
    x_nudge = x_nudge,
    y_nudge = y_nudge,
    side = "all",
    dir = -1
  )
}

#' @rdname sf_bbox_misc
#' @name sf_bbox_expand
#' @export
sf_bbox_expand <- function(bbox,
                           x_nudge = 0,
                           y_nudge = 0) {
  sf_bbox_shift(
    bbox = bbox,
    x_nudge = x_nudge,
    y_nudge = y_nudge,
    side = "all",
    dir = 1
  )
}

#' @param point point to find npc coords for center
#' @export
#' @importFrom sf st_coordinates st_distance st_point
sf_bbox_to_npc <- function(bbox,
                           point) {
  point <- st_transform_ext(point, bbox)
  point <- sf::st_coordinates(as_centroid(point))

  if (is_sf(bbox)) {
    bbox <- as_bbox(bbox)
  }

  npc <- c(0, 0)

  min_point <- sf_bbox_point(bbox, c("xmin", "ymin"), crs = NA)

  xdist_to_pt <-
    sf::st_distance(
      min_point,
      sf::st_point(c(point[1], bbox[["ymin"]]))
    )

  npc[1] <- xdist_to_pt / sf_bbox_xdist(bbox)

  ydist_to_pt <-
    sf::st_distance(
      min_point,
      sf::st_point(c(bbox[["xmin"]], point[2]))
    )

  npc[2] <- ydist_to_pt / sf_bbox_ydist(bbox)

  npc
}
