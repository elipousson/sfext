#' Convert and tranform bounding boxes
#'
#' @description
#' Simple bounding box functions that you can use to:
#'
#' - Convert a bounding box to a simple feature object
#' ([sf_bbox_to_sf()]) or simple feature collection ([sf_bbox_to_sfc()])
#' - Transform the coordinate reference system ([sf_bbox_transform()])
#' - Return a point from any of the corners, center, or midpoints
#' ([sf_bbox_point()])
#' - Convert a bounding box to a SQL style query ([sf_bbox_to_lonlat_query()]),
#' well known text ([sf_bbox_to_wkt()])
#' - Convert a point and a corresponding bounding box into into a npc
#' (normalised parent coordinates) value with [sf_bbox_to_npc()]
#'
#' @param bbox A `bbox` object. Convert input objects to bounding boxes
#'   with [sf::st_bbox()] or [as_bbox()].
#' @param coords Column names with coordinates for query. e.g. `c("X", "Y")` or
#'   `c("longitude", "latitude")` (default). Used by [sf_bbox_to_lonlat_query()]
#'   only.
#' @param crs A coordinate reference system to use for
#'   [sf_bbox_to_lonlat_query()] (defaults to 4326) or for resulting bounding
#'   box ([sf_bbox_transform()]) or sfc object ([sf_bbox_point()]) (defaults to
#'   `NULL`).
#' @inheritParams rlang::args_error_context
#' @name sf_bbox_misc
NULL

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

  if (!is_null(sf_col) && is_sf(bbox_sf)) {
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

#' @name sf_bbox_transform
#' @rdname sf_bbox_misc
#' @export
#' @importFrom sf st_crs st_transform st_bbox
sf_bbox_transform <- function(bbox, crs = NULL) {
  if (is_null(crs)) {
    return(bbox)
  }

  crs <- as_crs(crs, TRUE)

  if (is_same_crs(bbox, crs)) {
    return(bbox)
  }

  sf::st_bbox(sf::st_transform(sf_bbox_to_sfc(bbox), crs))
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
  check_required(bbox)
  # Get CRS from bbox (unless CRS is NA)
  crs <- crs %||% sf::st_crs(bbox)

  # Convert bbox to simple list
  bbox <- as.list(bbox)

  point <-
    arg_match(
      point,
      c("xmin", "ymin", "xmax", "ymax", "xmid", "ymid"),
      multiple = TRUE, error_call = call
    )

  if (any(c("xmid", "ymid") %in% point)) {
    bbox <-
      c(
        bbox,
        list(
          xmid = mean(c(bbox[["xmax"]], bbox[["xmin"]])),
          ymid = mean(c(bbox[["ymax"]], bbox[["ymin"]]))
        )
      )
  }

  stopifnot(
    # Check to make sure point is a pair
    has_length(point, 2),
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
#' @name sf_bbox_to_npc
#' @param point point to find npc coords for center
#' @export
#' @importFrom sf st_coordinates st_distance st_point
sf_bbox_to_npc <- function(bbox,
                           point) {
  if (is_sf(bbox)) {
    bbox <- as_bbox(bbox)
  }

  min_point <- sf_bbox_point(bbox, c("xmin", "ymin"), crs = NA)

  point <- st_transform_ext(point, bbox)
  point <- sf::st_coordinates(as_centroid(point))

  npc <- c(0, 0)

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
