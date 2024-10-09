#' Get a bounding box buffered a set distance or to match an aspect ratio
#'
#' [st_bbox_ext()] converts the input to a bounding box with [as_bbox()],
#' applies a buffer (based on a specified distance or proportion of the diagonal
#' distance across the bounding box) and adjusts the bounding box aspect ratio
#' before returning a bounding box (or another class specified by the class
#' parameter). If the input object is a list or `sf_list` object, the function
#' always returns a `list` or `sf_list`.
#'
#' [st_bbox_asp()] supports aspect ratio adjustments without applying a buffer.
#' asp can be supplied as a number or a string matching the format of
#' "width:height". Common aspect ratios include "1:1" (1), "4:6" (0.666),
#' "8.5:11", "16:9" (1.777).
#'
#' @param x A `sf`, `sfc`, `bbox`, `sfg`, `Raster`, `Spatial`, `Extent`,
#'   `numeric`, or `character` object (a place name passed to
#'   [osmdata::getbb()]). See [as_bbox()] for more details.
#' @inheritParams st_buffer_ext
#' @inheritParams get_asp
#' @param nudge Passed as to parameter [st_nudge()] when not `NULL`. A numeric
#'   vector, a `sf` object, or any other object that can be converted to a
#'   simple feature collection with `as_sfc()`..
#' @param crs Coordinate reference system of bounding box to return; defaults to
#'   `NULL` which maintains the crs of the input object.
#' @param class Class of object to return passed to [as_sf_class()]; defaults to
#'   "bbox".
#' @param allow_null If `TRUE` (default) and x is `NULL`, return `NULL` or, if
#'   `FALSE`, abort function.
#' @return A `bbox` object converted to match the class of the class parameter.
#' @aliases st_bbox_adj
#' @name st_bbox_ext
#' @export
st_bbox_ext <- function(x,
                        dist = NULL,
                        diag_ratio = NULL,
                        asp = NULL,
                        unit = NULL,
                        crs = NULL,
                        class = "bbox",
                        nudge = NULL,
                        allow_null = TRUE,
                        allow_list = TRUE) {
  UseMethod("st_bbox_ext")
}

#' @export
st_bbox_ext.default <- function(x,
                                dist = NULL,
                                diag_ratio = NULL,
                                asp = NULL,
                                unit = NULL,
                                crs = NULL,
                                class = "bbox",
                                nudge = NULL,
                                allow_null = TRUE,
                                ...) {
  check_sf(
    x,
    allow_null = allow_null,
    ext = c("sfc", "bbox", "sfg", "Raster", "Spatial", "Extent", "numeric", "character")
  )

  if (allow_null && is_null(x)) {
    return(x)
  }

  if (!is_null(nudge)) {
    x <- st_nudge(x, to = nudge)
  }

  # Get buffered area
  x <- st_buffer_ext(
    x = x,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit,
    ...
  )

  x <- st_transform_ext(x, crs = crs)

  # FIXME: Add check for empty geometries
  # Get aspect adjusted bbox
  st_bbox_asp(
    x = x,
    asp = asp,
    class = class
  )
}

#' @export
st_bbox_ext.list <- function(x,
                             dist = NULL,
                             diag_ratio = NULL,
                             asp = NULL,
                             unit = NULL,
                             crs = NULL,
                             class = "bbox",
                             nudge = NULL,
                             allow_null = TRUE,
                             allow_list = TRUE,
                             ...) {
  if (!allow_list) {
    cli_abort(
      "{.arg allow_list} must be {.code TRUE} if {.arg x} is a {.cls list}."
    )
  }

  map(
    x,
    function(x, ...) {
      st_bbox_ext.default(
        x = x,
        dist = dist,
        diag_ratio = diag_ratio,
        asp = asp,
        unit = unit,
        crs = crs,
        class = class,
        nudge = nudge,
        allow_null = allow_null,
        ...
      )
    }
  )
}

#' @export
st_bbox_ext.sf_list <- function(x,
                                dist = NULL,
                                diag_ratio = NULL,
                                asp = NULL,
                                unit = NULL,
                                crs = NULL,
                                class = "bbox",
                                nudge = NULL,
                                allow_null = TRUE,
                                allow_list = TRUE,
                                ...) {
  if (!allow_list) {
    cli_abort(
      "{.arg allow_list} must be {.code TRUE} if {.arg x} is a {.cls sf_list}."
    )
  }

  as_sf_list(
    st_bbox_ext.list(
      x = x,
      dist = dist,
      diag_ratio = diag_ratio,
      asp = asp,
      unit = unit,
      crs = crs,
      class = class,
      nudge = nudge,
      allow_null = allow_null,
      ...
    )
  )
}

#' @rdname st_bbox_ext
#' @name st_bbox_asp
#' @export
st_bbox_asp <- function(x,
                        asp = NULL,
                        class = "bbox",
                        allow_null = TRUE,
                        allow_list = TRUE) {
  UseMethod("st_bbox_asp")
}

#' @export
st_bbox_asp.default <- function(x,
                                asp = NULL,
                                class = "bbox",
                                ...,
                                allow_null = TRUE) {
  if (allow_null && is_null(x)) {
    return(x)
  }

  st_bbox_asp.bbox(
    x = as_bbox(x),
    asp = asp,
    class = class
  )
}

#' @export
st_bbox_asp.bbox <- function(x,
                             asp = NULL,
                             ...,
                             class = "bbox") {
  # Get adjusted aspect ratio
  if (!is_bare_numeric(asp)) {
    asp <- get_asp(asp = asp)
  }

  if (is_empty(asp)) {
    return(as_sf_class(x, class = class))
  }

  xdist <- sf_bbox_xdist(x)
  ydist <- sf_bbox_ydist(x)

  nudge_x <- 0
  nudge_y <- ((xdist / asp) - ydist) / 2

  # Compare adjust aspect ratio to bbox aspect ratio
  if (asp >= sf_bbox_asp(x)) {
    nudge_x <- (asp * ydist - xdist) / 2
    nudge_y <- 0
  }

  x <- sf_bbox_expand(
      x,
      nudge_x = nudge_x,
      nudge_y = nudge_y
    )

  as_sf_class(x, class = class)
}

#' @export
st_bbox_asp.list <- function(x,
                             asp = NULL,
                             class = "bbox",
                             allow_null = TRUE,
                             allow_list = TRUE) {
  if (!allow_list) {
    cli_abort(
      "{.arg allow_list} must be {.code TRUE} if {.arg x} is a {.cls list}."
    )
  }

  map(
    x,
    function(x) {
      st_bbox_asp.default(
        x,
        asp = asp,
        class = class,
        allow_null = allow_null
      )
    }
  )
}

#' @export
st_bbox_asp.sf_list <- function(x,
                                asp = NULL,
                                class = "bbox",
                                allow_null = TRUE,
                                allow_list = TRUE) {
  if (!allow_list) {
    cli_abort(
      "{.arg allow_list} must be {.code TRUE} if {.arg x} is a {.cls sf_list}."
    )
  }

  as_sf_list(
    st_bbox_asp.list(
      x,
      asp = asp,
      class = class,
      allow_null = allow_null
    )
  )
}
