#' Get a bounding box buffered a set distance or to match an aspect ratio
#'
#' This function takes a `sf` object and returns a bounding box buffered by a
#' specified distance or diagonal ratio (a proportion of the diagonal distance
#' across the bounding box) and to match the provided aspect ratio.
#'
#' Common aspect ratios include "1:1" (1), "4:6" (0.666), "8.5:11", "16:9"
#' (1.777). The asp parameter supports both numeric values and character
#' strings with ratios matching the format of "width:height".
#'
#' @param x An object `sf`, `bbox`, `sfc`, `raster`, or `sp` object or a
#'   `data.frame` that can be converted to an `sf` object or a list of `sf`,
#'   `bbox`, or `sfc` objects. [st_bbox_asp()] also supports vectors in the same
#'   format as a `bbox` object.
#' @inheritParams st_buffer_ext
#' @inheritParams get_asp
#' @param nudge Passed as to parameter [st_nudge()] when not `NULL`. Numeric
#'   vector or sf object.
#' @param crs Coordinate reference system of bounding box to return; defaults to
#'   `NULL` which maintains the crs of the input object.
#' @param class Class of object to return (`sf` or `bbox`); defaults to "bbox".
#' @param allow_null If `TRUE` and x is `NULL`, return `NULL`.
#' @return A `bbox` object converted to match the class of the class parameter.
#' @aliases st_bbox_adj
#' @name st_bbox_ext
#' @export
#' @importFrom sf st_bbox
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

#' @name st_bbox_ext
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
    ext = c("sf", "sfc", "bbox", "sfg", "Raster", "Extent", "numeric", "character")
  )

  st_bbox_ext.bbox(
    x = as_bbox(x),
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

#' @name st_bbox_ext
#' @export
st_bbox_ext.bbox <- function(x,
                             dist = NULL,
                             diag_ratio = NULL,
                             asp = NULL,
                             unit = NULL,
                             crs = NULL,
                             class = "bbox",
                             nudge = NULL,
                             allow_null = TRUE,
                             ...) {
  if (allow_null && is_null(x)) {
    return(x)
  }

  if (!is_null(nudge)) {
    x <- st_nudge(x, to = nudge)
  }

  # Get buffered area
  x <-
    st_buffer_ext(
      x = x,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      class = "sfc",
      ...
    )

  # Transform crs of sfc object
  x <- transform_sf(x, crs = crs)

  # Get aspect adjusted bbox
  st_bbox_asp(
    x = x,
    asp = asp,
    class = class
  )
}

#' @name st_bbox_ext
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

#' @name st_bbox_ext
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

#' @name st_bbox_asp
#' @rdname st_bbox_ext
#' @export
st_bbox_asp.default <- function(x,
                                asp = NULL,
                                class = "bbox",
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

#' @name st_bbox_asp
#' @rdname st_bbox_ext
#' @export
st_bbox_asp.bbox <- function(x,
                             asp = NULL,
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

  x <-
    sf_bbox_expand(
      x,
      nudge_x = nudge_x,
      nudge_y = nudge_y
    )

  as_sf_class(x, class = class)
}

#' @name st_bbox_asp
#' @rdname st_bbox_ext
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

#' @name st_bbox_asp
#' @rdname st_bbox_ext
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
