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
st_bbox_ext <- function(x = NULL,
                        dist = NULL,
                        diag_ratio = NULL,
                        asp = NULL,
                        unit = NULL,
                        crs = NULL,
                        class = "bbox",
                        nudge = NULL,
                        allow_null = TRUE,
                        allow_list = TRUE) {
  check_sf(x,
    allow_null = allow_null,
    allow_list = allow_list,
    ext = c("sf", "sfc", "bbox", "sfg", "Raster", "Extent", "numeric")
  )

  if (is_null(x) && is_true(allow_null)) {
    return(x)
  }

  if (is_sf_list(x, ext = TRUE) && is_true(allow_list)) {
    bbox_list <-
      map(
        x,
        ~ st_bbox_ext(
          x = .x,
          dist = dist,
          diag_ratio = diag_ratio,
          asp = asp,
          unit = unit,
          crs = crs,
          class = class
        )
      )

    return(bbox_list)
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
      unit = unit
    )

  # Transform crs of sf object
  x <- transform_sf(x, crs = crs)

  # Get aspect adjusted bbox
  bbox <-
    st_bbox_asp(
      x = x,
      asp = asp
    )

  as_sf_class(bbox, class = class)
}

#' @rdname st_bbox_ext
#' @name st_bbox_asp
#' @export
st_bbox_asp <- function(x = NULL,
                        asp = NULL,
                        class = "bbox",
                        allow_list = TRUE) {
  if (is_sf_list(x, ext = TRUE) && is_true(allow_list)) {
    return(
      map(
        x,
        ~ st_bbox_asp(
          .x,
          asp = asp,
          class = class
        )
      )
    )
  }

  bbox <- as_bbox(x)

  # Get adjusted aspect ratio
  if (!is.numeric(asp)) {
    asp <- get_asp(asp = asp)
  }

  if (!is_null(asp) && is.numeric(asp)) {
    # Get width/height
    xdist <- sf_bbox_xdist(bbox) # Get width
    ydist <- sf_bbox_ydist(bbox) # Get height

    # Set default nudge to 0
    x_nudge <- 0
    y_nudge <- 0

    # Compare adjust aspect ratio to bbox aspect ratio
    if (asp >= sf_bbox_asp(bbox)) {
      # adjust x
      x_nudge <- (asp * ydist - xdist) / 2
    } else {
      # adjust y
      y_nudge <- ((xdist / asp) - ydist) / 2
    }

    bbox <-
      sf_bbox_shift(
        bbox = bbox,
        x_nudge = x_nudge,
        y_nudge = y_nudge,
        side = c("top", "bottom", "left", "right"),
        dir = "out"
      )
  }

  as_sf_class(bbox, class = class)
}
