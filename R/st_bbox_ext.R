#' Get a bounding box buffered to match an aspect ratio
#'
#' Takes an area as an sf object or a bounding box and returns a bounding box
#' that matches the provided aspect ratio and contains the area or bounding box
#' provided.
#'
#' Common aspect ratios include "1:1" (1), "4:6" (0.666), "8.5:11", "16:9"
#' (1.777). The asp parameter supports both numeric values and character
#' strings with ratios matching the format of "width:height".
#'
#' @param x An object sf, bbox, sfc, raster, or sp object or a data frame that
#'   can be converted to an sf object or a list of sf, bbox, or sfc objects.
#'   st_bbox_asp also supports vectors in the same format as a bbox object.
#' @inheritParams st_buffer_ext
#' @inheritParams get_asp
#' @param crs Coordinate reference system of bounding box to return
#' @param class Class of object to return (sf or bbox); defaults to "bbox".
#' @param null.ok If `TRUE` and x is `NULL`, return `NULL`.
#' @return Class `bbox` object
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
                        null.ok = TRUE) {
  if (is.null(x)) {
    return(x)
  }

  if (is_sf_list(x, ext = TRUE)) {
    bbox_list <-
      purrr::map(
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
  } else {
    # Get buffered area
    x <-
      st_buffer_ext(
        x = x,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit
      )

    # FIXME: Switching back to the regular st_transform function means that CRS
    # cannot be an sf object for this function
    if (!is.null(crs)) {
      x <- st_transform_ext(x, crs = crs)
    }

    # Get aspect adjusted bbox
    bbox <-
      st_bbox_asp(
        x = x,
        asp = asp
      )

    if ("sf" %in% class) {
      return(sf_bbox_to_sf(bbox))
    }

    bbox
  }
}

#' @rdname st_bbox_ext
#' @name st_bbox_asp
#' @export
st_bbox_asp <- function(x = NULL,
                        asp = NULL,
                        class = "bbox") {
  if (is_sf_list(x, ext = TRUE)) {
    bbox_list <- purrr::map(x, ~ st_bbox_asp(x = .x, asp = asp, class = class))
    return(bbox_list)
  }
  bbox <- as_bbox(x)
  # Get adjusted aspect ratio
  asp <- get_asp(asp = asp)

  if (!is.null(asp) && is.numeric(asp)) {
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

  if ("sf" %in% class) {
    return(sf_bbox_to_sf(bbox))
  }

  bbox
}
