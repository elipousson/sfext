#' Clip the side or corner of a simple feature or bounding box object
#'
#' Clip based on the corner of the object bounding box.
#'
#' @param x `sf` or `bbox` object to clip
#' @param clip Character string describing the part of the area to clip or
#'   remove. Options include c("top", "right", "bottom", "left", "topright",
#'   "bottomright", "bottomleft", "topleft"). If NULL, the area is not clipped
#'   and a full edge can be returned.
#' @param keep Alternate way of defining clip (by naming the section to keep).
#' @param flip Logical. Default FALSE. If TRUE, than the clip area
#'   is kept instead of removed. If keep is provided, flip is automatically set to TRUE.
#' @param dist Numeric. Distance to use for the edge. Default NULL
#'   meters. Use negative values for an inside edge or positive numbers for an
#'   outside edge.
#' @param diag_ratio Alternate way to define edge distance.
#' @inheritParams st_edge
#' @return `sf` object clipped based on parameters
#' @export
#' @rdname st_clip
#' @export
#' @importFrom sf st_crs st_bbox
#' @importFrom dplyr select
st_clip <- function(x,
                    clip = NULL,
                    keep = NULL,
                    flip = FALSE,
                    dist = NULL,
                    diag_ratio = NULL,
                    unit = "meter") {

  # If bbox, convert to sf
  x <- as_sf(x)

  if (!is.null(dist) || !is.null(diag_ratio)) {
    x <-
      st_edge(x = x, dist = dist, diag_ratio = diag_ratio, unit = unit)
  }

  if (!is.null(keep)) {
    clip <- keep
    flip <- TRUE
  }

  if (!is.null(clip)) {
    clip <-
      make_clip(
        x = x,
        crs = sf::st_crs(x),
        clip = clip
      )

    return(st_erase(x = x, y = clip, flip = flip))
  }

  x
}

#' Make clip area from bounding box.
#'
#' Utility function for [st_clip]
#'
#' @param x A `bbox` object.
#' @param clip A clip style. Supported options include "top", "right", "bottom",
#'   "left", "topright", "bottomright", "bottomleft", or "topleft".
#' @param center Center coordinates in a data frame or a named list with
#'   longitude and latitude values.
#' @param crs Coordinate reference system to return.
#' @param style Style (either "corner" or "side"); defaults to `NULL` where
#'   style is determined by the selected "clip" value.
#' @noRd
#' @importFrom sf st_sf st_sfc st_convex_hull st_union
make_clip <- function(x, clip, crs, style = NULL) {
  clip <- match.arg(clip, c("top", "right", "bottom", "left", "topright", "bottomright", "bottomleft", "topleft"))

  top <- grepl("top", clip)
  bottom <- grepl("bottom", clip)
  left <- grepl("left", clip)
  right <- grepl("right", clip)

  stopifnot(
    !(top && bottom),
    !(left && right)
  )

  if (is.null(style)) {
    if ((top || bottom) && (left || right)) {
      style <- "corner"
    } else {
      style <- "side"
    }
  }

  edges <- get_edges(x = x)

  if (style == "side") {
    if (top) {
      pts <- c(edges$h$top, edges$h$middle)
    } else if (bottom) {
      pts <- c(edges$h$bottom, edges$h$middle)
    } else if (left) {
      pts <- c(edges$v$left, edges$v$middle)
    } else if (right) {
      pts <- c(edges$v$right, edges$v$middle)
    }
  } else if (style == "corner") {
    if (top) {
      pts <- edges$v$middle[2:3]
      if (right) {
        pts <- c(pts, edges$h$top[2:3], edges$h$middle[2:3], edges$v$right[2:3])
      } else if (left) {
        pts <- c(pts, edges$h$top[1:2], edges$h$middle[1:2], edges$v$left[2:3])
      } else {
        pts <- c(pts, edges$h$top[2:3], edges$h$middle[2:3], edges$v$right[2:3])
      }
    } else if (bottom) {
      pts <- edges$v$middle[1:2]
      if (right) {
        pts <- c(pts, edges$h$bottom[2:3], edges$h$middle[2:3], edges$v$right[1:1])
      } else if (left) {
        pts <- c(pts, edges$h$bottom[1:2], edges$h$middle[1:2], edges$v$left[1:2])
      } else {
        pts <- c(pts, edges$h$bottom[2:3], edges$h$middle[2:3], edges$v$right[1:1])
      }
    }
  }

  clip_geometry <-
    sf::st_sfc(sf::st_convex_hull(sf::st_union(pts)))

  clip <-
    sf::st_sf(
      name = "",
      crs = crs,
      geometry = clip_geometry
    )

  return(clip)
}

#' Get bounding box edges
#'
#' Utility function for make_clip
#'
#' @noRd
get_edges <- function(x) {
  # FIXME: Is there a way to get st_clip to work with a by_element parameter?
  x <- sf::st_union(x)
  x <- as_sf(x)
  bbox <- as_bbox(x)
  center <- st_center(as_sf(x), ext = TRUE)$sf

  h_top <-
    as_points(
      c(bbox$xmin, bbox$ymax),
      c(center$lon, bbox$ymax),
      c(bbox$xmax, bbox$ymax)
    )

  h_middle <-
    as_points(
      c(bbox$xmin, center$lat),
      c(center$lon, center$lat),
      c(bbox$xmax, center$lat)
    )

  h_bottom <-
    as_points(
      c(bbox$xmin, bbox$ymin),
      c(center$lon, bbox$ymin),
      c(bbox$xmax, bbox$ymin)
    )

  v_left <-
    as_points(
      c(bbox$xmin, bbox$ymin),
      c(bbox$xmin, center$lat),
      c(bbox$xmin, bbox$ymax)
    )

  v_middle <-
    as_points(
      c(center$lon, bbox$ymin),
      c(center$lon, center$lat),
      c(center$lon, bbox$ymax)
    )

  v_right <-
    as_points(
      c(bbox$xmax, bbox$ymin),
      c(bbox$xmax, center$lat),
      c(bbox$xmax, bbox$ymax)
    )

  list(
    "h" = list(
      "top" = h_top,
      "middle" = h_middle,
      "bottom" = h_bottom
    ),
    "v" = list(
      "left" = v_left,
      "middle" = v_middle,
      "right" = v_right
    )
  )
}
