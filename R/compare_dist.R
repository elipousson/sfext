#' Does this distance fit within the bounding box of x or another distance?
#'
#' Check if a distance fits within the bounding box of a sf, sfc, or bbox
#' object, a distance returned by [sf::st_distance()] passing x as the first
#' parameter, or a units class or numeric value. Set `drop = FALSE` to return
#' the number of times dist fits within the specified distance.
#'
#' @param x A `sf`, `sfc`, or `bbox` object or a numeric vector.
#' @param dist Distance to check as a units class or numeric vector
#' @param units Units to compare. If dist or x are numeric they are assumed to
#'   be in the provided units or the units of the coordinate reference system
#'   (CRS) for x (if x is a `sf`, `sfc`, or `bbox` object).
#' @param to Distance to compare dist with. Standard options include "xdist"
#'   (default), "ydist", or "diagdist" to compare to [sf_bbox_xdist()],
#'   [sf_bbox_ydist()], or [sf_bbox_diagdist()]. If to is a length 2 character
#'   vector, x, to, and any ... parameters are passed to [sf_bbox_dist()]. If to is
#'   an sf or sfc object, to and any additional ... parameters are passed to
#'   [sf::st_distance()] using x as the first argument and to as the second. If
#'   x is a numeric vector the value of to is ignored.
#' @param check,round If check is `TRUE` (default), return a logical vector
#'   indicating if the value dist can fit within the distance specified by x and
#'   to. If check is `FALSE` and round is `TRUE`, return the number of times,
#'   dist fits in the specified distance or 0 if dist is greater than the
#'   specified distance. If check is `FALSE` and round is `FALSE`, return the
#'   comparison distance divided by the input distance.
#' @family dist
#' @returns A logical or numeric vector.
#' @export
#' @importFrom rlang arg_match
#' @importFrom sf st_distance
compare_dist <- function(x,
                         dist = NULL,
                         units = NULL,
                         to = "xdist",
                         check = TRUE,
                         round = TRUE,
                         ...) {
  # Modify value of to based on x (does not warn when overriding existing to value)
  to <-
    dplyr::case_when(
      is.numeric(x) ~ "x",
      (length(to) == 2) && is.character(to) ~ "bbox",
      is_sf(to, ext = TRUE) ~ "dist",
      TRUE ~ to
    )

  if (is_sf(to) | is_sfc(to)) {
    compare <- "x"
    x <- sf::st_distance(x, y = to, ...)
  }

  if (compare == "x") {
    x <- convert_dist_units(x, to = units, drop = TRUE)
  } else if (!is_bbox(x)) {
    x <- as_bbox(x)
  }

  compare <-
    arg_match(compare, c("xdist", "ydist", "diagdist", "bbox", "x"))


  compare_dist <-
    switch(compare,
      "xdist" = sf_bbox_xdist(x, units = units),
      "ydist" = sf_bbox_ydist(x, units = units),
      "diagdist" = sf_bbox_diagdist(x, units = units),
      "bbox" = sf_bbox_dist(x, to = to, units = units, ...),
      "x" = x
    )

  dist <- convert_dist_units(dist, to = units, drop = TRUE)

  if (check) {
    return(dist / compare_dist <= 1)
  }

  compare <- compare_dist / dist

  if (round) {
    return(round(compare, 0))
  }

  compare
}
