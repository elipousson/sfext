#' Compare a distance to the dimension of a bounding box or another distance
#' value
#'
#' Compare dist to a distance based on x. If to is "xdist" (default), "ydist",
#' "diagdist", or any length 2 character vector supported by [sf_bbox_dist()]
#' dist is compared to the resulting distance. If to is an sf or sfc object, the
#' comparison distance is the results from [sf::st_distance()] with x as the
#' first parameter and to as the second. x can also be a numeric value with or
#' without a units class. Use the "how" argument to determine the type of
#' comparison: "ratio" (dist divided by comparison distance) "fit" (how many
#' lengths of dist can fit wholly within the comparison distance) "longer",
#' "shorter", "same" (uses tolerance parameter).
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
#' @param what If "ratio", dist divided by the comparison distance. If what is
#'   "fit", return the number of times dist fits in the comparison distance
#'   without remainder. If "longer", "shorter", "same", return a logical vector.
#' @family dist
#' @returns A logical or numeric vector.
#' @export
#' @importFrom rlang arg_match
#' @importFrom sf st_distance
compare_dist <- function(dist,
                         x,
                         units = NULL,
                         to = "xdist",
                         how = "ratio",
                         tolerance = 1.5e-8,
                         ...) {
  # Modify value of to based on x (does not warn when overriding existing to value)
  compare_to <-
    dplyr::case_when(
      is.numeric(x) ~ "x",
      is_sf(to, ext = TRUE) ~ "dist",
      (length(to) == 2) && is.character(to) ~ "bbox",
      (length(to) == 1) && is.character(to) ~ to
    )

  if (is_sf(to) | is_sfc(to)) {
    x <- sf::st_distance(x, y = to, ...)
    compare_to <- "x"
  }

  compare_to <-
    arg_match(compare_to, c("xdist", "ydist", "diagdist", "bbox", "x"))

  if (compare_to == "x") {
    x <- convert_dist_units(x, to = units, drop = TRUE)
  } else if (!is_bbox(x)) {
    x <- as_bbox(x)
  }

  dist_to_compare <-
    switch(compare_to,
      "xdist" = sf_bbox_xdist(x, units = units, drop = TRUE),
      "ydist" = sf_bbox_ydist(x, units = units, drop = TRUE),
      "diagdist" = sf_bbox_diagdist(x, units = units, drop = TRUE),
      "bbox" = sf_bbox_dist(x, to = to, units = units, drop = TRUE, ...),
      "x" = x
    )

  dist <- convert_dist_units(dist, to = units, drop = TRUE)

  how <- arg_match(how, c("ratio", "fit", "longer", "shorter", "same"))

  switch(how,
    "ratio" = dist / dist_to_compare,
    "fit" = round(dist / dist_to_compare, 0),
    "longer" = dist > dist_to_compare,
    "shorter" = dist < dist_to_compare,
    "same" = all.equal(dist_to_compare, dist, tolerance = tolerance)
  )
}
