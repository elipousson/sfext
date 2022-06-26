#' General utility functions for working with distance units objects
#'
#' - [is_dist_units]: Is x a distance unit object?
#' - [is_diff_dist]: What is the difference between x and y distance?
#' - [is_same_dist]: Is x the same distance as y? or does the bbox of x and bbox of y have the same x, y, or diagonal distance?
#' - [is_shorter], is_longer: Is x shorter or longer than y?
#' - [is_same_area]: do x and y have the same area?
#'
#' There are two additional functions that support these utility functions:
#'
#' - [get_dist_units]: Get the distance units from x (if x is a sf or units
#' objects or a character string from [dist_unit_options])
#' - [as_dist_units]: Convert x to units using [units::as_units]
#'
#' @name is_dist_units
#' @param x,y objects to check
#' @family dist
#' @export
is_dist_units <- function(x) {
  is_units(x) && (get_dist_units(x) %in% c(dist_unit_options, area_unit_options))
}

#' @name diff_dist
#' @rdname is_dist_units
#' @param units For [is_diff_dist], if x and y are both not units objects, use
#'   units; default to `NULL`.
#' @export
#' @importFrom dplyr case_when
#' @importFrom cli cli_alert_danger
is_diff_dist <- function(x, y, units = NULL) {
  which_is_units <-
    dplyr::case_when(
      is_units(x) && !is_units(y) ~ "x",
      !is_units(x) && is_units(y) ~ "y",
      is_units(x) && is_units(y) ~ "xy",
      TRUE ~ "neither"
    )

  if ((which_is_units == "neither") && is.null(units)) {
    cli::cli_alert_danger("No units could be determined for x or y.")
  }

  switch(which_is_units,
    "x" = diff(c(x, as_dist_units(y, units = x))),
    "y" = diff(c(as_dist_units(x, units = y), y)),
    "xy" = diff(c(x, y)),
    "neither" = diff(c(as_units(x, units = units), as_units(y, units = units)))
  )
}

#' @name is_same_dist
#' @rdname  is_dist_units
#' @param dist type of distance to compare if x and y are `sf`, `sfc`, or `bbox`
#'   objects; "diagdist", "xdist", "ydist". defaults to `NULL`.
#' @param diff If `TRUE`, return results from [is_diff_dist] or [is_diff_area];
#'   if `FALSE`, return logical indicator; defaults to `FALSE`
#' @param ... Additional parameters passed to all.equal
#' @export
#' @importFrom sf st_area
is_same_dist <- function(x, y, dist = NULL, diff = FALSE, ...) {
  if (is.character(dist) && is_sf(x, ext = TRUE) && is_sf(y, ext = TRUE)) {
    x <- as_bbox(x)
    y <- as_bbox(x)

    dist <- match.arg(dist, c("diagdist", "xdist", "ydist"))

    x <-
      switch(dist,
        # FIXME: Is this going to work or is there a tolerance factor needed?
        "diagdist" = sf_bbox_diagdist(x, drop = FALSE),
        "xdist" = sf_bbox_xdist(x, drop = FALSE),
        "ydist" = sf_bbox_ydist(x, drop = FALSE)
      )

    y <-
      switch(dist,
        # FIXME: Is this going to work or is there a tolerance factor needed?
        "diagdist" = sf_bbox_diagdist(y, drop = FALSE),
        "xdist" = sf_bbox_xdist(y, drop = FALSE),
        "ydist" = sf_bbox_ydist(y, drop = FALSE)
      )
  }

  if (diff) {
    return(is_diff_dist(x, y))
  }

  all.equal(as.numeric(is_diff_dist(x, y)), 0, ...)
}

#' @name is_longer
#' @rdname is_dist_units
#' @export
is_longer <- function(x, y) {
  as.numeric(is_diff_dist(x, y)) > 0
}

#' @name is_shorter
#' @rdname is_dist_units
#' @export
is_shorter <- function(x, y) {
  as.numeric(is_diff_dist(x, y)) < 0
}


#' @name get_dist_units
#' @rdname is_dist_units
#' @param null.ok If null.ok is `TRUE`, allow x to return a `NULL` value; if
#'   `FALSE`, error on `NULL` values.
#' @export
#' @importFrom cli cli_abort
#' @importFrom sf st_crs
get_dist_units <- function(x, null.ok = TRUE) {
  if (is.null(x) && null.ok) {
    return(x)
  } else if (is.null(x)) {
    cli::cli_abort(
      "{.var units} must be a unit chracter string, a unit class object, or a sf object with a valid coordinate reference system."
    )
  }

  if (is_sf(x)) {
    return(sf::st_crs(x)$units_gdal)
  }

  if (is_units(x) && all(as.character(units(x)[["numerator"]]) %in% dist_unit_options) && !(as.character(units(x)) %in% area_unit_options)) {
    return(as.character(units(x)[["numerator"]]))
  }

  if (is_units(x)) {
    return(as.character(units(x)))
  }

  if (is.character(x)) {
    return(x[x %in% c(dist_unit_options, area_unit_options)])
  }
}

#' @name as_dist_units
#' @rdname is_dist_units
#' @export
#' @importFrom sf st_crs
#' @importFrom units as_units
as_dist_units <- function(x, units = NULL, null.ok = FALSE) {
  units <- get_dist_units(units, null.ok = null.ok)

  if (!is.null(units)) {
    units <- match.arg(units, c(dist_unit_options, area_unit_options))
  } else if (null.ok) {
    return(x)
  }

  if (is.numeric(x) && !is_dist_units(x)) {
    units::as_units(x, units)
  } else if (cli_yeah("Did you mean to convert {.var x} to {.val {units}}?")) {
    convert_dist_units(
      dist = x,
      to = units
    )
  }
}

#' @name is_diff_area
#' @rdname  is_dist_units
#' @param union If `TRUE`, union objects before comparing area with
#'   [is_diff_area()] or [is_same_area()], defaults to `TRUE`.
#' @export
#' @importFrom sf st_union st_area
is_diff_area <- function(x, y, units = NULL, union = TRUE) {
  if (union) {
    x <- sf::st_union(x)
    y <- sf::st_union(y)
  }

  x_area <- sf::st_area(x)
  y_area <- sf::st_area(y)

  diff(x_area, y_area)
}

#' @name is_same_area
#' @rdname  is_dist_units
#' @export
is_same_area <- function(x, y, units = NULL, union = TRUE, diff = FALSE, ...) {
  if (diff) {
    return(is_diff_area(x, y, units = units, union = union))
  }

  all.equal(as.numeric(is_diff_area(x, y, union = union)), 0, ...)
}

#' @noRd
is_units <- function(x) {
  is_class(x, "units")
}


#' @noRd
is_same_units <- function(x, y) {
  as.character(units(x)) == as.character(units(y))
}
