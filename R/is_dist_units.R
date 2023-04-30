#' General utility functions for working with distance units objects
#'
#' - [is_dist_units()]: Is x a distance unit object?
#' - [is_diff_dist()]: What is the difference between x and y distance?
#' - [is_same_dist()]: Is x the same distance as y? or does the bbox of x and bbox
#' of y have the same x, y, or diagonal distance?
#' - [is_shorter()], [is_longer()]: Is x shorter or longer than y?
#' - [is_same_area()]: do x and y have the same area?
#' - [is_same_units()]: are x and y character strings that represent the same
#' units or objects that use the same units?
#'
#' There are two additional functions that support these utility functions:
#'
#' - [get_dist_units()]: Get the distance units from x (if x is a sf or units
#' objects or a character string from [dist_unit_options])
#' - [as_dist_units()]: Convert x to units using [units::as_units]
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
is_diff_dist <- function(x, y, units = NULL) {
  if (!is_null(units)) {
    x <- convert_dist_units(x, to = units)
    y <- convert_dist_units(y, to = units)
  } else if (is_units(x)) {
    y <- convert_dist_units(y, to = x)
  } else if (is_units(y)) {
    x <- convert_dist_units(x, to = y)
  } else {
    cli_abort(
      "units can't be determined for {.arg x} or {.arg y}."
    )
  }

  diff(c(x, y))
}

#' @name is_same_dist
#' @rdname  is_dist_units
#' @param dist type of distance to compare if x and y are `sf`, `sfc`, or `bbox`
#'   objects; "diagdist", "xdist", "ydist". defaults to `NULL`.
#' @param diff If `TRUE`, return results from [is_diff_dist] or [is_diff_area];
#'   if `FALSE`, return logical indicator; defaults to `FALSE`
#' @inheritParams rlang::args_error_context
#' @param ... Additional parameters passed to [all.equal()]
#' @export
#' @importFrom sf st_area
is_same_dist <- function(x, y, dist = NULL, diff = FALSE, call = caller_env(), ...) {
  if (is.character(dist) && is_sf(x, ext = TRUE) && is_sf(y, ext = TRUE)) {
    x <- as_bbox(x)
    y <- as_bbox(x)

    dist <- arg_match(dist, c("diagdist", "xdist", "ydist"), error_call = call)

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
  as.numeric(is_diff_dist(x, y)) < 0
}

#' @name is_shorter
#' @rdname is_dist_units
#' @export
is_shorter <- function(x, y) {
  as.numeric(is_diff_dist(x, y)) > 0
}

#' @name get_dist_units
#' @rdname is_dist_units
#' @param allow_null If allow_null is `TRUE`, allow x to return a `NULL` value;
#'   if `FALSE`, error on `NULL` values.
#' @param multiple If `TRUE` and x is a character vector with distance/area
#'   units, [get_dist_units] may return multiple units. Passed to [rlang::arg_match].
#' @param quiet If `TRUE`, suppress warning messages.
#' @export
#' @importFrom sf st_crs
#' @importFrom rlang arg_match
#' @importFrom cliExtras cli_warn_ifnot cli_abort_ifnot
get_dist_units <- function(x, allow_null = TRUE, multiple = TRUE, quiet = FALSE) {
  if (allow_null && is_null(x)) {
    return(x)
  }

  check_null(x)

  if (is.character(x)) {
    x <- underscore(x)

    return(
      arg_match(
        x,
        c(dist_unit_options, area_unit_options),
        multiple = multiple
      )
    )
  }

  if (is_units(x)) {
    x_is_dist_unit <-
      all(as.character(units(x)[["numerator"]]) %in% dist_unit_options)

    x_not_area_unit <-
      !(as.character(units(x)) %in% area_unit_options)

    if (x_is_dist_unit && x_not_area_unit) {
      return(as.character(units(x)[["numerator"]]))
    }

    return(as.character(units(x)))
  }

  if (is_sf(x, ext = TRUE)) {
    return(sf::st_crs(x)$units_gdal)
  }

  if (is.numeric(x)) {
    cliExtras::cli_warn_ifnot(
      "{.var units} can't be determined for a numeric vector with no
      {.arg units} attribute.",
      condition = quiet
    )

    return(invisible(NULL))
  }

  cliExtras::cli_abort_ifnot(
    "{.var units} must be a {.cls character} string from
    {.code dist_unit_options} or {.code area_unit_options}, a {.cls units}
    object, or a {.cls sf} object with a valid crs.",
    condition = inherits(x, c("character", "units", "sf"))
  )
}

#' @name as_dist_units
#' @rdname is_dist_units
#' @export
#' @importFrom rlang arg_match
#' @importFrom units as_units
#' @importFrom cliExtras cli_yesno
as_dist_units <- function(x,
                          units = NULL,
                          allow_null = FALSE,
                          call = caller_env()) {
  units <- get_dist_units(units, allow_null = allow_null)

  if (allow_null && is_null(units)) {
    return(x)
  }

  units <-
    arg_match(
      units,
      c(dist_unit_options, area_unit_options),
      error_call = call
    )

  if (is.numeric(x) && !is_dist_units(x)) {
    return(units::as_units(x, units))
  }

  if (cliExtras::cli_yesno(
    "Did you mean to convert {.var x} to {.val {units}}?"
  )) {
    convert_dist_units(
      dist = x,
      to = units
    )
  }
}

#' @name is_diff_area
#' @rdname  is_dist_units
#' @param combine If `TRUE`, combine objects with [sf::st_combine()] before
#'   comparing area with [is_diff_area()] or [is_same_area()], defaults to
#'   `TRUE`.
#' @export
#' @importFrom sf st_union st_area
is_diff_area <- function(x, y, units = NULL, combine = TRUE) {
  if (combine) {
    return(diff(st_combined_area(x), st_combined_area(y)))
  }

  diff(sf::st_area(x), sf::st_area(y))
}

#' @noRd
st_combined_area <- function(x) {
  sf::st_area(sf::st_combine(x))
}

#' @name is_same_area
#' @rdname  is_dist_units
#' @export
is_same_area <- function(x,
                         y,
                         units = NULL,
                         combine = TRUE,
                         diff = FALSE,
                         ...) {
  if (diff) {
    return(is_diff_area(x, y, units = units, combine = combine))
  }

  all.equal(as.numeric(is_diff_area(x, y, combine = combine)), 0, ...)
}

#' @noRd
#' @importFrom units as_units
as_units_attr <- function(x) {
  if (is.character(x)) {
    x <- units::as_units(x)
  }

  units(x)
}

#' @name is_same_units
#' @rdname  is_dist_units
#' @export
#' @importFrom units as_units
is_same_units <- function(x, y = NULL) {
  if (any(is_null(c(x, y)))) {
    return(FALSE)
  }

  x <- as_units_attr(x)
  y <- as_units_attr(y)

  in_opts <- c("in", "inch", "inches", "international_inch", "international_inches")
  ft_opts <- c("ft", "foot", "feet", "international_foot", "international_feet")
  yd_opts <- c("yd", "yard", "yards", "international_yard", "international_yards")

  nums <- c(x[["numerator"]], y[["numerator"]])
  dens <- c(x[["denominator"]], y[["denominator"]])

  if (any(
    c(all(nums %in% in_opts), all(nums %in% ft_opts), all(nums %in% yd_opts))
  ) && (
    all(dens == character(0)) | (dens[1] == dens[2])
  )) {
    return(TRUE)
  }

  units::as_units(x) == units::as_units(y)
}
