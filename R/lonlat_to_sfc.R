#' Convert a lon/lat or lat/lon coordinate pair to a sfc object
#'
#' `r lifecycle::badge("experimental")`
#'
#' @name lonlat_to_sfc
#' @param x A length 2 numeric vector with geodetic coordinates in a
#'   [EPSG:4326](https://epsg.org/crs_4326/WGS-84.html) coordinate reference
#'   system.
#' @param range For [lonlat_to_sfc()], an object that is coercible to a `bbox`
#'   object or a length 4 vector with names xmin, xmax, ymin, and ymax. If a
#'   coordinate pair falls outside the latitude/longitude range defined by the
#'   vector but inside the range if reversed, the coordinates are assumed to be
#'   in lat/lon order and are switched to lon/lat order before being converted
#'   to a point. Defaults to `c("xmin" = -180, "ymin" = -50, "xmax" = 180,
#'   "ymax" = 60)`. Note that this default setting will reverse valid
#'   coordinates north of Anchorage, Alaska or south of New Zealand.
#' @param quiet If `TRUE`, suppress alert messages when converting a lat/lon
#'   coordinate pair to a lon/lat pair. Defaults to `FALSE`.
#' @inheritParams rlang::args_error_context
#' @inheritDotParams sf::st_sfc -crs
#' @seealso [is_geo_coords()]
#' @export
#' @importFrom cli cli_alert_warning cli_alert_success
#' @importFrom sf st_point st_sfc
lonlat_to_sfc <- function(x,
                          range = getOption("sfext.coord_range", c("xmin" = -180, "ymin" = -50, "xmax" = 180, "ymax" = 60)),
                          quiet = FALSE,
                          call = parent.frame(),
                          ...) {
  if (!is_geo_coords(x)) {
    cli_abort("{.arg x} must be geodetic coordinates.", call = call)
  }
  cli_quiet(quiet)

  rev_latlon <- FALSE

  if (!is_null(range)) {
    if (is_sf(range, ext = TRUE)) {
      range <- as_bbox(range, crs = 4326)
      range <- set_names(as.numeric(range), names(range))
    }

    likely_latlon <-
      !is_lonlat_in_range(x, range) && is_lonlat_in_range(x, range, rev = TRUE)

    rev_latlon <- any(
      c(
        likely_latlon,
        max(abs(x[[2]])) > 90
      )
    )
  }

  if (isTRUE(rev_latlon)) {
    if (isTRUE(likely_latlon)) {
      cli::cli_alert_warning(
        "Supplied coordinates appear to be in lat/lon order based on {.arg range}."
      )
    }

    cli::cli_alert_success(
      "Reversing lat/lon coordinates to lon/lat order:
        {.val {x}} {cli::symbol$arrow_right} {.val {rev(x)}}"
    )

    x <- rev(x)
  }


  sf::st_sfc(sf::st_point(x), crs = 4326, ...)
}

#' @keywords internal
#' @noRd
check_range <- function(range = NULL, nm = c("xmin", "ymin", "xmax", "ymax")) {
  if (!is_vector(range, length(nm)) | !all(has_name(range, nm))) {
    cli_abort(
      "{.arg range} must be a length {length(nm)} vector with names {.val {nm}}."
    )
  }
}

#' @keywords internal
#' @noRd
is_lonlat_in_range <- function(x,
                               range = NULL,
                               rev = FALSE,
                               allow_null = TRUE,
                               call = parent.frame()) {
  if (is_null(range) & allow_null) {
    return(TRUE)
  }

  cli_abort_ifnot(
    "{.arg x} must be a length 2 vector." = (length(x) == 2),
    call = call
  )

  check_range(range)

  if (isTRUE(rev)) {
    x <- rev(x)
  }

  x <- as.numeric(x)

  lon_in_range <- (x[[1]] >= range[["xmin"]]) & (x[[1]] <= range[["xmax"]])
  lat_in_range <- (x[[2]] >= range[["ymin"]]) & (x[[2]] <= range[["ymax"]])

  lon_in_range & lon_in_range
}
