#' Buffer a simple feature or bounding box object
#'
#' Return an sf object with a buffer based on `dist` or a proportion of the
#' diagonal distance defined by `diag_ratio`. If x uses geographic coordinates,
#' the coordinate reference system is transformed into EPSG:3857 and then
#' transformed back into the original CRS after the buffer has been applied.
#'
#' [st_edge()] is a variation on [st_buffer_ext()] where dist or diag_ratio is used to
#' define the width of the edge to return either outside the existing geometry
#' (for positive dist values) or inside the existing geometry (for negative dist
#' values).
#'
#' @param x A `sf`, `sfc`, or `bbox` object or a list of `sf` objects.
#' @param dist buffer distance in units. Optional.
#' @param diag_ratio ratio of diagonal distance of area's bounding box used as
#'   buffer distance. e.g. if the diagonal distance is 3000 meters and the
#'   "diag_ratio = 0.1"  a 300 meter will be used. Ignored when `dist` is
#'   provided.
#' @param unit Units for buffer. Supported options include "meter", "foot",
#'   "kilometer", and "mile", "nautical mile" Common abbreviations (e.g. "km"
#'   instead of "kilometer") are also supported. Distance in units is converted
#'   to units matching GDAL units for x; defaults to "meter"
#' @param dist_limits Numeric vector of any length (minimum and maximum values
#'   used as lower and upper limits on distance buffer). Units must match the
#'   provided units; defaults to `NULL`.
#' @param single_side If `TRUE`, single-sided buffers are returned for linear
#'   geometries, in which case negative dist values give buffers on the
#'   right-hand side, positive on the left.
#' @param list.ok If `TRUE`, allow sf list objects as an input and use
#'   [purrr::map()] to apply the provided parameters to each object within the
#'   list to return as a new sf list object.
#' @param ... additional parameters passed to [sf::st_buffer()]
#' @export
#' @importFrom purrr map
#' @importFrom sf st_is_longlat st_crs st_transform st_bbox st_buffer
#' @importFrom units set_units drop_units
st_buffer_ext <- function(x,
                          dist = NULL,
                          diag_ratio = NULL,
                          unit = "meter",
                          dist_limits = NULL,
                          single_side = FALSE,
                          list.ok = TRUE,
                          ...) {
  if (is_sf_list(x, ext = TRUE) && list.ok) {
    return(
      purrr::map(
        x,
        ~ st_buffer_ext(.x, dist, diag_ratio, unit, dist_limits, single_side)
      )
    )
  }

  # If dist is NULL and diag_ratio is NULL return x (with bbox converted to sf
  # if no buffer applied)
  if (is.null(dist) && is.null(diag_ratio)) {
    if (is_bbox(x)) {
      x <- as_sf(x)
    }

    return(x)
  }

  check_sf(x, ext = TRUE)

  # If bbox, convert to sf
  if (is_bbox(x)) {
    x <- as_sf(x)
  }

  # If longlat, save crs and transform to suggested crs
  is_lonlat <- sf::st_is_longlat(x)

  if (is_lonlat) {
    lonlat_crs <- sf::st_crs(x)
    x <- sf::st_transform(x, 3857)
  }

  # Get crs and rename gdal units to match options for set_units
  crs <- sf::st_crs(x)

  dist <- diag_ratio_to_dist(x, dist, diag_ratio)

  unit <- unit %||% get_dist_units(dist, quiet = TRUE)

  units_gdal <- crs$units_gdal

  dist <- convert_dist_units(dist = dist, from = unit, to = units_gdal)

  dist <- limit_dist(
    dist = dist,
    dist_limits = dist_limits,
    unit = unit,
    crs = crs)

  x <- sf::st_buffer(x = x, dist = dist, singleSide = single_side, ...)

  if (!is_lonlat) {
    return(x)
  }

  sf::st_transform(x, lonlat_crs)
}

#' Return distance based on diagonal ratio if dist is NULL
#'
#' @noRd
diag_ratio_to_dist <- function(x = NULL, dist = NULL, diag_ratio = NULL) {
  if (is.null(x) | !is.null(dist)) {
    return(dist)
  }

  if (!is.null(diag_ratio)) {
    sf_bbox_diagdist(bbox = as_bbox(x), drop = TRUE) * diag_ratio
  } else {
    dist
  }
}

#' Limit distance to the min/max values of dist_limits
#'
#' @noRd
#' @importFrom dplyr between
#' @importFrom cli cli_alert_info
limit_dist <- function(dist = NULL,
                       dist_limits = NULL,
                       unit = NULL,
                       crs = NULL,
                       between.ok = FALSE,
                       call = caller_env()) {
  if (is.null(dist_limits)) {
    return(dist)
  }

  units_gdal <- crs$units_gdal

  # dist should already be a units object
  input_dist <-
    convert_dist_units(
      dist = dist,
      from = unit,
      to = units_gdal
    )

  dist_limits <-
    convert_dist_units(
      dist = dist_limits,
      from = unit,
      to = units_gdal
    )

  cli_abort_ifnot(
    "{.arg dist_limits} must be length 2 or greater
    and a {.code units} class object.",
    condition = (length(dist_limits) >= 2) && is_units(dist_limits),
    call = call
  )

  min_limit <- min(dist_limits)
  max_limit <- max(dist_limits)

  dist_between <-
    suppressWarnings(
      dplyr::between(
        input_dist,
        min_limit,
        max_limit
      )
    )

  if (dist_between && between.ok) {
    return(dist)
  }

  compared_to <-
    dplyr::case_when(
      dist_between ~ "between",
      (input_dist < min_limit) ~ "below",
      (input_dist > max_limit) ~ "above"
    )

  dist <-
    switch(compared_to,
      "between" = dist_limits[[which.min(abs(dist_limits - input_dist))]],
      "below" = min_limit,
      "above" = max_limit
    )

  input_lab <- dist_unit_lab(input_dist, to = unit)
  limit_lab <- dist_unit_lab(dist, to = unit)

  message <-
    switch(compared_to,
      "between" = c("The buffer dist ({input_lab}) is between the min/max distance limits.",
        "v" = "Replacing with nearest distance limit ({limit_lab})."
      ),
      "below" = "Replacing buffer dist ({input_lab}) with the minimum limit ({limit_lab}).",
      "above" = "Replacing buffer dist ({input_lab}) with the maximum limit ({limit_lab})."
    )

  cli_inform(
    message = message
  )

  dist
}

#' @noRd
dist_unit_lab <- function(x, to = NULL) {
  if (!is.null(to)) {
    x <- convert_dist_units(x, to = to)
    unit_lab <- to
  } else {
    unit_lab <- get_dist_units(x)
  }

  paste(as.character(x), unit_lab)
}

#' @rdname st_buffer_ext
#' @name st_edge
#' @importFrom sf st_difference
st_edge <- function(x,
                    dist = NULL,
                    diag_ratio = NULL,
                    unit = "meter",
                    ...) {
  if (is_null(c(dist, diag_ratio))) {
    return(x)
  }

  check_sf(x, ext = TRUE)

  if (is_bbox(x)) {
    x <- as_sf(x)
  }

  x_dist <-
    st_buffer_ext(x, dist = dist, diag_ratio = diag_ratio, unit = unit, ...)

  if (any(c(dist, diag_ratio) > 0)) {
    st_erase(x_dist, x)
  } else if (any(c(dist, diag_ratio) <= 0)) {
    # FIXME: Does this cause an error when dist or diag_ratio = 0?
    st_erase(x, x_dist)
  }
}
