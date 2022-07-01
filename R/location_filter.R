#' Filter, crop, or trim data to a location
#'
#' Location can be:
#'
#' - A `sf`, `bbox`, or `sfc` object
#' - A U.S. state (name, abbreviation, or GeoID) or county (GeoID)
#' - An address
#'
#' @param data Data to filter by location.
#' @param location A sf, sfc, or bbox object or a character string that is an
#'   address, county GeoID, state name, abbreviation, or GeoID (dist parameters
#'   are ignored if location is a character string).
#' @inheritParams st_bbox_ext
#' @param crop  If `TRUE`, data is cropped to location or bounding box
#'   [sf::st_crop] adjusted by the `dist`, `diag_ratio`, and `asp` parameters
#'   provided. Default TRUE.
#' @param trim  If `TRUE`, data is trimmed to area with [sf::st_intersection].
#'   This option ignores any `dist`, `diag_ratio`, or `asp` parameters. Default
#'   `FALSE`.
#' @param ... Additional parameters; bbox (used instead of location or adjusted
#'   location), county and state (used with get_counties or get_states), join
#'   (passed to [sf::st_filter])
#' @export
location_filter <- function(data,
                            location = NULL,
                            dist = NULL,
                            diag_ratio = NULL,
                            asp = NULL,
                            unit = "meter",
                            crs = NULL,
                            trim = FALSE,
                            crop = TRUE,
                            ...) {
  stopifnot(
    is_sf(data, ext = TRUE)
  )

  if (sf::st_is_longlat(data)) {
    suppressMessages(sf::sf_use_s2(FALSE))
  }

  params <- list2(...)
  bbox <- NULL

  if (is.null(location) && has_name(params, "bbox") && is_bbox(params$bbox)) {
    bbox <- params$bbox
  }

  if (!is.null(location)) {
    if (is.character(location)) {
      # FIXME: Could this be replaced with a call to make_features
      location <- address_to_sf(location, crs = data)
    }

    bbox <-
      st_bbox_ext(
        x = location,
        dist = dist,
        diag_ratio = diag_ratio,
        asp = asp,
        unit = units,
        crs = data,
        class = "bbox"
      )
  }

  if (trim && !is.null(location)) {
    data <- sf_filter(data = data, location = location, join = params$join, trim = trim)
  } else if (!is.null(bbox)) {
    # Otherwise, match bbox crs to data
    data <- bbox_filter(data = data, bbox = bbox, join = params$join, crop = crop)
  }

  if (is_lonlat) {
    suppressMessages(sf::sf_use_s2(TRUE))
  }

  data
}

#' Filter and/or crop data using a bounding box
#'
#' @noRd
#' @importFrom sf st_crop st_filter
bbox_filter <- function(data, bbox = NULL, join = NULL, crop = TRUE) {
  if (is.null(bbox)) {
    return(data)
  }

  bbox <- st_transform_ext(x = bbox, crs = data)

  if (crop) {
    return(suppressWarnings(sf::st_crop(data, bbox)))
  }

  # If no cropping, filter with bbox
  bbox_sf <- sf_bbox_to_sf(bbox)

  if (!is.null(join)) {
    return(sf::st_filter(x = data, y = bbox_sf, join = join))
  }

  sf::st_filter(x = data, y = bbox_sf)
}

#' Filter and/or trim data using an simple feature location
#'
#' @noRd
#' @importFrom cli cli_warn
#' @importFrom sf st_filter
sf_filter <- function(data, location, join = NULL, trim = FALSE) {
  if (is.null(location)) {
    return(data)
  }

  location <- st_transform_ext(x = location, crs = data)

  if (is_point(location) || is_multipoint(location)) {
    location <- st_buffer_ext(x = location, dist = 0.00000001)

    if (trim) {
      cli_warn("location_filter does not support trim = TRUE for POINT or MULTIPOINT geometry.")
      trim <- FALSE
    }
  }

  if (trim) {
    return(st_trim(x = data, y = location))
  }

  if (!is.null(join)) {
    return(sf::st_filter(x = data, y = location, join = join))
  }

  sf::st_filter(x = data, y = location)
}
