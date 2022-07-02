#' Filter, crop, trim, or erase data
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
#' @inheritParams sf::st_filter
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
st_filter_ext <- function(x,
                          y = NULL,
                          trim = FALSE,
                          crop = FALSE,
                          erase = FALSE,
                          crs = NULL,
                          .predicate = sf::st_intersects,
                          ...) {
  if (!is_sf(x)) {
    x <- as_sf(x)
  }

  if (is.null(y)) {
    return(x)
  }

  if (erase) {
    cli_warn_ifnot(
      "{.arg erase} is ignored when {.arg trim} or {.arg crop} are {.val TRUE}.",
      condition = !trim && !crop
    )
  }

  type <-
    dplyr::case_when(
      trim ~ "trim",
      crop ~ "crop",
      erase ~ "erase",
      TRUE ~ "filter"
    )

  if (!crop) {
    if (is_bbox(y)) {
      y <- as_sfc(y)
    }

    x <-
      sf::st_filter(
        x,
        st_transform_ext(y, crs = x),
        ...,
        .predicate = .predicate
      )
  }

  x <-
    switch(type,
      "trim" = st_trim(x, y),
      "crop" = suppressWarnings(sf::st_crop(x, y)),
      "erase" = st_erase(x, y),
      "filter" = x
    )

  as_crs(x, crs = crs)
}
