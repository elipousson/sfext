#' Filter, crop, trim, or erase a simple feature object or list
#'
#' @param x,y A `sf`, `sfc`, or `bbox` object. x may also be a `sf` list
#'   objects. If x is an `sf` list, additional parameters in ... will be ignored.
#' @param crop  If `TRUE`, x is cropped to y using [sf::st_crop()].
#' @param trim  If `TRUE`, x is trimmed to y with [st_trim()].
#' @param erase If `TRUE`, x is erased by y with [st_erase()].
#' @param crs Coordinate reference system to return.
#' @param .predicate geometry predicate function with the same profile as
#'   [sf::st_intersects()]; see details for [sf::st_filter()] for more options.
#' @param null.ok If y is `NULL` and null.ok is `TRUE`, x is returned without
#'   changes. Defaults to `TRUE`.
#' @inheritDotParams sf::st_filter -x -y
#' @name st_filter_ext
#' @export
st_filter_ext <- function(x,
                          y = NULL,
                          crop = FALSE,
                          trim = FALSE,
                          erase = FALSE,
                          crs = NULL,
                          .predicate = sf::st_intersects,
                          geom_type = NULL,
                          null.ok = TRUE,
                          list.ok = TRUE,
                          ...) {
  if (is.null(y) && null.ok) {
    return(x)
  }

  if (is_sf_list(x, ext = TRUE) && list.ok) {
    x <-
      purrr::map(
        x,
        ~ st_filter_ext(
          x = .x,
          y = y,
          crop = crop,
          trim = trim,
          erase = erase,
          crs = crs,
          .predicate = sf::st_intersects,
          geom_type = geom_type,
          null.ok = null.ok,
          list.ok = list.ok
        )
      )

    return(x)
  }

  cli_abort_ifnot(
    "{.arg x} and {.arg y} must be either sf, sfc or bbox objects.",
    condition = is_sf(x, ext = TRUE) && is_sf(y, ext = TRUE)
  )

  if (is_bbox(x)) {
    x <- as_sfc(x)
  }

  if (erase) {
    cli_warn_ifnot(
      "{.arg erase} is ignored if {.arg trim} or {.arg crop} are {.val TRUE}.",
      condition = !trim | !crop
    )
  }

  y <- st_transform_ext(y, crs = x)

  type <-
    dplyr::case_when(
      crop ~ "crop",
      trim ~ "trim",
      erase ~ "erase",
      TRUE ~ "filter"
    )

  if (crop) {
    y <- sf::st_bbox(y)
  }

  if (is_bbox(y)) {
    y <- as_sfc(y)
  }

  x <-
    sf::st_filter(
      x,
      y,
      ...,
      .predicate = .predicate
    )

  x <-
    switch(type,
      "crop" = suppressWarnings(sf::st_crop(x, y)),
      "trim" = st_trim(x, y),
      "erase" = st_erase(x, y),
      "filter" = x
    )

  if (!is.null(geom_type)) {
    geom_type <- is_geom_type(x, type = geom_type, by_geometry = TRUE)

    if (is_sf(x)) {
      x <- x[geom_type, ]
    } else {
      x <- x[geom_type]
    }
  }

  transform_sf(x, crs = crs)
}
