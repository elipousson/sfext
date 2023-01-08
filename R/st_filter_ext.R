#' Filter, crop, trim, or erase a simple feature object or list
#'
#' Extend [sf::st_filter()] to filter a sf list or a `sf`, `sfc`, or `bbox` with
#' options to crop, trim or erase the geometry of the input object based on a
#' predicate function.
#'
#' @param x,y A `sf`, `sfc`, or `bbox` object. x may also be a `sf` list
#'   objects. If x is an `sf` list, additional parameters in `...` will be
#'   ignored.
#' @param crop  If `TRUE`, x is cropped to y using [sf::st_crop()].
#' @param trim  If `TRUE`, x is trimmed to y with [st_trim()].
#' @param erase If `TRUE`, x is erased by y with [st_erase()].
#' @param crs Coordinate reference system to return.
#' @param .predicate geometry predicate function with the same profile as
#'   [sf::st_intersects()]; see details for [sf::st_filter()] for more options.
#' @param type Character string passed to type argument of [sf::st_is()]
#'   to filter features to only those matching the specified geometry type.
#' @param null.ok If `y = NULL` and `null.ok = TRUE`, x is returned without
#'   changes. Defaults to `TRUE`.
#' @param list.ok If `TRUE`, x can be a list of `sf`, `sfc`, or `bbox` objects.
#'   If `FALSE`, only `sf`, `sfc`, or `bbox` objects are supported. Defaults to
#'   `TRUE`.
#' @inheritDotParams sf::st_filter -x -y
#' @name st_filter_ext
#' @export
#' @importFrom sf st_intersects st_bbox st_filter st_crop
#' @importFrom purrr map
#' @importFrom dplyr case_when
st_filter_ext <- function(x,
                          y = NULL,
                          crop = FALSE,
                          trim = FALSE,
                          erase = FALSE,
                          crs = NULL,
                          .predicate = sf::st_intersects,
                          type = NULL,
                          list.ok = TRUE,
                          ...) {
  if (is.null(y)) {
    if (!is.null(type)) {
      return(st_filter_geom_type(transform_sf(x, crs = crs), type = type))
    }
    return(transform_sf(x, crs = crs))
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
          type = type,
          list.ok = list.ok
        )
      )

    return(x)
  }

  cli_abort_ifnot(
    "Both {.arg x} and {.arg y} must be either sf, sfc or bbox objects.",
    condition = is_sf(x, ext = TRUE) && is_sf(y, ext = TRUE)
  )

  if (is_bbox(x)) {
    x <- sf_bbox_to_sfc(x)
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

  x <-
    sf::st_filter(
      x,
      as_sfc(y),
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

  x <- st_filter_geom_type(x, type)

  transform_sf(x, crs = crs)
}

#' Filter by geometry type
#'
#' @rdname st_filter_geom_type
#' @name st_filter_ext
#' @param type Geometry type.
#' @export
#' @importFrom sf st_is
st_filter_geom_type <- function(x, type = NULL) {
  cli_abort_ifnot(
    "{.arg x} must be an {.cls sf} or {.cls sfc} object.",
    condition = is_sf(x) | is_sfc(x)
  )

  if (is.null(type)) {
    return(x)
  }

  match <- sf::st_is(x, type)

  if (is_sf(x)) {
    return(x[match, ])
  }

  if (is_sfc(x)) {
    x[match]
  }
}
