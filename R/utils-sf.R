#' Additional utility functions for sf objects
#'
#' @description
#' - [transform_sf()] is similar to [sf::st_transform()] but supports sf, sfc,
#' or bbox objects as the crs parameter, supports sfg objects (transformed to
#' sfc), and uses [sf::st_set_crs()] if the CRS for the provided object is `NA`.
#' This function does not support bounding box transformations.
#' - [relocate_sf_col()] relocates the sf geometry column after specified column
#' (by default after everything).
#' - [rename_sf_col()] a wrapper for [sf::st_set_geometry()] that renames the sf
#' column.
#' - [get_sf_col()] returns the "sf_column" attribute.
#' - [get_sf_colnames()] returns the column names of a file that can be read
#' with [sf::read_sf()] to allow you to use column names to build a query (or
#' provide a value for name_col) without reading the whole file.
#' @name misc_sf
#' @param ... Additional parameters passed to [sf::st_transform()] by
#'   [transform_sf()] or to [sf::read_sf()] by [get_sf_colnames()] if x is not
#'   `NULL`.
NULL

#' @name transform_sf
#' @rdname misc_sf
#' @param x A `sf` or `sfc` object. If x has a missing crs, the crs is set to
#'   the provided value.
#' @param crs A coordinate reference system identifier (numeric or character) or
#'   a `sf`, `sfc`, `bbox`, or `crs` class object supported by [sf::st_crs()].
#' @param allow_null If `TRUE` and crs is `NULL`, return x.
#' @export
#' @importFrom sf st_crs st_set_crs st_transform
transform_sf <- function(x, crs = NULL, allow_null = TRUE, ...) {
  if (allow_null && is_null(crs)) {
    return(x)
  }

  crs <- as_crs(crs, check = TRUE)

  if (is_same_crs(x, crs)) {
    return(x)
  }

  if (is_sfg(x)) {
    x <- as_sfc(x)
  }

  if (is.na(sf::st_crs(x))) {
    return(sf::st_set_crs(x, crs))
  }

  sf::st_transform(x, crs, ...)
}

#' @name relocate_sf_col
#' @rdname misc_sf
#' @param .after The location to place sf column after; defaults to
#'   [dplyr::everything()].
#' @export
#' @importFrom dplyr everything relocate all_of
relocate_sf_col <- function(x, .after = dplyr::everything()) {
  suppressWarnings(
    # FIXME: I thought I refactored this correctly but continued to get the
    # tidyselect error on tests so I'm suppressing warnings for now
    dplyr::relocate(
      x,
      dplyr::all_of(get_sf_col(x)),
      .after = .after
    )
  )
}

#' @name rename_sf_col
#' @rdname misc_sf
#' @param sf_col Name to use for the sf column after renaming; defaults to
#'   "geometry".
#' @export
#' @importFrom sf st_set_geometry
rename_sf_col <- function(x, sf_col = "geometry") {
  sf::st_set_geometry(x, value = sf_col)
}

#' @name get_sf_col
#' @rdname misc_sf
#' @export
get_sf_col <- function(x = NULL) {
  attr(x, "sf_column")
}

#' @name get_sf_colnames
#' @rdname misc_sf
#' @inheritParams sf::read_sf
#' @export
#' @importFrom sf st_layers read_sf
get_sf_colnames <- function(x = NULL, dsn = NULL, layer = NULL, ...) {
  # Based on https://mastodon.sdf.org/@caleb/109286477381437051
  if (is_null(x) && !is_null(dsn)) {
    layer <- match.arg(layer, sf::st_layers(dsn)[["name"]])
    x <-
      sf::read_sf(
        dsn,
        query = paste("select * from", layer, "limit 1"),
        ...
      )
  }
  colnames(x)
}
