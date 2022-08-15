#' Additional utility functions for sf objects
#'
#' @name misc_sf
NULL

#' @name transform_sf
#' @rdname misc_sf
#' @param x A `sf` or `sfc` object.
#' @param crs A coordinate reference system identifier (numeric or character) or
#'   a `sf`, `sfc`, `bbox`, or `crs` class object supported by [sf::st_crs()].
#' @param null.ok If `TRUE` and x is `NULL`, return x.
#' @param ... Additional parameters passed to [sf::st_transform()]
#' @export
#' @importFrom sf st_crs st_transform
transform_sf <- function(x, crs = NULL, null.ok = TRUE, ...) {
  if ((is.null(crs) && null.ok) | is_same_crs(x, crs)) {
    return(x)
  }

  if (is_sfg(x)) {
    x <- as_sfc(x)
  }

  if (is.na(sf::st_crs(x))) {
    sf::st_crs(x) <- crs
    return(x)
  }

  if (is_sf(crs, ext = TRUE)) {
    crs <- sf::st_crs(x = crs)
  }

  sf::st_transform(x, crs, ...)
}

#' @name relocate_sf_col
#' @rdname misc_sf
#' @param .after The location to place sf column after; defaults to
#'   [dplyr::everything].
#' @export
#' @importFrom dplyr everything relocate all_of
relocate_sf_col <- function(x, .after = dplyr::everything()) {
  dplyr::relocate(
    x,
    dplyr::all_of(get_sf_col(x)),
    .after = .after
  )
}

#' @name rename_sf_col
#' @rdname misc_sf
#' @param sf_col Name to use for the sf column after renaming; defaults to "geometry".
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
