#' Additional utility functions for sf objects
#'
#' @name misc_sf
NULL

#' @name transform_sf
#' @rdname misc_sf
#' @param x A `sf` object.
#' @param crs A coordinate reference system or an sf, sfc, or bbox object
#'   returns a crs from [sf::st_crs].
#' @param null.ok If `TRUE` and x is `NULL`, return x.
#' @param ... Additional parameters passed to [sf::st_transform]
#' @export
#' @importFrom sf st_crs st_transform
transform_sf <- function(x, crs = NULL, null.ok = TRUE, ...) {
  if ((is.null(crs) && null.ok) | is_same_crs(x, crs)) {
    return(x)
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
    dplyr::all_of(attributes(x)$sf_column),
    .after = .after
  )
}

#' @name rename_sf_col
#' @rdname misc_sf
#' @param sf_col Name to use for the sf column after renaming; defaults to "geometry".
#' @export
rename_sf_col <- function(x, sf_col = "geometry") {
  check_null(sf_col)

  names(x)[names(x) == attr(x, "sf_column")] <- sf_col
  attr(x, "sf_column") <- sf_col

  x
}
