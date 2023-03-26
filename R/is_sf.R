#' What is the class or spatial attributes of this feature?
#'
#' @param x An `sf`, `sfc`, or `bbox` object.
#' @param y An sf object or a character or numeric object supported by
#'   [sf::st_crs] that can be compared to x. (used by [is_same_crs])
#' @param ext If `TRUE`, check if x is a `sf`, `sfc`, or `bbox` class object or
#'   not; defaults to `FALSE`. (used by [is_sf])
#' @param allow_null If `TRUE` and x is `NULL`, return `TRUE`; defaults to `FALSE`.
#' @param allow_list If `TRUE`, [is_sf] will return TRUE if x is a list of sf objects.
#' @details
#' - [is_sf]: is x a `sf` class object?
#' - [is_sfc]: is x is a `sfc` class object?
#' - [is_bbox]: is x is a `bbox` class object?
#' - [is_sf_list]: is x is a list of `sf` class objects (with or without names)?
#' - [is_raster]: is x a `Raster` class object?
#' - [is_sp]: is x a `Spatial` class object of any type?
#' - [is_geo_coords]: is x likely a geodetic coordinate pair (a length 2 numeric vector, with a max absolute value less than or equal to 180)?
#' - [is_same_crs]: do x and y have the same coordinate reference system?
#'
#' @export
#' @md
is_sf <- function(x, ext = FALSE, allow_null = FALSE, allow_list = FALSE) {
  classes <- "sf"

  if (isTRUE(ext)) {
    classes <- c(classes, "sfc", "bbox")
  } else if (is.character(ext)) {
    classes <- c(classes, ext)
  }

  if (isFALSE(allow_list)) {
    return(is_class(x, classes = classes, allow_null = allow_null))
  }

  is_class(x, classes = classes, allow_null = allow_null) | is_sf_list(x, ext = ext, allow_null = allow_null)
}

#' @name is_sfg
#' @rdname is_sf
#' @export
is_sfg <- function(x, allow_null = FALSE) {
  is_class(x, classes = "sfg", allow_null = allow_null)
}

#' @name is_sfc
#' @rdname is_sf
#' @export
is_sfc <- function(x, allow_null = FALSE) {
  is_class(x, classes = "sfc", allow_null = allow_null)
}

#' @name is_bbox
#' @rdname is_sf
#' @export
is_bbox <- function(x, allow_null = FALSE) {
  is_class(x, classes = "bbox", allow_null = allow_null)
}

#' @rdname is_sf
#' @name is_sf_list
#' @param named If `TRUE`, check if sf list is named; defaults `FALSE`.
#' @export
is_sf_list <- function(x, named = FALSE, ext = FALSE, allow_null = FALSE) {
  if (is.null(x) && isTRUE(allow_null)) {
    return(TRUE)
  }

  if (is_sf(x, ext = TRUE)) {
    return(FALSE)
  }

  is_sf_list <-
    is.list(x) && all(
      vapply(
        x,
        function(x) {
          is_sf(x, ext = ext, allow_null = allow_null)
        },
        TRUE
      )
    )

  if (isFALSE(named)) {
    return(is_sf_list)
  }

  isTRUE(is_sf_list) && is_named(x)
}

#' @name is_raster
#' @rdname is_sf
#' @export
is_raster <- function(x, allow_null = FALSE) {
  is_class(x, classes = "RasterLayer", allow_null = allow_null)
}

#' @name is_sp
#' @rdname is_sf
#' @export
is_sp <- function(x, allow_null = FALSE) {
  if (is.null(x) && isTRUE(allow_null)) {
    return(TRUE)
  }

  any(grepl("Spatial", class(x)))
}

#' @name is_coords
#' @rdname is_sf
#' @export
is_geo_coords <- function(x, allow_null = FALSE) {
  if (is.null(x) && isTRUE(allow_null)) {
    return(TRUE)
  }

  is.numeric(x) && (length(x) == 2) && (max(abs(x)) <= 180)
}

#' @name is_same_crs
#' @rdname  is_sf
#' @importFrom sf st_crs
#' @export
is_same_crs <- function(x, y) {
  sf::st_crs(x) == sf::st_crs(y)
}
