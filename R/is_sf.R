#' What is the class or spatial attributes of this feature?
#'
#' @param x An `sf`, `sfc`, or `bbox` object.
#' @param y An sf object or a character or numeric object supported by
#'   [sf::st_crs] that can be compared to x. (used by [is_same_crs])
#' @param ext If `TRUE`, check if x is a `sf`, `sfc`, or `bbox` class object or
#'   not; defaults to `FALSE`. (used by [is_sf])
#' @param null.ok If `TRUE` and x is `NULL`, return `TRUE`; defaults to `FALSE`.
#' @param list.ok If `TRUE`, [is_sf] will return TRUE if x is a list of sf objects.
#' @details
#' - [is_sf]: is x a `sf` class object?
#' - [is_sfc]: is x is a `sfc` class object?
#' - [is_bbox]: is x is a `bbox` class object?
#' - [is_sf_list]: is x is a list of `sf` class objects (with or without names)?
#' - [is_raster]: is x a `Raster` class object?
#' - [is_sp]: is x a `Spatial` class object of any type?
#' - [is_same_crs]: do x and y have the same coordinate reference system?
#'
#' @export
#' @md
is_sf <- function(x, ext = FALSE, null.ok = FALSE, list.ok = FALSE) {
  classes <- "sf"

  if (ext) {
    classes <- c(classes, "sfc", "bbox")
  }

  if (!list.ok) {
    return(is_class(x, classes = classes, null.ok = null.ok))
  }

  is_class(x, classes = classes, null.ok = null.ok) | is_sf_list(x, ext = ext, null.ok = null.ok)
}

#' @name is_sfg
#' @rdname is_sf
#' @export
is_sfg <- function(x, null.ok = FALSE) {
  is_class(x, classes = "sfg", null.ok = null.ok)
}

#' @name is_sfc
#' @rdname is_sf
#' @export
is_sfc <- function(x, null.ok = FALSE) {
  is_class(x, classes = "sfc", null.ok = null.ok)
}

#' @name is_bbox
#' @rdname is_sf
#' @export
is_bbox <- function(x, null.ok = FALSE) {
  is_class(x, classes = "bbox", null.ok = null.ok)
}

#' @rdname is_sf
#' @name is_sf_list
#' @param named If `TRUE`, check if sf list is named; defaults `FALSE`.
#' @export
is_sf_list <- function(x, named = FALSE, ext = FALSE, null.ok = FALSE) {
  if (is.null(x) && null.ok) {
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
          is_sf(x, ext = ext, null.ok = null.ok)
        },
        TRUE
      )
    )

  if (!named) {
    return(is_sf_list)
  }

  is_sf_list && is_named(x)
}

#' @name is_raster
#' @rdname is_sf
#' @export
is_raster <- function(x, null.ok = FALSE) {
  is_class(x, classes = "RasterLayer", null.ok = null.ok)
}

#' @name is_sp
#' @rdname is_sf
#' @export
is_sp <- function(x, null.ok = FALSE) {
  if (is.null(x) && null.ok) {
    return(TRUE)
  }

  any(grepl("Spatial", class(x)))
}

#' @name is_same_crs
#' @rdname  is_sf
#' @importFrom sf st_crs
#' @export
is_same_crs <- function(x, y) {
  sf::st_crs(x) == sf::st_crs(y)
}
