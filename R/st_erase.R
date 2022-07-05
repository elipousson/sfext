
#' Erase an area of a simple feature object
#'
#' An extended version of [sf::st_difference] that uses the flip parameter to
#' optionally call [sf::st_intersection] and combines and unions the second
#' object by default. [st_trim] is [st_erase] with flip set to `TRUE`.
#'
#' @param x A sf, sfc, or bbox object to erase or trim.
#' @param y A sf, sfc, or bbox object to use to erase or trim.
#' @param flip If `TRUE`, use st_intersection; if `FALSE` use st_difference,
#'   Default: `FALSE`
#' @param union If `TRUE`, use [sf::st_combine] and [sf::st_union] on y before
#'   applying difference/intersection, Default: `FALSE`
#' @export
#' @importFrom sf st_union st_combine st_intersection st_difference
st_erase <- function(x, y, flip = FALSE, union = TRUE) {
  check_sf(x, ext = TRUE)

  if (is_bbox(x)) {
    x <- as_sf(x)
  }

  check_sf(y, ext = TRUE)

  is_lonlat <- sf::st_is_longlat(x)

  if (is_lonlat) {
    lonlat_crs <- sf::st_crs(x)
    x <- st_transform_ext(x, crs = 3857)
  }

  y <- st_transform_ext(y, crs = x, class = "sfc")

  if (union) {
    y <- sf::st_union(sf::st_combine(y))
  }

  if (flip) {
    x <- suppressWarnings(sf::st_intersection(x, y))
  } else {
    x <- suppressWarnings(sf::st_difference(x, y))
  }

  if (is_lonlat) {
    x <- st_transform_ext(x, crs = lonlat_crs)
  }

  x
}

#' @rdname st_erase
#' @name st_trim
#' @export
st_trim <- function(x, y, union = TRUE) {
  st_erase(
    x = x,
    y = y,
    flip = TRUE,
    union = union
  )
}
