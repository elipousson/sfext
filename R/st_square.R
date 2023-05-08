#' Create a square within or around a simple feature object or collection
#'
#' Get a circumscribed square or approximate inscribed square in a `sf` object.
#' If inscribed is `TRUE`, the square geometry returned may not be contained
#' wholly within the original geometry. The inscribed square is created from the
#' bounding box of an inscribed circle rotated 45 degrees.
#'
#' @inheritParams st_scale_rotate
#' @param inscribed If `TRUE`, the returned geometry is inscribed within x, if
#'   `FALSE` (default), the geometry is circumscribed.
#' @param by_feature If `TRUE`, create new geometry for each feature. If
#'   `FALSE`, create new geometry for all features combine with
#'   [st_union_ext()].
#' @examples
#' nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
#' nc <- sf::st_transform(nc, crs = 3857)
#'
#' plot(st_square(nc), max.plot = 1)
#' plot(st_square(nc[1:10,], by_feature = TRUE), max.plot = 1)
#'
#' @export
#' @importFrom sf st_is_longlat st_inscribed_circle st_geometry st_dimension
#'   st_set_geometry
st_square <- function(x,
                      scale = 1,
                      rotate = 0,
                      inscribed = FALSE,
                      by_feature = FALSE,
                      call = caller_env()) {
  check_sf(x, ext = TRUE)
  UseMethod("st_square")
}

#' @name st_square
#' @export
st_square.default <- function(x,
                              ...) {
  st_square.sfc(as_sfc(x), ...)
}

#' @name st_square
#' @export
st_square.sfc <- function(x,
                          scale = 1,
                          rotate = 0,
                          inscribed = FALSE,
                          by_feature = FALSE,
                          call = caller_env()) {
  is_lonlat <- sf::st_is_longlat(x)

  if (is_lonlat) {
    lonlat_crs <- sf::st_crs(x)
    x <- sf::st_transform(x, crs = 3857)
  }

  crs <- sf::st_crs(x)

  if (!by_feature) {
    x <- sf::st_union(x)
  }

  if (inscribed) {
    geometry <- sf::st_inscribed_circle(x, nQuadSegs = 1)
    geometry <- discard(geometry, ~ is.na(sf::st_dimension(.x)))
    rotate <- rotate + 45
  } else {
    geometry <-
      map(
        vctrs::vec_chop(x),
        ~ st_bbox_ext(
          x = .x,
          asp = 1,
          class = "sfc",
          crs = crs
        )
      )

    geometry <- vctrs::list_unchop(geometry)
  }

  geometry <- st_scale_rotate(geometry, rotate = rotate, scale = scale, call = call)

  if (!is_lonlat) {
    return(geometry)
  }

  sf::st_transform(geometry, crs = lonlat_crs)
}

#' @name st_square
#' @export
st_square.sf <- function(x,
                         ...,
                         by_feature = FALSE) {
  if (!by_feature) {
    x <- st_union_ext(x, name_col = NULL)
  }

  sf::st_set_geometry(
    x,
    st_square.sfc(sf::st_geometry(x), ..., by_feature = by_feature)
  )
}

#' @name st_square
#' @export
st_square.bbox <- function(x,
                           ...) {
  sf::st_bbox(st_square.sfc(sf::st_as_sfc(x), ...))
}

#' @rdname st_square
#' @name st_inscribed_square
#' @export
st_inscribed_square <- function(x,
                                scale = 1,
                                rotate = 0,
                                by_feature = FALSE) {
  st_square(
    x = x,
    scale = scale,
    rotate = rotate,
    inscribed = TRUE,
    by_feature = by_feature
  )
}
