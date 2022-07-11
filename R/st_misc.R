#' Modify the geometry of a simple feature or bounding box object
#'
#' Support both `bbox` and `sf` objects as inputs.
#'
#'  - Scale or rotate a simple feature or bounding box object using affine
#'  transformations
#'  - Get the center point for a `sf` object
#'  - Get a circumscribed square or approximate inscribed square in a `sf` object
#'  - Get a circumscribed circle or inscribed circle in a `sf` object
#'
#' st_inscribed_square wraps `sf::st_inscribed_circle()` but limits the circle
#' to 1 segment per quadrant (`nQuadSegs = 1`) and then rotates the resulting
#' geometry 45 degrees to provide a (mostly) inscribed square. A different
#' rotation value can be provided to change the orientation of the shape, e.g.
#' `rotate = -45` to return a diamond shape. [st_square()] wraps [st_bbox_ext()] with
#' `asp = 1`.
#' @example examples/st_misc.R
#' @param x A sf, sfc, or bbox object
#' @param scale numeric; scale factor, Default: 1
#' @param rotate numeric; degrees to rotate (-360 to 360), Default: 0
#' @param inscribed If `TRUE`, make circle or square inscribed within x, if
#'   `FALSE`, make it circumscribed.
#' @seealso
#'  - [sf::geos_unary]
#' @name st_misc
#' @md
NULL

#' @rdname st_misc
#' @name st_scale_rotate
#' @export
#' @importFrom sf st_geometry st_crs
st_scale_rotate <- function(x, scale = 1, rotate = 0) {
  x <- as_sf(x)
  crs <- sf::st_crs(x)

  # rotate function (see here: https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

  geometry <- as_sfc(x)
  centroid <- suppressWarnings(sf::st_centroid(geometry))
  geometry <- (geometry - centroid) * rot(pi / (360 / (rotate * 2)))
  geometry <- geometry * scale + centroid

  sf::st_geometry(x) <- geometry
  sf::st_crs(x) <- crs

  x
}


#' @rdname st_misc
#' @name st_center
#' @param ext If `TRUE`, st_center returns a list with the centroid as a sfc
#'   object, as an sf object (with lon and lat values), the original geometry
#'   (x), and the original crs. objects; defaults TRUE. If `FALSE`, return an sf
#'   object.
#' @param ... Additional parameters passed to `sf::st_centroid()` by st_center
#' @export
#' @importFrom sf st_crs st_geometry st_centroid st_sf
st_center <- function(x,
                      ext = TRUE,
                      ...) {
  x <- as_sf(x)
  geometry <- as_sfc(x)
  centroid <- suppressWarnings(sf::st_centroid(geometry, ...))

  if (!ext) {
    return(centroid)
  }

  list(
    "sfc" = centroid, # sfc based on centroid
    "sf" = get_coords(as_sf(centroid), drop = FALSE), # sf based on centroid (won't include original cols)
    "geometry" = geometry, # original geometry (sfc)
    "x" = x, # original object
    "crs" = sf::st_crs(x) # original crs
  )
}

#' @rdname st_misc
#' @name st_square
#' @export
#' @importFrom sf st_is_longlat st_inscribed_circle st_geometry st_dimension st_set_geometry
#' @importFrom purrr discard
st_square <- function(x, scale = 1, rotate = 0, inscribed = FALSE) {
  check_sf(x, ext = TRUE)

  if (!is_sf(x)) {
    x <- as_sf(x)
  }

  is_lonlat <- sf::st_is_longlat(x)

  if (is_lonlat) {
    crs <- sf::st_crs(x)
    x <- sf::st_transform(x, crs = 3857)
  }

  if (inscribed) {
    geom <- sf::st_inscribed_circle(as_sfc(x), nQuadSegs = 1)
    geom <- purrr::discard(geom, ~ is.na(sf::st_dimension(.x)))
    x <- sf::st_set_geometry(x, geom)
    rotate <- rotate + 45
  } else {
    x <-
      st_bbox_ext(
        x = x,
        asp = 1,
        class = "sf"
      )
  }

  square <- st_scale_rotate(x, rotate = rotate, scale = scale)

  if (is_lonlat) {
    square <- sf::st_transform(square, crs = crs)
  }

  return(square)
}

#' @rdname st_misc
#' @name st_inscribed_square
#' @export
st_inscribed_square <- function(x, scale = 1, rotate = 0) {
  st_square(x = x, scale = scale, rotate = rotate, inscribed = TRUE)
}

#' @rdname st_misc
#' @name st_circle
#' @inheritParams sf::st_inscribed_circle
#' @export
#' @importFrom sf st_inscribed_circle
st_circle <- function(x, scale = 1, inscribed = FALSE, dTolerance = 0) {
  check_sf(x, ext = TRUE)

  if (!is_sf(x)) {
    x <- as_sf(x)
  }

  if (inscribed) {
    circle <-
      sf::st_inscribed_circle(
        x = sf::st_union(x),
        dTolerance = dTolerance
      )

    return(st_scale_rotate(circle, scale = scale))
  }

  radius <- sf_bbox_diagdist(as_bbox(x)) / 2
  center <- st_center(x, ext = FALSE)

  st_buffer_ext(
    x = as_sf(center),
    dist = radius * scale
  )
}

#' @rdname st_misc
#' @name st_circumscribed_circle
#' @export
st_circumscribed_circle <- function(x, scale = 1) {
  st_circle(x = x, scale = scale, inscribed = FALSE)
}
