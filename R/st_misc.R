#' Helper to discard geometry with no dimensions
#'
#' @noRd
discard_na_geom <- function(x) {
  purrr::discard(x, ~ is.na(sf::st_dimension(.x)))
}

#' Modify the geometry of a simple feature or bounding box object
#'
#' @description
#' Support `sf`, `sfc`, and `bbox` and objects as inputs.
#'
#'  - Scale or rotate a simple feature or bounding box object using affine
#'  transformations
#'  - Get the center point for a `sf` object
#'  - Get a circumscribed square or approximate inscribed square in a `sf` object
#'  - Get a circumscribed circle or inscribed circle in a `sf` object
#'  - Get a donut for a `sf` object
#'
#' st_inscribed_square wraps [sf::st_inscribed_circle()] but limits the circle
#' to 1 segment per quadrant (`nQuadSegs = 1`) and then rotates the resulting
#' geometry 45 degrees to provide a (mostly) inscribed square. A different
#' rotation value can be provided to change the orientation of the shape, e.g.
#' `rotate = -45` to return a diamond shape. [st_square()] wraps [st_bbox_ext()] with
#' `asp = 1`.
#'
#' @example examples/st_misc.R
#' @param x A `sf`, `sfc`, or `bbox` object
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
  if ((scale == 1) && (rotate == 0)) {
    return(x)
  }

  x <- as_sf(x)
  crs <- sf::st_crs(x)

  # rotate function (see here: https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

  geom <- as_sfc(x)
  centroid <- suppressWarnings(sf::st_centroid(geom))
  geom <- (geom - centroid) * rot(pi / (360 / (rotate * 2)))
  geom <- geom * scale + centroid

  sf::st_geometry(x) <- geom
  sf::st_set_crs(x, crs)
}


#' @rdname st_misc
#' @name st_center
#' @param ext If `TRUE`, st_center returns a list with the centroid as a `sfc`
#'   object, as an `sf` object (with lon and lat values), the original geometry
#'   (x), and the original crs. objects; defaults TRUE. If `FALSE`, return an `sf`
#'   object.
#' @param ... Additional parameters passed to [sf::st_centroid()]
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
st_square <- function(x, scale = 1, rotate = 0, inscribed = FALSE, by_feature = FALSE) {
  check_sf(x, ext = TRUE)

  if (!is_sf(x)) {
    x <- as_sf(x)
  }

  is_lonlat <- sf::st_is_longlat(x)

  if (is_lonlat) {
    lonlat_crs <- sf::st_crs(x)
    x <- sf::st_transform(x, crs = 3857)
  }

  crs <- sf::st_crs(x)

  if (!by_feature) {
    x <- st_union_ext(x, name_col = NULL)
  }

  if (inscribed) {
    geom <- sf::st_inscribed_circle(as_sfc(x, crs = crs), nQuadSegs = 1)
    geom <- purrr::discard(geom, ~ is.na(sf::st_dimension(.x)))
    rotate <- rotate + 45
  } else {
    geom <-
      purrr::map_dfr(
        sf::st_geometry(x),
        ~ st_bbox_ext(
          x = .x,
          asp = 1,
          class = "sf",
          crs = crs
        )
      )

    geom <- sf::st_as_sfc(geom)
  }

  x <- sf::st_set_geometry(x, geom)

  square <- st_scale_rotate(x, rotate = rotate, scale = scale)

  if (is_lonlat) {
    square <- sf::st_transform(square, crs = lonlat_crs)
  }

  square
}

#' @rdname st_misc
#' @name st_inscribed_square
#' @export
st_inscribed_square <- function(x, scale = 1, rotate = 0, by_feature = FALSE) {
  st_square(x = x, scale = scale, rotate = rotate, inscribed = TRUE, by_feature = by_feature)
}

#' @rdname st_misc
#' @name st_circle
#' @inheritParams sf::st_inscribed_circle
#' @param use_hull For [st_circle()], if `TRUE` use the geometry from
#'   [sf::st_convex_hull()] (to address issues with MULTIPOLYGON objects).
#' @param use_lwgeom If `TRUE`, `by_feature = TRUE` and `inscribed = FALSE`, use
#'   [lwgeom::st_minimum_bounding_circle()].
#' @export
#' @importFrom sf st_inscribed_circle
st_circle <- function(x, scale = 1, inscribed = TRUE, dTolerance = 0.01, by_feature = FALSE, use_hull = FALSE, use_lwgeom = FALSE) {
  if (!is_sf(x)) {
    x <- as_sf(x)
  }

  if (by_feature) {
    if (use_lwgeom && !inscribed) {
      is_pkg_installed("lwgeom")
      return(lwgeom::st_minimum_bounding_circle(x))
    }

    x$st_circle_id <- seq(nrow(x))
    x <- as_sf_list(x, col = "st_circle_id")

    x <-
      purrr::map(
        x,
        ~ st_circle(
          x = .x,
          scale = scale,
          inscribed = inscribed,
          dTolerance = dTolerance,
          by_feature = FALSE
        )
      )

    x <- as_sf(x)
    x$st_circle_id <- NULL

    return(x)
  }

  crs <- sf::st_crs(x)
  is_lonlat <- sf::st_is_longlat(x)

  if (is_lonlat) {
    lonlat_crs <- sf::st_crs(x)
    x <- sf::st_transform(x, crs = 3857)
  }

  geom <- sf::st_union(sf::st_combine(x))

  if (use_hull) {
    if (is_multipolygon(geom)) {
      geom <- sf::st_cast(geom, to = "MULTIPOINT")
    }
    geom <- sf::st_convex_hull(geom)
  }

  if (inscribed) {
    geom <- sf::st_inscribed_circle(geom, dTolerance = dTolerance)
    geom <- discard_na_geom(geom)
  } else {
    radius <- sf_bbox_diagdist(as_bbox(geom), drop = TRUE) / 2
    geom <-
      sf::st_buffer(
        x = sf::st_centroid(geom),
        dist = radius * scale,
        units = get_dist_units(x)
      )
  }

  if (nrow(x) != length(geom)) {
    x <- st_union_ext(x, name_col = NULL)
  }

  sf::st_geometry(x) <- geom

  if (is_lonlat) {
    x <- sf::st_transform(x, crs = lonlat_crs)
  }

  x
}

#' @rdname st_misc
#' @name st_circumscribed_circle
#' @export
st_circumscribed_circle <- function(x, scale = 1, dTolerance = 0, by_feature = FALSE) {
  st_circle(x = x, scale = scale, inscribed = FALSE, dTolerance = dTolerance, by_feature = by_feature)
}

#' @rdname st_misc
#' @name st_donut
#' @param width Donut width as proportion of outer size.
#' @export
#' @importFrom sf st_inscribed_circle
st_donut <- function(x, width = 0.4, scale = 1, by_feature = FALSE, ...) {
  crs <- sf::st_crs(x)

  outer <- st_circle(x, scale = scale, by_feature = by_feature, ...)
  inner <- st_circle(x, scale = (scale * width), by_feature = by_feature, ...)

  if (!by_feature) {
    return(st_erase(outer, inner, union = FALSE))
  }

  donut <-
    purrr::map2_dfr(
      sf::st_geometry(outer),
      sf::st_geometry(inner),
      ~ as_sf(st_erase(as_sfc(.x, crs = crs), as_sfc(.y, crs = crs)))
    )

  sf::st_set_geometry(outer, as_sfc(donut))
}
