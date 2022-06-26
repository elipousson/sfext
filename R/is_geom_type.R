#' What geometry type is this feature?
#'
#' A flexible wrapper for [sf::st_geometry_type] and [sf::st_is].
#'
#' @name is_geom_type
#' @param x A `sf` or `sfc` object passed to [sf::st_geometry_type]
#' @param type If "POINT", check if geometry type is POINT. Same for all
#'   available geometry types; not case sensitive; Default: `NULL`
#' @param by_geometry Passed to [sf::st_geometry_type]; defaults to `FALSE`
#' @param ext For st_geom_type, if ext `TRUE` and check is `NULL`, return a list
#'   with checks for POINTS, POLYGONS, LINESTRING, and the returned types.
#' @returns If ext is FALSE and type is NULL, returns vector with geometry types
#'   identical to [sf::st_geometry_type]. If ext is `TRUE`, returns a list and,
#'   if type is not `NULL`, returns a logical vector.
#' @export
#' @importFrom sf st_geometry_type st_is
is_geom_type <- function(x, type = NULL, by_geometry = FALSE, ext = TRUE) {
  if (!is_sf(x, ext = TRUE) && !is_sfg(x)) {
    return(FALSE)
  }

  if (!is.null(type)) {
    geom_type <- sf::st_is(x, type)

    if (!by_geometry) {
      geom_type <- all(geom_type)
    }

    return(geom_type)
  }

  geom_type <-
    sf::st_geometry_type(x, by_geometry = by_geometry)

  if (!ext) {
    return(geom_type)
  }

  list(
    "TYPES" = geom_type,
    "POINTS" = sf::st_is(x, c("POINT", "MULTIPOINT")),
    "POLYGONS" = sf::st_is(x, c("POLYGON", "MULTIPOLYGON")),
    "LINESTRINGS" = sf::st_is(x, c("LINESTRING", "MULTILINESTRING")),
    "COLLECTION" = sf::st_is(x, "GEOMETRYCOLLECTION"),
    "OTHER" = sf::st_is(x, c(
      "GEOMETRY", "CIRCULARSTRING", "COMPOUNDCURVE",
      "CURVEPOLYGON", "MULTICURVE", "MULTISURFACE",
      "CURVE", "SURFACE", "POLYHEDRALSURFACE",
      "TIN", "TRIANGLE"
    ))
  )
}

#' @name is_point
#' @rdname is_geom_type
#' @export
is_point <- function(x, by_geometry = FALSE) {
  is_geom_type(
    x = x,
    type = "POINT",
    by_geometry = by_geometry
  )
}

#' @name is_multipoint
#' @rdname is_geom_type
#' @export
is_multipoint <- function(x, by_geometry = FALSE) {
  is_geom_type(
    x = x,
    type = "MULTIPOINT",
    by_geometry = by_geometry
  )
}

#' @name is_line
#' @rdname is_geom_type
#' @export
is_line <- function(x, by_geometry = FALSE) {
  is_geom_type(
    x = x,
    type = "LINESTRING",
    by_geometry = by_geometry
  )
}

#' @name is_multiline
#' @rdname is_geom_type
#' @export
is_multiline <- function(x, by_geometry = FALSE) {
  is_geom_type(
    x = x,
    type = "MULTILINESTRING",
    by_geometry = by_geometry
  )
}

#' @name is_polygon
#' @rdname is_geom_type
#' @export
is_polygon <- function(x, by_geometry = FALSE) {
  is_geom_type(
    x = x,
    type = "POLYGON",
    by_geometry = by_geometry
  )
}


#' @name is_multipolygon
#' @rdname is_geom_type
#' @export
is_multipolygon <- function(x, by_geometry = FALSE) {
  is_geom_type(
    x = x,
    type = "MULTIPOLYGON",
    by_geometry = by_geometry
  )
}
