#' What geometry type is this feature?
#'
#' @description
#' [is_geom_type()] extends [sf::st_geometry_type()] to return a list with the
#' standard output ("TYPES") and additional list values with logical vectors
#' from [sf::st_is()] for "POINTS" (passing "POINT" and "MULTIPOINT" as type),
#' "POLYGONS" (passing "POLYGON", "MULTIPOLYGON"), "LINESTRINGS" ("LINESTRING"
#' and "MULTILINESTRING"), "GEOMETRYCOLLECTION" and "OTHER".
#'
#' [st_is_ext()] adds a by_geometry argument that passes the results of
#' [sf::st_is()] to [all()] if by_geometry is `FALSE`.
#'
#' @name is_geom_type
#' @param x A `sf` or `sfc` object passed to [sf::st_geometry_type()]
#' @param type If "POINT", check if geometry type is POINT. Same for all
#'   available geometry types; not case sensitive; Default: `NULL`
#' @param by_geometry Passed to [sf::st_geometry_type()]; defaults to `FALSE`
#' @param ext For [is_geom_type()], if ext `TRUE` and check is `NULL`, return a list
#'   with checks for POINTS, POLYGONS, LINESTRING, and the returned types.
#' @returns
#' - If ext is `FALSE` and type is `NULL`, returns vector with geometry types
#' identical to [sf::st_geometry_type()].
#' - If ext is `TRUE`, returns a list and, if type is not `NULL`, returns a
#' logical vector.
#' @export
#' @importFrom sf st_geometry_type st_is
is_geom_type <- function(x, type = NULL, by_geometry = FALSE, ext = TRUE) {
  if (!is_sf(x) && !is_sfc(x) && !is_sfg(x)) {
    return(FALSE)
  }

  if (!is.null(type)) {
    return(st_is_ext(x, type, by_geometry))
  }

  if (!ext) {
    return(sf::st_geometry_type(x, by_geometry = by_geometry))
  }

  list(
    "TYPES" = sf::st_geometry_type(x, by_geometry = by_geometry),
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
  st_is_ext(
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
  st_is_ext(
    x = x,
    type = "LINESTRING",
    by_geometry = by_geometry
  )
}

#' @name is_multiline
#' @rdname is_geom_type
#' @export
is_multiline <- function(x, by_geometry = FALSE) {
  st_is_ext(
    x = x,
    type = "MULTILINESTRING",
    by_geometry = by_geometry
  )
}

#' @name is_polygon
#' @rdname is_geom_type
#' @export
is_polygon <- function(x, by_geometry = FALSE) {
  st_is_ext(
    x = x,
    type = "POLYGON",
    by_geometry = by_geometry
  )
}


#' @name is_multipolygon
#' @rdname is_geom_type
#' @export
is_multipolygon <- function(x, by_geometry = FALSE) {
  st_is_ext(
    x = x,
    type = "MULTIPOLYGON",
    by_geometry = by_geometry
  )
}


#' @name st_is_ext
#' @rdname is_geom_type
#' @export
st_is_ext <- function(x, type = NULL, by_geometry = FALSE) {
  if (!is_sf(x) && !is_sfc(x) && !is_sfg(x)) {
    return(FALSE)
  }

  if (by_geometry) {
    return(sf::st_is(x, type))
  }

  all(sf::st_is(x, type))
}
