#' Cast geometry to another type
#'
#' Wrapper for [sf::st_cast()] that currently supports casting MULTIPOLYGON to
#' POLYGON or MULTIPOLYGON or POLYGON to POINT or, if simplify = FALSE, can cast
#' MULTIPOINT to LINESTRING. This is not very different than the basic
#' functionality of st_cast but further development may improve the utility of
#' this function.
#'
#' @param x A `sf` or `sfc` object to cast to another type.
#' @inheritParams sf::st_cast
#' @param simplify If TRUE, simplify geometry type; defaults to TRUE.
#' @seealso
#'  [sf::st_cast()]
#' @rdname st_cast_ext
#' @export
#' @importFrom sf st_cast
st_cast_ext <- function(x, to = "POINT", simplify = TRUE, ...) {
  geom_type <- is_geom_type(x, ext = FALSE)

  if (any(geom_type %in% c("MULTIPOLYGON", "POLYGON")) && simplify) {
    repeat {
      type_to <-
        switch(as.character(geom_type),
          "MULTIPOLYGON" = "POLYGON",
          "POLYGON" = "MULTIPOINT",
          "MULTIPOINT" = "POINT"
        )

      x <- sf::st_cast(x, to = type_to, warn = FALSE, ...)
      geom_type <- is_geom_type(x, ext = FALSE)

      if (geom_type == to) break
    }
  } else if (!simplify) {
    repeat {
      type_to <-
        switch(as.character(geom_type),
          "MULTIPOINT" = "LINESTRING"
        )

      x <- sf::st_cast(x, to = type_to, warn = FALSE, ...)
      geom_type <- is_geom_type(x, ext = FALSE)

      if (geom_type == to) break
    }
  }

  x
}
