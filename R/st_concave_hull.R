#' Make a concave hull around simple feature object by attribute
#'
#' @param x A sf object.
#' @param by Column name to use for grouping and combining geometry, Default:
#'   `NULL`
#' @param centroid If `TRUE`, use centroids for geometry of x, Default: `FALSE`
#' @inheritParams sf::st_concave_hull
#' @rdname st_concave_hull_ext
#' @export
#' @importFrom sf st_centroid st_combine st_cast st_make_valid st_concave_hull
#'   st_set_geometry
#' @importFrom dplyr summarise
st_concave_hull_ext <- function(x,
                                by = NULL,
                                centroid = FALSE,
                                ratio = 0.5,
                                allow_holes = FALSE) {
  check_sf(x, ext = "sfc")

  return_sfc <- FALSE

  if (!is_sf(x)) {
    return_sfc <- TRUE
    x <- as_sf(x)
  }

  if (centroid) {
    x <- suppressWarnings(sf::st_centroid(x))
  }

  if (!is_null(by)) {
    x <- st_union_by(x, .data[[by]])
  }

  if (is_point(x)) {
    cli_warn(
      "Features with POINT geometry should be combined using the {.arg by}
      to create convex hulls."
    )
  }

  geometry <-
    map(
      as_sfc(x),
      ~ sf::st_make_valid(
        sf::st_concave_hull(
          x = as_sf(.x),
          ratio = ratio,
          allow_holes = allow_holes
        )
      )
    )

  goemetry <- sf::st_as_sf(list_rbind(geometry))

  geometry <- as_sfc(goemetry, crs = x)

  if (return_sfc) {
    return(geometry)
  }

  sf::st_set_geometry(x, geometry)
}
