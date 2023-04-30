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
#' @importFrom purrr map_dfr
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
    sf_col <- get_sf_col(x)
    x <-
      dplyr::summarise(
        group_by(x, .data[[by]]),
        "{sf_col}" := sf::st_combine(.data[[sf_col]])
      )
  }

  if (is_point(x)) {
    cli_warn(
      "Features with POINT geometry should be combined using the {.arg by}
      to create convex hulls."
    )
  }

  if (!is_multipoint(x)) {
    x <- sf::st_cast(x, to = "MULTIPOINT", warn = FALSE, do_split = FALSE)
  }

  geometry <-
    purrr::map_dfr(
      as_sfc(x),
      ~ sf::st_make_valid(
        sf::st_concave_hull(
          x = as_sf(.x),
          ratio = ratio,
          allow_holes = allow_holes
        )
      )
    )

  geometry <- as_sfc(geometry, crs = x)

  if (return_sfc) {
    return(geometry)
  }

  sf::st_set_geometry(x, geometry)
}
