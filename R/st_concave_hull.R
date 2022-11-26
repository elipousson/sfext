#' Make a concave hull around simple feature object by attribute
#'
#' @param x A sf object.
#' @param by Column name to use for grouping and combining geometry, Default:
#'   `NULL`
#' @param centroid If `TRUE`, use centroids for geometry of x, Default: `FALSE`
#' @inheritParams concaveman::concaveman
#' @seealso
#'  \code{\link[concaveman]{concaveman}}
#' @rdname st_concave_hull
#' @export
#' @importFrom sf st_centroid st_combine st_cast st_make_valid st_set_geometry
#' @importFrom dplyr summarise
#' @importFrom purrr map_dfr
st_concave_hull <- function(x,
                            by = NULL,
                            centroid = FALSE,
                            concavity = 2,
                            length_threshold = 0) {
  is_pkg_installed("concaveman")

  check_sf(x)

  if (!is_sf(x)) {
    x <- as_sf(x)
  }

  if (centroid) {
    x <- suppressWarnings(sf::st_centroid(x))
  }

  if (!is.null(by)) {
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
        concaveman::concaveman(
          points = as_sf(.x),
          concavity = concavity,
          length_threshold = length_threshold
        )
      )
    )


  sf::st_set_geometry(x, as_sfc(geometry, crs = x))
}
