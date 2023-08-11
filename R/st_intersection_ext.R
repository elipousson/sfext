#' A wrapper for `sf::st_intersection()`
#'
#' Includes checks for empty input for x and y. Also allows the specification of
#' an output coordinate reference system.
#'
#' @inheritParams sf::st_intersection
#' @inheritDotParams sf::st_intersection
#' @return A sf or sfc object.
#' @seealso
#'  [sf::geos_binary_ops()]
#' @rdname st_intersection_ext
#' @export
#' @importFrom sf st_intersection
st_intersection_ext <- function(x, y = NULL, crs = NULL, ...) {
  if (is_empty(x)) {
    cli_abort(
      "{.arg x} can't be empty."
    )
  }

  if (!is.null(y) && is_empty(y)) {
    cli_abort(
      "{.arg y} can't be empty."
    )
  }

  x <- st_transform_ext(x, crs)
  y <- st_transform_ext(y, crs = x)

  sf::st_intersection(x, y, ...)
}
