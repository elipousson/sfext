#' Get bounding box corner points from a bbox, sfc, or sf object
#'
#' If x is an sf object, the results include a column with a name matching .id
#' containing the values "SW", "SE", "NE", and "NW" corresponding to the
#' cardinal position of each corner.
#'
#' @param x A `bbox`, length 1 `sfc` object, or a `sf` object with a single
#'   feature
#' @param .id A name to use for added column with cardinal position of points,
#'   Default: 'corner'
#' @param class Class of object to return. Must be "sf" (default) or "sfc".
#' @return A `sf` with four features (duplicating input feature with new
#'   geometry derived from bounding box) or a length 4 `sfc` object with POINT
#'   geometry.
#' @export
#' @importFrom sf st_cast st_drop_geometry
#' @importFrom dplyr bind_cols
sf_bbox_corners <- function(x, .id = "corner", class = "sf") {
  check_sf(x, ext = TRUE)
  class <- arg_match(class, c("sf", "sfc"))

  bbox <- x
  sf_in <- FALSE

  if (!is_bbox(x)) {
    if (is_sf(x)) {
      sf_in <- TRUE
    }

    cli_abort_ifnot(
      (is_sf(x) && (nrow(x) == 1)) || (is_sfc(x) && length(x) == 1),
      message = "{.arg x} must be a length 1 {.cls sfc} object or a {.cls sf} object with 1 feature.",
    )

    bbox <- as_bbox(x)
  }

  geometry <- sf::st_cast(as_sfc(bbox), to = "POINT")[1:4]

  if (class != "sf") {
    return(geometry)
  }

  if (sf_in) {
    x <- has_same_name_col(x, .id)

    return(
      as_sf(
        dplyr::bind_cols(
          sf::st_drop_geometry(x),
          "{.id}" := c("SW", "SE", "NE", "NW"),
          "geometry" = geometry
        )
      )
    )
  }

  as_sf(
    dplyr::bind_cols(
      "{.id}" := c("SW", "SE", "NE", "NW"),
      "geometry" = geometry
    )
  )
}
