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
#' @return A `sf` with four features or a length 4 `sfc` object with POINT
#'   geometry.
#' @export
#' @importFrom sf st_cast st_drop_geometry
#' @importFrom dplyr bind_cols
sf_bbox_corners <- function(x, .id = "corner") {
  check_sf(x, ext = TRUE)

  bbox <- x

  if (!is_bbox(x)) {
    bbox <- as_bbox(x)
  }

  cli_abort_ifnot(
    "{.arg x} must be a length 1 {.cls sfc} object or a {.cls sf} object with 1 feature.",
    condition = (is_sf(x) && (nrow(x) == 1)) | (is_sfc(x) && length(x) == 1)
  )

  geometry <- sf::st_cast(as_sfc(bbox), to = "POINT")[1:4, ]

  if (!is_sf(x)) {
    return(geometry)
  }

  x <- has_same_name_col(x, .id)

  as_sf(
    dplyr::bind_cols(
      sf::st_drop_geometry(x),
      "{.id}" := c("SW", "SE", "NE", "NW"),
      geometry
    )
  )
}
