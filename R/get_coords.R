#' Get coordinates for a simple feature or bounding box object
#'
#' An extended version of [sf::st_coordinates] that supports binding coordinates
#' to the object, optionally dropping the geometry, and returning wkt or a point
#' on surface (geometry = "surface point") instead of the centroid.
#'
#' [get_minmax] get a bounding box for each feature (or group of features)
#' appends the xmin, ymin, xmax, and ymax values for each feature to the simple
#' feature object.
#'
#' @param x `sf`, `bbox`, or `sfc` object
#' @param coords Column names to use for coordinates in results, Default: `NULL`;
#'   which is set to c("lon", "lat") by [check_coords]
#' @param geometry geometry to use for coordinates "centroid", "surface point",
#'   or alternatively "wkt"; defaults to `NULL` ("centroid")
#' @param keep_all If `TRUE`, bind the coordinates columns to the provided object x,
#'   Default: `TRUE`.
#' @param crs Coordinate reference system to use for coordinates; defaults to `NULL`.
#' @param drop If `TRUE` and x is an sf object, drop the geometry Default: `TRUE`.
#' @rdname get_coords
#' @aliases st_coords
#' @export
#' @importFrom sf st_as_text st_point_on_surface st_coordinates
#'   st_drop_geometry st_zm
#' @importFrom dplyr select bind_cols
get_coords <- function(x, coords = NULL, geometry = NULL, crs = NULL, keep_all = TRUE, drop = TRUE) {
  geometry <- match.arg(geometry, c("centroid", "surface point", "wkt"))

  stopifnot(
    is_sf(x, ext = TRUE)
  )

  x_coords <- st_transform_ext(x = x, crs = crs)

  if (geometry == "wkt") {
    x <- has_same_name_col(x, "wkt", quiet = TRUE)
    # Convert geometry to wkt
    x_coords <- data.frame("wkt" = sf::st_as_text(as_sfc(x)))
  } else {
    # Convert to coordinates at centroid or as a point on surface
    # FIXME: This approach may be an issue if a sf object has mixed geometry

    if (!is_point(x)) {
      if (geometry == "centroid") {
        # FIXME: Double check that this doesn't cause issues for sfc objects
        x_coords <- st_center(x_coords, ext = FALSE)
      } else if (geometry == "surface point") {
        x_coords <- suppressMessages(sf::st_point_on_surface(sf::st_zm(x_coords)))
      }
    }

    x_coords <- as.data.frame(sf::st_coordinates(x_coords))

    coords <- check_coords(coords = coords)
    x <- has_same_name_col(x, coords[1], quiet = TRUE)
    x <- has_same_name_col(x, coords[2], quiet = TRUE)

    x_coords <-
      dplyr::select(
        x_coords,
        "{coords[1]}" := .data$X,
        "{coords[2]}" := .data$Y
      )
  }

  # If x is an sfc or keep_all = FALSE return coordinates
  if (!keep_all) {
    return(x_coords)
  }

  x <- dplyr::bind_cols(x, x_coords)

  if (drop) {
    x <- sf::st_drop_geometry(x)
  }

  relocate_sf_col(x)
}

#' @name get_minmax
#' @aliases st_coords_minmax
#' @rdname get_coords
#' @importFrom dplyr mutate row_number select bind_cols
#' @importFrom purrr map
#' @importFrom tibble enframe
#' @importFrom tidyr unnest_wider
#' @importFrom sf st_drop_geometry
get_minmax <- function(x, crs = NULL, keep_all = TRUE, drop = TRUE) {
  stopifnot(
    is_sf(x, ext = TRUE)
  )

  x <- dplyr::mutate(
    x,
    minmax_row_num = as.character(dplyr::row_number())
  )

  col <- "minmax_row_num"

  # Get bbox for each feature (col must be unique)
  x_bbox_list <-
    st_bbox_ext(as_sf_list(x, col = col), crs = crs, class = "list")
  # Drop bbox class

  x_bbox_list <-
    purrr::map(
      x_bbox_list,
      ~ as.numeric(.x)
    )

  x <- dplyr::select(x, -col)

  minmax_opts <- c("xmin", "ymin", "xmax", "ymax")

  minmax_df <-
    tidyr::unnest_wider(
      tibble::enframe(x_bbox_list, value = "bbox"),
      "bbox",
      names_repair = ~ c(col, minmax_opts),
      names_sep = ""
    )

  minmax_df <-
    dplyr::select(minmax_df, -col)


  if (!keep_all) {
    return(minmax_df)
  }

  if (drop) {
    x <- sf::st_drop_geometry(x)
  }

  x <-
    dplyr::bind_cols(
      x,
      minmax_df
    )

  relocate_sf_col(x)
}
