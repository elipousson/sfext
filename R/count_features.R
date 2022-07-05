
#' Count simple feature objects
#'
#' Count the number of simple feature objects within a geometry or by some
#' attribute contained in both the data and the corresponding boundary geometry.
#'
#' @param data Data frame or `sf` object, Default: `NULL`
#' @param boundaries Boundary data as an `sf` object, Default: `NULL`
#' @param by Character string to join data by if data is not an `sf` object,
#'   Default: `NULL`
#' @param join The join function used by [sf::st_join()] if data is an `sf`
#'   object, Default: [sf::st_intersects]
#' @param .id Count column, Default: "count"
#' @param ... Additional parameters (not in use)
#' @rdname count_features
#' @export
#' @importFrom sf st_intersects st_nearest_feature st_drop_geometry st_join st_as_sf
#' @importFrom purrr map_dfr
#' @importFrom dplyr pull left_join
count_features <- function(x = NULL,
                           y = NULL,
                           join = NULL,
                           by = NULL,
                           .id = "count",
                           col = NULL,
                           ...) {
  if (is_sf(x)) {
    check_sf(x)

    if (is_sf(y)) {
      y <- as_sf_list(y, col = col)
    }

    join <- set_join_by_geom_type(x, join = join)

    x <-
      purrr::map_dfr(
        # Taken from write_exif_keywords
        y,
        ~ sf::st_drop_geometry(
          sf::st_join(
            x,
            dplyr::select(.x, geometry, id),
            join = join
          )
        ),
        .id = .id
      )

    return(count(x, .id))
  } else if (is.data.frame(x) && is_sf(y)) {
    if (class(dplyr::pull(x, by)) != class(dplyr::pull(y, by))) {

    }

    # If features is count data
    # If boundary data is provided
    data <-
      dplyr::left_join(
        data,
        boundaries,
        by = by
      )

    data <- sf::st_as_sf(data)
    data <- rename_sf_col(data)

    # Return boundary data geometry
  }

  data
}
