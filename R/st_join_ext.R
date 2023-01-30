#' Complete a spatial join using a simple feature objects or an object and list
#'
#' Wrapper for [sf::st_join()] that works with sf lists.
#'
#' @param x A `sf`, `sfc`, or `bbox` object.
#' @param y An `sf` object with a column named "name" or a list of sf
#'   objects where all items in the list have a "name" column.
#' @param join geometry predicate function; defaults to `NULL`, set to
#'   [sf::st_intersects()] if y contains only POLYGON or MULTIPOLYGON
#'   objects or [sf::st_nearest_feature()] if y contains other types.
#' @param col Column name used to convert y into a `sf` list if it is not
#'   already.
#' @param .id Column name with the values that should be added as a column to
#'   the input `sf` object.
#' @param ... Additional parameters passed to [sf::st_join()]
#' @export
#' @importFrom dplyr rename select
#' @importFrom sf st_join
st_join_ext <- function(x,
                        y,
                        col = NULL,
                        .id = "name",
                        join = NULL,
                        ...) {
  check_sf(x, ext = TRUE)

  if (is_bbox(x)) {
    x <- sf_bbox_to_sfc(x)
  }

  join <- set_join_by_geom_type(y, join = join)

  if (!is_sf_list(y) && !is.null(col)) {
    y <- as_sf_list(y, crs = x, col = col)
  }

  if (is_sf(y)) {
    y <- st_transform_ext(y, crs = x)

    return(relocate_sf_col(sf::st_join(x, y, join = join, ...)))
  }

  if (is_sf_list(y) && !is.null(.id)) {
    y <- st_transform_ext(y, crs = x)

    condition <- all(sapply(y, has_name, .id))
    cli_abort_ifnot(
      "If {.arg y} is a sf list, each entry must have a column named
      {.val {(.id)}} to match {.arg .id} argument.",
      condition = condition
    )

    y_list <- y

    for (nm in names(y_list)) {
      y <-
        dplyr::rename(
          dplyr::select(y_list[[nm]], dplyr::all_of(.id)),
          "{nm}" := .data[[.id]]
        )

      x <- has_same_name_col(x, col = nm, drop = FALSE)
      x <- suppressWarnings(sf::st_join(x, y, join = join))
    }

    return(x)

    # FIXME: Everything below here isn't working
    if (length(y_list) == 1) {
      col <- names(y_list)

      x <-
        dplyr::group_by(
          x,
          .data[[col]]
        )

      x <-
        dplyr::summarize(
          x,
          "{col}" := list(unique(.data[[col]]))
        )
    }

    return(relocate_sf_col(x))
  }
}

#' Set join function based on geometry type
#'
#' @name set_join_by_geom_type
#' @inheritParams is_geom_type
#' @param join geometry predicate function; defaults to `NULL`, set to
#'   [sf::st_intersects()] if x contains only POLYGON or MULTIPOLYGON
#'   objects or [sf::st_nearest_feature()] if x contains other types.
#' @importFrom sf st_intersects st_nearest_feature
#' @noRd
set_join_by_geom_type <- function(x, join = NULL) {
  if (!is.null(join)) {
    return(join)
  }

  if (is_sf(x)) {
    x_is_poly <-
      all(
        is_multipolygon(x, by_geometry = TRUE) |
          is_polygon(x, by_geometry = TRUE)
      )
  }

  if (is_sf_list(x)) {
    # FIXME: Replace these sapply calls w/ vapply
    x_is_poly <-
      all(
        sapply(x, is_polygon, by_geometry = FALSE) |
          sapply(x, is_multipolygon, by_geometry = FALSE)
      )
  }

  if (x_is_poly) {
    return(sf::st_intersects)
  }

  sf::st_nearest_feature
}
