#' Spatial join with support for simple feature lists
#'
#' @name st_join_ext
#' @param boundary An sf object with a column named "name" or a list of sf
#'   objects where all items in the list have a "name" column.
#' @param join geometry predicate function; defaults to `NULL`, set to
#'   [sf::st_intersects] if key_list contains only POLYGON or MULTIPOLYGON
#'   objects or [sf::st_nearest_feature] if key_list contains other types.
#' @param col Column name used to convert y into a `sf` list if it is not
#'   already.
#' @param .id Column name with the values that should be added as a column to
#'   the input `sf` object.
#' @export
#' @importFrom rlang has_name
#' @importFrom dplyr rename select
#' @importFrom sf st_join
st_join_ext <- function(x,
                        y,
                        col = NULL,
                        .id = "name",
                        join = NULL,
                        ...) {
  check_sf(x)

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
      "If {.arg y} is a sf list, each entry must have a column named {.arg {.id}}.",
      condition = condition
    )

    y_list <- y

    for (nm in names(y_list)) {
      y <-
        dplyr::rename(
          dplyr::select(y_list[[nm]], .id),
          "{nm}" := .data[[.id]]
        )

      x <- has_same_name_col(x, col = nm, drop = FALSE)
      x <- suppressWarnings(sf::st_join(x, y, join = join))
    }

    if (length(y_list) == 1) {
      # FIXME: This isn't working
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
#'   [sf::st_intersects] if key_list contains only POLYGON or MULTIPOLYGON
#'   objects or [sf::st_nearest_feature] if key_list contains other types.
#' @importFrom sf st_intersects st_nearest_feature
#' @noRd
set_join_by_geom_type <- function(x, join = NULL) {
  if (!is.null(join)) {
    return(join)
  }

  if (all(sapply(x, is_polygon) | sapply(x, is_multipolygon))) {
    return(sf::st_intersects)
  }

  sf::st_nearest_feature
}
