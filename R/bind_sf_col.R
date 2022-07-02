#' Bind columns based on `sf` objects:
#'
#'  [bind_boundary_col] uses [sf::st_join] to assign simple feature data to an
#'  enclosing polygon. This function with other join functions to add columns
#'  based on other spatial relationships, e.g. bind a column for the nearest
#'  point feature.
#'
#' @name bind_sf_col
NULL

#' @name bind_boundary_col
#' @rdname bind_sf_col
#' @param boundary An sf object with a column named "name" or a list of sf
#'   objects where all items in the list have a "name" column.
#' @param join geometry predicate function; defaults to `NULL`, set to
#'   [sf::st_intersects] if key_list contains only POLYGON or MULTIPOLYGON objects
#'   or [sf::st_nearest_feature] if key_list contains other types.
#' @param col Column name used to convert boundary into a `sf` list and contains
#'   the values that should be added as a column to the input `sf` object.
#' @export
#' @importFrom rlang has_name
#' @importFrom dplyr rename select
#' @importFrom sf st_join
bind_boundary_col <- function(x, boundary = NULL, join = NULL, col = "name", ...) {
  if (!is_sf_list(boundary)) {
    boundary <- as_sf_list(boundary, crs = x, col = col)
  } else {
    boundary <- st_transform_ext(boundary, crs = x)
  }

  cli_abort_ifnot(
    "{.arg boundary} must be a list of `sf` objects where each object has a column named {.arg {col}}.",
    condition = all(sapply(boundary, has_name, col))
  )

  join <- set_join_by_geom_type(boundary, join = join)

  for (nm in names(boundary)) {
    y <-
      dplyr::rename(
        dplyr::select(boundary[[nm]], name),
        "{nm}" := "name"
      )

    x <- has_same_name_col(x, col = nm, drop = FALSE)
    x <- sf::st_join(x, y, join = join, ...)
  }

  relocate_sf_col(x)
}

#' @name bind_units_col
#' @rdname bind_sf_col
#' @param y Vector of numeric or units values to bind to x.
#' @param units Units to use for y (if numeric) or convert to (if y is units
#'   class); defaults to `NULL`.
#' @param drop If `TRUE`, apply the [units::drop_units] function to the column
#'   with units class values and return numeric values instead; defaults to
#'   `FALSE`.
#' @param keep_all If `FALSE`, keep all columns. If `FALSE`, return only the
#'   named .id column.
#' @param .id Name to use for vector of units provided to "y" parameter, when
#'   "y" is bound to the "x" data frame or tibble as a new column.
#' @export
#' @importFrom dplyr bind_cols
bind_units_col <- function(x, y, units = NULL, drop = FALSE, keep_all = TRUE, .id = NULL, call = caller_env()) {
  is_pkg_installed("units")

  if (!is.null(units)) {
    y <-
      convert_dist_units(
        dist = y,
        from = get_dist_units(y),
        to = units
      )
  } else {
    units <-
      try_fetch(
        get_dist_units(y),
        warning = function(cnd) NULL
      )
  }

  if (drop) {
    y <- units::drop_units(y)
  }

  if (!keep_all) {
    return(y)
  }

  cli_abort_ifnot(
    "A {.arg .id} or {.arg units} value must be provided.",
    condition = !is.null(units) | !is.null(.id),
    call = call
  )

  .id <- .id %||% janitor::make_clean_names(units)

  x <-
    has_same_name_col(x, col = .id)

  x <-
    dplyr::bind_cols(
      x,
      "{.id}" := y
    )

  if (!is_sf(x)) {
    return(x)
  }

  relocate_sf_col(x)
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
