#' Union simple feature objects and combine name column values
#'
#' Wrapper for [sf::st_union()] supporting the additional feature of a combined
#' name column collapsed into a single vector with [cli::pluralize()].
#' [st_union_by()] wraps [dplyr::group_by()] and [dplyr::summarise()] to allow
#' quick unioning geometry by the passed parameters.
#'
#' @param x A `sf`, `sfc`, or `bbox` object or `sf` only for [st_union_by()]
#'   Required.
#' @param y A `sf` or `sfc` object, defaults to `NULL`.
#' @param name_col Column name to collapse into new name_col value, Default:
#'   'name'
#' @param .sf_col Geometry column name to use if x is an `sf` object, Default:
#'   `NULL`.
#' @param label Length 1 character vector used if name_col is `NULL`.
#' @param ext If `FALSE`, [st_union_ext()] functions the same as
#'   [sf::st_union()], Default: `TRUE`
#' @param ... Additional parameters passed to [sf::st_union()] or
#'   [dplyr::group_by()].
#' @return A `sfc` object if y is `NULL` and ext is `FALSE`. A tibble sf if x is
#'   a sf object and y is `NULL`. If y is provided, [st_union_ext()] is
#'   identical to [sf::st_union()]
#' @rdname st_union_ext
#' @example examples/st_union_ext.R
#' @export
#' @importFrom sf st_union st_geometry
#' @importFrom dplyr tibble
#' @importFrom cli pluralize
st_union_ext <- function(x,
                         y = NULL,
                         name_col = "name",
                         .sf_col = NULL,
                         label = NULL,
                         ext = TRUE,
                         ...) {
  check_sf(x, ext = TRUE)

  if (is_bbox(x)) {
    x <- sf_bbox_to_sfc(x)
  }

  if (!is_null(y)) {
    if (!is_same_crs(x, y)) {
      y <- st_transform_ext(y, crs = x)
    }

    return(suppressWarnings(sf::st_union(x, y, ...)))
  }

  sfc <- sf::st_union(x, ...)

  if (!ext || !is_sf(x)) {
    return(sfc)
  }

  if (nrow(x) == 1) {
    return(sf::st_set_geometry(x, sfc))
  }

  .sf_col <- .sf_col %||% get_sf_col(x)

  if (!is_null(name_col)) {
    if (!has_name(x, name_col)) {
      cli_warn(
        c("{.arg name_col} {.val {name_col}} can't be found in {.arg x}",
          "i" = "Setting {.arg name_col} to the first column of {.arg x}:
          {.val {names(x)[[1]]}}"
        )
      )

      name_col <- names(x)[[1]]
    }

    as_sf(
      dplyr::tibble(
        "{name_col}" := as.character(cli::pluralize("{x[[name_col]]}")),
        "{.sf_col}" := sfc
      )
    )
  } else if (!is_null(label)) {
    as_sf(
      dplyr::tibble(
        "label" = label,
        "{.sf_col}" := sfc
      )
    )
  } else {
    as_sf(
      sfc,
      sf_col = .sf_col %||% "geometry"
    )
  }
}

#' @name st_union_by
#' @rdname st_union_ext
#' @export
#' @importFrom dplyr summarise group_by
#' @importFrom sf st_make_valid
st_union_by <- function(x, ..., .sf_col = NULL) {
  check_sf(x)
  .sf_col <- .sf_col %||% get_sf_col(x)
  x <-
    dplyr::summarise(
      dplyr::group_by(
        sf::st_make_valid(x),
        ...
      ),
      "{.sf_col}" := {.sf_col}
    )

  sf::st_make_valid(x)
}
