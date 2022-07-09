#' Union simple feature objects and combine name column values
#'
#' Wrapper for [sf::st_union] supporting the additional feature of a combined
#' name column collapsed into a single vector with [cli::pluralize].
#'
#' @param x A sf, sfc, or bbox object, Default: `NULL`
#' @param y A sf or sfc object, Default: `NULL`
#' @param name_col Column name to collapse into new name_col value, Default:
#'   'name'
#' @param sf_col Geometry column name to use if x is an sf object, Default:
#'   'geometry'
#' @param ext If `FALSE`, st_union_ext functions the same as [sf::st_union],
#'   Default: `TRUE`
#' @param ... Additional parameters passed to [sf::st_union]
#' @return A sfc object if y is `NULL` and ext is `FALSE`. A tibble sf if x is a
#'   sf object and y is `NULL`. If y is provided, [st_union_ext] is identical to
#'   [sf::st_union]
#' @rdname st_union_ext
#' @example examples/st_union_ext.R
#' @export
#' @importFrom sf st_union st_geometry
#' @importFrom cli pluralize
#' @importFrom dplyr tibble mutate all_of
st_union_ext <- function(x = NULL, y = NULL, name_col = "name", sf_col = "geometry", ext = TRUE, ...) {
  check_sf(x, ext = TRUE)

  if (is_bbox(x)) {
    x <- as_sfc(x)
  }

  if (!is.null(y)) {
    return(sf::st_union(x, y, ...))
  }

  sfc <- sf::st_union(x, ...)

  if (!ext | !is_sf(x)) {
    return(sfc)
  }

  if (nrow(x) == 1) {
    sf::st_geometry(x) <- sfc
    return(x)
  }

  if (!has_name(x, name_col)) {
    cli_warn(
      c("{.arg name_col} {.val {name_col}} can't be found in {.arg x}",
        "i" = "Setting {.arg name_col} to the first column of {.arg x}: {.val {names(x)[[1]]}}"
      )
    )

    name_col <- names(x)[[1]]
  }

  as_sf(
    dplyr::tibble(
      "{name_col}" := as.character(cli::pluralize("{x[[name_col]]}")),
      "{sf_col}" := sfc
    )
  )
}
