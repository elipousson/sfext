#' Bind units column to data frame
#'
#' Utility function supporting [get_area], [get_dist], and [get_bearing].
#'
#' @name bind_units_col
#' @param x Data frame or sf object.
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
#' @inheritParams rlang::args_error_context
#' @export
#' @importFrom dplyr bind_cols
bind_units_col <- function(x,
                           y,
                           units = NULL,
                           drop = FALSE,
                           keep_all = TRUE,
                           .id = NULL,
                           call = caller_env()) {
  if (!is_null(units)) {
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
    condition = !is_null(units) | !is_null(.id),
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
