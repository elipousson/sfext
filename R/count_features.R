#' Count simple features based on relationship with a second simple feature object
#'
#' Use [st_join_ext()] and [dplyr::count()] to count features in x based on
#' their spatial relationship with y. Very similar to [count_sf_ext()] so they may
#' be merged in the future.
#'
#' @param x Data frame or `sf` object, Default: `NULL`
#' @param y Length 1 named `sf` list (name of y is used as count if count is
#'   `NULL`) or a `sf` object if nm is not `NULL`. y must include a column name
#'   matching the value of .id. If y is `NULL`, count is required. Default: `NULL`
#' @param nm Vector of names to use with [as_sf_list()] to convert y to an sf list
#'   (if y is not already an sf list). Defaults to "data".
#' @inheritParams st_join_ext
#' @inheritParams df_to_sf
#' @param count Name of column to count. If `NULL`, count is set to `names(y)`
#'   (assuming y is a length 1 named `sf` list).
#' @inheritParams dplyr::count
#' @param geometry If "y", replace x geometry with y geometry joining based on
#'   by. If by is `NULL`, by is set to the same value as count.
#' @param ... Additional parameters passed to [st_join_ext()]
#' @export
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr count rename
count_features <- function(x = NULL,
                           y = NULL,
                           nm = "data",
                           join = NULL,
                           .id = "name",
                           by = NULL,
                           count = NULL,
                           sort = FALSE,
                           name = NULL,
                           geometry = "y",
                           ...) {
  if (!is_sf_list(y, null.ok = TRUE) && !is.null(nm)) {
    y <- as_sf_list(y, nm = nm, crs = x)
  }

  cli_abort_ifnot(
    "{.arg y} must be an sf object (if {.arg nm} is provided), an sf list, or NULL (if {.arg count} is provided).",
    condition = is_sf_list(y) | !is.null(count)
  )

  if (is_sf(x) && is_sf_list(y)) {
    cli_abort_ifnot(
      "{.arg y} must be length 1 if it is an sf list.",
      condition = (length(y) == 1)
    )

    x <- st_join_ext(x, y, join = join, .id = .id, ...)

    count <- count %||% names(y)

    # Convert y back into a sf object
    y <- as_sf(y)
  }

  cli_abort_ifnot(
    "{.arg count} must be a length 1 character vector.",
    condition = is.character(count) && (length(count) == 1)
  )

  x <-
    dplyr::count(
      x,
      .data[[count]],
      wt = NULL,
      sort = sort,
      name = name
    )

  geometry <- arg_match(geometry, c("y", "x", "drop"))

  if (!is_sf(y) || (geometry == "x")) {
    return(x)
  }

  if (is_sf(x)) {
    x <- sf::st_drop_geometry(x)
  }

  if (geometry == "drop") {
    return(x)
  }

  if (is.null(by)) {
    y <- dplyr::rename(y, "{count}" := .id)
    by <- count
  }

  join_sf_to_df(x = x, y = y, by = by)
}
