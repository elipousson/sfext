#' Convert object to coordinate reference system or check coordinate reference
#' system
#'
#' - [as_crs]: coerce x to a CRS object and (optionally) error if a NA value is returned.
#' - [is_same_crs]: do x and y have the same coordinate reference system?
#'
#' @param x For [as_crs()], object to convert to a coordinate reference system.
#'   For [is_same_crs()] and [is_wgs84()], object to check. For [as_wgs84()],
#'   object to convert to EPSG:4326.
#' @param allow_na For `as_crs()`, if `TRUE`, return `NA_crs_` if x can't be
#'   converted to a valid coordinate reference system. If `FALSE`, error instead
#'   of returning an invalid CRS.
#' @inheritParams rlang::args_error_context
#' @example examples/as_sf.R
#' @export
#' @importFrom rlang try_fetch
#' @importFrom sf st_crs
#' @importFrom cli cli_alert_warning cli_abort
as_crs <- function(x = NULL,
                   allow_na = TRUE,
                   arg = caller_arg(x),
                   call = caller_env()) {
  try_fetch(
    sf::st_crs(x),
    error = function(cnd) {
      msg <- "{.arg {arg}} {.val {crs}} can't be converted to a CRS."
      if (!allow_na) {
        cli_abort(msg, parent = cnd, call = call)
      } else {
        cli::cli_alert_warning(msg)
        NA_crs_
      }
    }
  )
}

#' @name is_same_crs
#' @rdname  as_crs
#' @param y For [is_same_crs()], object to compare to x.
#' @export
#' @importFrom sf st_crs
is_same_crs <- function(x, y) {
  crs_x <- as_crs(x, allow_na = FALSE)
  crs_y <- as_crs(y, allow_na = FALSE)

  any(
    c(
      crs_x == crs_y,
      !is.na(crs_x$input) &&
        !is.na(crs_y$input) &&
        (crs_x$input == crs_y$input)
    )
  )
}

#' @name is_wgs84
#' @rdname  as_crs
#' @export
is_wgs84 <- function(x) {
  is_same_crs(x, 4326)
}
