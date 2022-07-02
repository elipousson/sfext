#' Convert distance (and area) values between different units
#'
#' @param dist Numeric or units object
#' @param from Existing unit for dist, Default: `NULL`. If dist is a units
#'   object, the numerator is used as "from"
#' @param to Unit to convert distance to, Default: 'meter'
#' @param drop If `TRUE`, return numeric. If `FALSE`, return class object.
#' @param digits Number of digits to include in result; defaults to NULL.
#' @return Object created by [units::set_units()]
#' @rdname convert_dist_units
#' @family dist
#' @export
#' @importFrom units set_units
convert_dist_units <- function(dist,
                               from = NULL,
                               to = "meter",
                               drop = FALSE,
                               digits = NULL) {
  cli_abort_ifnot(
    "{.arg dist} must be a numeric or units class object.",
    condition = (is.numeric(dist) || is_units(dist))
  )

  if (is_units(dist) && !is.null(from)) {
    # cli_warn(
    #  "Replacing {.arg from} of {.val {from}} with the units of {.arg dist} ({.val {get_dist_units(dist)}})."
    # )

    from <- get_dist_units(dist)
    dist <- units::drop_units(dist)
  }

  if (!is_units(dist) && !is.null(from)) {
    dist <- set_dist_units(dist, value = from)
  }

  dist <- set_dist_units(dist, value = to)

  if (!is.null(digits)) {
    dist <- round(dist, digits = digits)
  }

  if (!drop) {
    return(dist)
  }

  as.numeric(dist)
}


#' Set distance units
#'
#' @noRd
#' @param null.ok If FALSE, error value; If TRUE, and value is NULL return x without setting units.
#' @importFrom units set_units
set_dist_units <- function(x = NULL, value = NULL, mode = "standard", null.ok = TRUE, call = caller_env()) {
  if (is.null(value) && null.ok) {
    return(x)
  }

  check_null(value)

  value <- underscore(value)

  value <-
    arg_match(
      value,
      c(dist_unit_options, area_unit_options),
      error_call = call
    )

  units::set_units(
    x = x,
    value = value,
    mode = mode
  )
}
