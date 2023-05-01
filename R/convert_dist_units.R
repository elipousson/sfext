#' Convert distance (and area) values between different units
#'
#' @param dist Numeric or units object
#' @param from Existing unit for dist, Default: `NULL`. If dist is a units
#'   object, the numerator is used as "from"
#' @param to Unit to convert distance to, Default: 'meter'
#' @param drop If `TRUE`, return numeric. If `FALSE`, return class units object.
#' @param digits Number of digits to include in result; defaults to `NULL`.
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

  if (is_units(dist)) {
    dist_from <- get_dist_units(dist)

    if (!is_null(from) && !is_same_units(from, dist_from)) {
      cli::cli_warn(
        c("{.arg dist} is class {.cls units} and is using different units
            than {.arg from}.",
          "*" = "Replacing {.arg from} with {.val {dist_from}}."
        )
      )
    }

    from <- dist_from
    dist <- units::drop_units(dist)
  }

  if (!is_null(from)) {
    dist <- set_dist_units(dist, value = get_dist_units(from))
  }

  dist <- set_dist_units(dist, value = get_dist_units(to))

  if (!is_null(digits)) {
    dist <- round(dist, digits = digits)
  }

  if (!drop) {
    return(dist)
  }

  as.numeric(dist)
}


#' Set distance units
#'
#' @param allow_null If `FALSE`, error value; If `TRUE`, and value is `NULL`
#'   return x without setting units.
#' @noRd
#' @importFrom units set_units
set_dist_units <- function(x = NULL,
                           value = NULL,
                           mode = "standard",
                           allow_null = TRUE,
                           call = caller_env()) {
  if (allow_null && is_null(value)) {
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
