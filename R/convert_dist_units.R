#' Convert distance (and area) values between different units
#'
#' @param dist Numeric or units object
#' @param from Existing unit for dist, Default: `NULL`. If dist is a units object,
#'   the numerator is used as "from"
#' @param to Unit to convert distance to, Default: 'meter'
#' @return Object created by [units::set_units()]
#' @rdname convert_dist_units
#' @family dist
#' @export
#' @importFrom units set_units
convert_dist_units <- function(dist,
                               from = NULL,
                               to = "meter") {
  stopifnot(
    is.numeric(dist) || is_units(dist)
  )

  if (is_units(dist)) {
    from <- get_dist_units(dist)
  }

  if (!is.null(from)) {
    from <- match.arg(from, c(dist_unit_options, area_unit_options))

    if (!is_units(dist)) {
      from <- gsub(" ", "_", from)
      dist <-
        units::set_units(
          x = dist,
          value = from,
          mode = "standard"
        )
    }
  }

  if (!is.null(to)) {
    to <- gsub(" ", "_", to)
    to <- match.arg(to, c(dist_unit_options, area_unit_options))

    dist <-
      units::set_units(
        x = dist,
        value = to,
        mode = "standard"
      )
  }

  return(dist)
}
