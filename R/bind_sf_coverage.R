#' Row bind coverage for a feature based on a coverage feature
#'
#' `r lifecycle::badge("experimental")`
#'
#' [bind_sf_coverage()] and the related helper functions
#' [st_make_valid_coverage()] and [st_make_valid_union()] take a sf object and
#' create a non-intersecting coverage area of a surrounding geography defined by
#' a second sf, sfc, or bbox object. This was originally developed for the
#' [getACS::make_area_xwalk()] function.
#'
#' @param x A input sf object
#' @param coverage A sf, sfc, or bbox object that covers x.
#' @param coverage_nm Name to use for coverage feature assigned as value for .id
#'   column, Default: `NULL`
#' @param .id .id column name, must be present in x Default: 'name'
#' @param x_arg,coverage_arg,id_arg Argument names for error messages if
#'   [bind_sf_coverage()] is used as a wrapper function.
#' @inheritParams rlang::args_error_context
#' @returns A sf object or a sfc object
#' @keywords internal
#' @seealso
#'  [vctrs::vec_bind()]
#'  [sf::valid()], [sf::st_as_sf()]
#' @rdname bind_sf_coverage
#' @export
#' @importFrom vctrs vec_rbind
#' @importFrom sf st_make_valid st_as_sf
bind_sf_coverage <- function(x,
                          coverage,
                          coverage_nm = NULL,
                          .id = "name",
                          x_arg = caller_arg(x),
                          coverage_arg = caller_arg(coverage),
                          id_arg = caller_arg(id),
                          combine = FALSE,
                          error_call = caller_env()) {
  check_sf(x, call = error_call)
  check_sf(coverage, ext = TRUE, call = error_call)

  if (is_bbox(coverage)) {
    coverage <- as_sfc(coverage)
  }

  coverage <- transform_sf(coverage, crs = x)

  coverage_geom_in <- sf::st_geometry(coverage)

  coverage <- try_fetch(
    st_make_valid_coverage(coverage, x, combine = combine),
    error = function(cnd) {
      cli_abort(
        "Valid spatial coverage for the area of {.arg {coverage_arg}} outside
        the {.arg {x_arg}} can't be created.",
        parent = cnd,
        call = error_call
      )
    }
  )

  if (coverage_geom_in == coverage) {
    cli_abort(
      "{.arg {x_arg}} is not covered by {.arg {coverage_arg}}",
      call = error_call
    )
  }

  if (is_empty(coverage)) {
    # FIXME: Add checks in case all features or no features intersect
    cli_abort(
      c("{.arg {x_arg}} is not covered by {.arg {coverage_arg}}",
        "i" = "Supply a {.arg {coverage_arg}} covering the full geometry of {.arg {x_arg}}."
      ),
      call = error_call
    )
  }

  check_has_name(x, .id, call = error_call)
  check_string(.id, call = error_call)

  check_string(
    coverage_nm,
    allow_null = TRUE,
    allow_empty = FALSE,
    call = error_call
    )

  if (is.null(coverage_nm)) {
    coverage_nm <- paste0(
      sample(letters, size = 12, replace = TRUE),
      collapse = ""
      )

    cli::cli_alert_info(
      "Using placeholder name {.val {coverage_nm}} for coverage"
    )
  }

  coverage <- set_names(
    data.frame(
      coverage_nm,
      coverage
    ),
    c(.id, attr(x, "sf_column"))
  )

  x <- vctrs::vec_rbind(x, coverage, .error_call = error_call)

  # FIXME: is this last st_make_valid call actually required?
  sf::st_make_valid(sf::st_as_sf(x))
}

#' @param y Second parameter for [st_make_valid_coverage()]
#' @param .fn Defaults to sf::st_difference
#' @rdname bind_sf_coverage
#' @export
#' @importFrom sf st_make_valid st_difference
st_make_valid_coverage <- function(x,
                                   y,
                                   combine = FALSE,
                                   is_coverage = TRUE,
                                   .fn = sf::st_difference) {
  sf::st_make_valid(
    .fn(
      st_make_valid_union(x, combine, is_coverage),
      st_make_valid_union(y, combine, is_coverage)
    )
  )
}

#' @rdname bind_sf_coverage
#' @inheritParams sf::st_union
#' @param combine If `TRUE`, use [sf::st_combine()] before passing x to
#'   [sf::st_union()]
#' @export
#' @importFrom sf st_make_valid st_union
st_make_valid_union <- function(x, combine = FALSE, is_coverage = TRUE) {
  if (combine) {
    x <- sf::st_combine(x)
  }

  sf::st_make_valid(sf::st_union(x, is_coverage = is_coverage))
}
