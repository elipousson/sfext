#' Check if x is an sf object
#'
#' If x is an `sf` object invisibly return TRUE. If not, return an error with [cli::cli_abort]
#'
#' @inheritParams is_sf
#' @param allow_list If `TRUE`, return `TRUE` if x is an sf list or, if ext is
#'   also `TRUE`, a list of sf, sfc, or bbox objects. Defaults to `FALSE`.
#' @param ... Additional parameters passed to [rlang::abort()].
#' @inheritParams rlang::args_error_context
#' @export
check_sf <- function(x,
                     ...,
                     ext = FALSE,
                     allow_list = FALSE,
                     allow_null = FALSE,
                     arg = caller_arg(x),
                     call = caller_env()) {
  if (!missing(x)) {
    is_sf_obj <- .check_is_sf_ext(
      x,
      ext = ext,
      allow_list = allow_list,
      allow_null = allow_null
    )
    if (is_sf_obj) {
      return(invisible(NULL))
    }
  }

  what <- .what_sf_ext(ext)

  # requires "standalone-types-check.R" from "r-lib/rlang"
  stop_input_type(
    x,
    what = glue("a {oxford_comma(what)} object"),
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @noRd
.what_sf_ext <- function(ext = FALSE) {
  what <- "sf"
  if (is_true(ext)) {
    c(what, "sfc", "bbox")
  } else if (is_character(ext)) {
    c(what, ext)
  } else {
    what
  }
}

#' @noRd
.check_is_sf_ext <- function(x,
                             ext,
                             allow_list,
                             allow_null) {
  if (is_sf(x, ext = ext)) {
    return(TRUE)
  }

  if (allow_list && is_sf_list(x, ext = ext)) {
    return(TRUE)
  }

  if (allow_null && is_null(x)) {
    return(TRUE)
  }

  FALSE
}
