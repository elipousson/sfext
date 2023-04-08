#' Check if x is NULL
#'
#' @noRd
check_null <- function(x = NULL,
                       arg = caller_arg(x),
                       allow_null = FALSE,
                       null.req = FALSE,
                       ...) {
  if (null.req) {
    allow_null <- null.req
  }

  if (is.null(x) && !allow_null) {
    cli_abort("{.arg {arg}} must not be NULL.", ...)
  }

  if (!is.null(x) && null.req) {
    cli_abort("{.arg {arg}} must be NULL.", ...)
  }

  return(invisible(TRUE))
}

#' Check if x is between a min and max length
#'
#' @noRd
check_len <- function(x = NULL,
                      len = 1,
                      arg = caller_arg(x),
                      allow_null = FALSE,
                      ...) {
  check_null(x, arg, allow_null)
  allow_null <- is.null(x) && allow_null

  if (((length(x) >= min(len)) && (length(x) <= max(len))) || allow_null) {
    return(invisible(TRUE))
  }

  if (length(len) > 1) {
    len <- glue("have a length between {min(len)} and {max(len)}")
  } else {
    len <- glue("be length {len}")
  }

  cli_abort(
    "{.arg {arg}} must {len}, not length {length(x)}.",
    ...
  )
}

#' Check if x matches pattern with grepl
#'
#' @noRd
check_grepl <- function(x = NULL,
                        pattern = NULL,
                        arg = caller_arg(x),
                        allow_null = FALSE,
                        ignore.case = FALSE,
                        perl = FALSE,
                        message = NULL,
                        ...) {
  check_null(x, arg, allow_null)
  allow_null <- is.null(x) && allow_null

  # FIXME: This will error if x is longer than 1
  if (grepl(pattern, x, ignore.case = ignore.case, perl = perl) || allow_null) {
    return(invisible(TRUE))
  }

  message <-
    message %||% "Can't detect pattern {.val {pattern}} in {.arg {arg}}."

  cli_abort(message = message)
}

#' Check if x starts with pattern
#'
#' @noRd
check_starts_with <- function(x = NULL,
                              string = NULL,
                              arg = caller_arg(x),
                              allow_null = FALSE,
                              ignore.case = FALSE,
                              perl = FALSE,
                              message = NULL,
                              ...) {
  check_character(x, allow_null = allow_null, arg = arg)
  allow_null <- is.null(x) && allow_null

  starts_with <-
    grepl(paste0("^", string), x,
      ignore.case = ignore.case, perl = perl
    )

  if (all(starts_with) || allow_null) {
    return(invisible(TRUE))
  }

  message <-
    message %||% c("{.arg {arg}} must start with {.val {string}}.",
      "i" = "The provided string is {.val {x}}."
    )

  cli_abort(message = message, ...)
}

#' Check if x is an sf object
#'
#' If x is an `sf` object invisibly return TRUE. If not, return an error with [cli::cli_abort]
#'
#' @inheritParams is_sf
#' @param allow_list If `TRUE`, return `TRUE` if x is an sf list or, if ext is also
#'   `TRUE`, a list of sf, sfc, or bbox objects. Defaults to `FALSE`.
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

  what <- .what_ext(ext)

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
.what_ext <- function(ext = FALSE) {
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

#' Check if the data.frame object has the required paper columns
#'
#' @noRd
check_df_paper <- function(x,
                           ext = FALSE,
                           arg = caller_arg(x),
                           call = caller_env()) {
  rlang::check_required(x, arg = arg, call = call)

  # FIXME: Add check to make sure input is a data frame
  paper_names <- c("width", "height", "orientation", "units")

  if (ext) {
    paper_names <- c(paper_names, "asp", "ncol", "nrow")
  }

  is_valid_paper <- has_name(x, paper_names)

  if (all(is_valid_paper)) {
    return(TRUE)
  }

  cli_abort(
    c("A {.arg paper} {.cls data.frame} must have columns named {.val {paper_names}}.",
      "i" = "The provided {.arg paper} is missing {.val {names[!is_valid_paper]}}."
    )
  )
}
