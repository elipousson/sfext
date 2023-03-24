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
#' @param list.ok If `TRUE`, return `TRUE` if x is an sf list or, if ext is also
#'   `TRUE`, a list of sf, sfc, or bbox objects. Defaults to `FALSE`.
#' @param arg Used internally to create better error messages; defaults to
#'   [rlang::caller_arg].
#' @inheritParams cli::cli_abort
#' @inheritDotParams cli::cli_abort
#' @export
#' @importFrom rlang check_required
#' @importFrom cliExtras cls_vec
check_sf <- function(x,
                     arg = caller_arg(x),
                     allow_null = FALSE,
                     list.ok = FALSE,
                     ext = FALSE,
                     call = caller_env(),
                     ...) {
  rlang::check_required(x, arg = arg, call = call)
  check_null(x, arg, allow_null)

  list.ok <- list.ok && is_sf_list(x, named = FALSE, ext, allow_null)

  if (is_sf(x, ext, allow_null) || list.ok) {
    return(invisible(TRUE))
  }

  sf <- "sf"

  if (isTRUE(ext)) {
    sf <- c(sf, "sfc", "bbox")
  } else if (is.character(ext)) {
    sf <- c(sf, ext)
  }

  cli_abort(
    c("{.arg {arg}} must be {cliExtras::cls_vec(sf)} class.",
      "i" = "{.arg {arg}} is {.cls {class(x)}}."
    ),
    call = call,
    ...
  )
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
