check_null <- function(x = NULL, arg = caller_arg(x), null.ok = FALSE, null.req = FALSE, ...) {
  if (null.req) {
    null.ok <- null.req
  }

  if (is.null(x) && !null.ok) {
    cli_abort("{.arg {arg}} must not be NULL.", ...)
  }

  if (!is.null(x) && null.req) {
    cli_abort("{.arg {arg}} must be NULL.", ...)
  }

  invisible(return(TRUE))
}

check_character <- function(x = NULL, arg = caller_arg(x), null.ok = FALSE, ...) {
  check_null(x, arg, null.ok)

  if (is.character(x)) {
    invisible(return(TRUE))
  }

  cli_abort(
    c("{.arg {arg}} must be a character vector.",
      "i" = "You've supplied a {class(x)} object."
    ),
    ...
  )
}

check_len <- function(x = NULL, len = 1, arg = caller_arg(x), null.ok = FALSE, ...) {
  check_null(x, arg, null.ok)

  if (length(x) == len) {
    invisible(return(TRUE))
  }

  cli_abort(
    c("{.arg {arg}} must be length {len}.",
      "i" = "You've supplied a length {length(x)} object."
    ),
    ...
  )
}

check_grepl <- function(x = NULL, pattern = NULL, arg = caller_arg(x), null.ok = FALSE, ignore.case = FALSE, perl = FALSE, message = NULL, ...) {
  check_null(x, arg, null.ok)

  if (grepl(pattern, x, ignore.case = ignore.case, perl = perl)) {
    invisible(return(TRUE))
  }

  if (is.null(message)) {
    cli_abort(
      "Can't detect pattern {.val {pattern}} in {.arg {arg}}."
    )
  }

  cli_abort(message = message)
}

check_starts_with <- function(x = NULL, string = NULL, arg = caller_arg(x), null.ok = FALSE, ignore.case = FALSE, perl = FALSE, message = NULL, ...) {
  check_character(x, arg, null.ok)

  if (all(grepl(paste0("^", string), x, ignore.case = ignore.case, perl = perl))) {
    invisible(return(TRUE))
  }

  if (is.null(message)) {
    cli_abort(
      c("{.arg {arg}} must start with {.val {string}}.",
        "i" = "The provided string is {.val {x}}."
      ),
      ...
    )
  }

  cli_abort(message = message, ...)
}

#' @noRd
cli_abort_ifnot <- function(..., condition = FALSE, x) {
  if (!is_logical(condition)) {
    condition <- as_function(condition)
    condition <- condition(x)
  }

  if (is_logical(condition) && !condition) {
    cli_abort(...)
  }

  invisible(return(NULL))
}

#' Add default user agent to request
#'
#' @noRd
req_getdata_user <- function(req,
                             string = getOption("getdata.useragent", default = "getdata (https://github.com/elipousson/getdata)")) {
  httr2::req_user_agent(
    req = req,
    string = string
  )
}
