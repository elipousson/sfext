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

  if (is_null(x) && !allow_null) {
    cli_abort("{.arg {arg}} must not be NULL.", ...)
  }

  if (!is_null(x) && null.req) {
    cli_abort("{.arg {arg}} must be NULL.", ...)
  }

  return(invisible(TRUE))
}

#' Check bare numeric
#'
#' @noRd
check_bare_numeric <- function(x,
                               ...,
                               allow_null = FALSE,
                               arg = caller_arg(x),
                               call = caller_env()) {
  if (!missing(x)) {
    if (is_bare_numeric(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a bare numeric vector",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
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
  allow_null <- allow_null && is_null(x)

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
  allow_null <- allow_null && is_null(x)

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
  allow_null <- allow_null && is_null(x)

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

#' Check if the data.frame object has the required paper columns
#'
#' @noRd
check_df_paper <- function(x,
                           ext = FALSE,
                           arg = caller_arg(x),
                           call = caller_env()) {
  check_required(x, arg = arg, call = call)

  # FIXME: Add check to make sure input is a data frame
  paper_names <- c("width", "height", "orientation", "units")

  if (ext) {
    paper_names <- c(paper_names, "asp", "ncol", "nrow")
  }

  check_has_name(x, paper_names, arg = arg, call = call)
}


#' Check if x has names
#'
#' @noRd
#' @importFrom cli cli_vec builtin_theme qty
check_has_name <- function(x,
                           nm,
                           allow_any = FALSE,
                           allow_null = FALSE,
                           arg = caller_arg(x),
                           ...,
                           call = caller_env()) {
  check_required(x, arg = arg, call = call)

  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  check_character(nm, call = call)

  has_nm <- has_name(x, nm)

  if (all(has_nm)) {
    return(invisible(NULL))
  }

  if (allow_any && any(has_nm)) {
    return(invisible(NULL))
  }

  name <- "name"

  if (is.data.frame(x)) {
    name <- "column name"
  }

  n_nm <- length(nm)

  if (allow_any) {
    nm <- .cli_vec_or(nm)
    message <- "{.arg {arg}} must have any of the {name}{qty(n_nm)}{?s} {.val {nm}}"
  } else {
    message <- c(
      "{.arg {arg}} must have all of the {name}{qty(n_nm)}{?s} {.val {nm}}",
        "i" = "The {name}{qty(n_nm)}{?s} {.val {nm[!has_nm]}} are missing."
      )
  }

  cli_abort(
    message = message,
    call = call
  )
}
