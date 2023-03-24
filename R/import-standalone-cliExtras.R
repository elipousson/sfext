# Standalone file: do not edit by hand
# Source: <https://github.com/elipousson/cliExtras/blob/main/R/standalone-cliExtras.R>
# ----------------------------------------------------------------------
#
# ---
# repo: elipousson/cliExtras
# file: standalone-cliExtras.R
# last-updated: 2023-03-21
# license: https://unlicense.org
# imports: rlang, cli
# ---
#
# ## Changelog
#
# 2023-03-22:
# * Added `cli_ifnot()`
#
# 2023-03-21:
# * Added `cli_quiet()` and `cli_if()`
#
# nocov start
cli_quiet <- function(quiet = FALSE,
                      push = FALSE,
                      .frame = rlang::caller_env()) {
  if (rlang::is_false(quiet)) {
    return(invisible(NULL))
  }

  if (rlang::is_true(push)) {
    return(rlang::push_options("cli.default_handler" = suppressMessages))
  }

  rlang::local_options("cli.default_handler" = suppressMessages, .frame = .frame)
}

cli_if <- function(x = NULL,
                   ...,
                   .predicate = rlang::is_true,
                   .fn = NULL,
                   .default = cli::cli_alert,
                   call = rlang::caller_env()) {
  check <- rlang::try_fetch(
    .predicate(x),
    error = function(cnd) cnd
  )

  if (!rlang::is_bool(check)) {
    parent <- NULL
    if (rlang::is_error(check)) {
      parent <- check
    }

    cli::cli_abort(
      "{.fn {.predicate}} must return a {.cls logical} object,
      not {.obj_type_friendly {check}}.",
      call = call,
      parent = parent
    )
  }

  if (rlang::is_true(check)) {
    .fn <- .fn %||% .default
    fn_call <- rlang::call2(.fn, ...)
    if (rlang::has_name(rlang::call_args_names(fn_call), "call")) {
      fn_call <- rlang::call_modify(fn_call, call = call, .homonyms = "last")
    }
    eval(fn_call)
  }
}

cli_ifnot <- function(x = NULL,
                      ...,
                      .predicate = rlang::is_false,
                      .fn = NULL,
                      .default = cli::cli_alert,
                      call = rlang::caller_env()) {
  cli_if(
    x = x,
    ...,
    .predicate = .predicate,
    .fn = .fn,
    .default = .default,
    call = call
  )
}
# nocov end
