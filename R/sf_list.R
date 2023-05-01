#' Creating and checking sf lists
#'
#' Functions for coercing an object to an sf_list class objects created by
#' [vctrs::new_list_of()] and checking lists of sf objects. Any function with
#' the `allow_list` parameter supports this type of input.
#'
#' @name sf_list
#' @seealso [is_sf()]
NULL

#' @name as_sf_list
#' @rdname sf_list
#' @inheritParams is_sf
#' @param nm For [as_sf_list], name(s) for sf list; defaults to "data". If col
#'   is provided, the values of the grouping column are used as names.
#' @param col For [as_sf_list], the name of the column used to group data if x
#'   is a sf object or used to group and nest data before passing to x.
#' @param clean_names If `TRUE`, clean names provided to nm or created based on
#'   value of col using [janitor::clean_names]. If `FALSE`, use names as
#'   provided.
#' @export
#' @importFrom dplyr summarize group_keys group_nest
as_sf_list <- function(x,
                       nm = "data",
                       col = NULL,
                       crs = NULL,
                       clean_names = TRUE,
                       .name_repair = "check_unique",
                       call = caller_env()) {
  check_required(x, call = call)
  check_string(col, allow_null = TRUE, call = call)

  # data frame with nested list column named data
  # produced by group_nest w/ keep_all = TRUE
  if (is.data.frame(x) && (has_name(x, "data")) && is_sf_list(x$data)) {
    if (nm == "data") {
      nm <- NULL
    }

    if (is.character(col)) {
      nm <- dplyr::summarize(x, .data[[col]])[[1]]
    }

    x <- x$data
  } else if (!is_sf_list(x, ext = TRUE)) {
    x <- new_sf_list(x, nm, col, clean_names, .name_repair, call)
  }

  if (!is_sf_list(x, ext = TRUE)) {
    cli_abort(
      c("{.arg x} must be a list of {.cls sf} objects or a {.cls sf} object that
      can be converted to a list.",
        "i" = "The provided {.arg x} is class {.cls {class(x)}}."
      ),
      call = call
    )
  }

  st_transform_ext(x, crs = crs)
}

#' @rdname sf_list
#' @name new_sf_list
#' @export
new_sf_list <- function(x,
                        nm = "data",
                        col = NULL,
                        clean_names = TRUE,
                        .name_repair = "check_unique",
                        call = caller_env()) {
  check_sf(x, ext = TRUE, call = call)

  if (is_sf(x)) {
    # geometry <- attributes(sf::st_geometry(x))
    class <- "sf_list"
    if (is_null(col)) {
      x <- list(x) # coercible sf object in list length 1
    } else {
      check_installed("tidyr", call = call)
      x <- group_by_col(data = x, col = col, call = call)
      nm <- dplyr::group_keys(x)[[col]]
      x <- tidyr::nest(x)[["data"]]
    }
  }

  if (clean_names) {
    check_installed(
      "janitor",
      reason = "{.pkg janitor} must be installed when
      {.arg clean_names} is {.code TRUE}.",
      call = call
    )
    .name_repair <- janitor::make_clean_names
  }

  x <- set_names_repair(x, nm = nm, .name_repair = .name_repair)

  vctrs::new_list_of(
    x,
    class = class
  )
}

#' @noRd
validate_sf_list <- function(x,
                             arg = caller_arg(x),
                             ...,
                             call = caller_env()) {
  if (inherits_all(x, c("sf_list", "vctrs_list_of"))) {
    return(invisible(x))
  }

  if (is_sf_list(x)) {
    cli_alert_warning("{.arg {arg}} is a list of {.cls sf} objects but is
                      missing class {.cls {c('sf_list', 'vctrs_list_of')}}.",
      wrap = TRUE
    )
    return(invisible(x))
  }

  cli_abort(
    "{.arg {arg}} must be a {.cls sf_list} object.",
    call = call
  )
}

#' @rdname sf_list
#' @name is_sf_list
#' @export
is_sf_list <- function(x, ext = TRUE, allow_null = FALSE) {
  if (allow_null && is_null(x)) {
    return(TRUE)
  }

  if (inherits_all(x, c("sf_list", "vctrs_list_of"))) {
    return(TRUE)
  }

  if (is_sf(x, ext = ext) || !is.list(x)) {
    return(FALSE)
  }

  all(
    vapply(
      x,
      function(x) {
        is_sf(x, ext = ext, allow_null = allow_null)
      },
      TRUE
    )
  )
}
