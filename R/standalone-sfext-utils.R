# ---
# repo: elipousson/sfext
# file: standalone-sfext-utils.R
# last-updated: 2023-09-08
# license: https://creativecommons.org/publicdomain/zero/1.0/
# imports: [cli, rlang, sf, units]
# ---
#
# This file provides a set of helper functions for check the class of sf objects
# and related object types and wrappers for key `{sf}` functions that suppress
# excess warnings and allow NULL inputs
#
# ## Changelog
#
# 2023-09-08:
# * First version of simple helpers and wrappers.
#
# nocov start

.is_sfg <- function(x) {
  inherits(x, "sfg")
}

.is_sfc <- function(x) {
  inherits(x, "sfc")
}

.is_sf <- function(x) {
  inherits(x, "sf")
}

.is_sfish <- function(x, class = c("sfc", "sfg")) {
  inherits(x, c("sf", class))
}

.is_bbox <- function(x) {
  inherits(x, "bbox")
}

.is_units <- function(x) {
  inherits(x, "units")
}

.cli_vec_or <- function(x,
                        style = c(
                          cli::builtin_theme()$span.val,
                          "vec-last" = " or "
                        )) {
  cli::cli_vec(nm, style = style)
}

.check_sf <- function(x,
                      ...,
                      allow_empty = TRUE,
                      allow_null = FALSE,
                      allow_class = NULL,
                      allow_bbox = FALSE,
                      arg = caller_arg(x),
                      call = caller_env()) {
  if (allow_null && is.null(x)) {
    return(x)
  }

  if (!allow_empty && .is_sfish(x) && sf::st_is_empty(x)) {
    message <- "{.arg {arg}} can't contain any empty feature geometries."
    cli::cli_abort(message, ..., call = call)
  }

  allow_class <- c("sf", allow_class)

  if (allow_bbox) {
    allow_class <- c(allow_class, "bbox")
  }

  if (inherits_any(x, class = allow_class)) {
    return(x)
  }

  message <- "{.arg {arg}} must be a {allow_class} object,
  not a {.obj_simple_type {x}}"

  if (length(allow_class) > 1) {
    message <- "{.arg {arg}} must be a {.cli_vec_or(allow_class)} object,
    not a {.obj_simple_type {x}}"
  }

  cli::cli_abort(message, ..., call = call)
}

.check_sfish <- function(x,
                         ...,
                         allow_empty = TRUE,
                         allow_null = FALSE,
                         allow_class = c("sfc", "sfg"),
                         allow_bbox = FALSE,
                         arg = caller_arg(x),
                         call = caller_env()) {
  .check_sf(
    x,
    ...,
    allow_empty = allow_empty,
    allow_null = allow_null,
    allow_class = allow_class,
    allow_bbox = allow_bbox,
    arg = arg,
    call = call
  )
}

.sf_transform <- function(x,
                          crs = NULL,
                          ...,
                          allow_null = TRUE,
                          arg = caller_arg(x),
                          call = caller_env()) {
  .check_sfish(
    x,
    arg = arg,
    allow_null = alllow_null,
    allow_bbox = TRUE,
    call = call
  )

  if (.is_bbox(x)) {
    .bbox_transform(x, crs = crs, ...)
  }

  if (is.null(x) || is.null(crs)) {
    return(x)
  }

  sf::st_transform(x, crs = sf::st_crs(crs), ...)
}

.bbox_transform <- function(x, crs = NULL, ..., allow_null = TRUE) {
  if (allow_null && (is_null(crs) || is_null(x))) {
    return(x)
  }

  sf::st_bbox(sf::st_transform(sf::st_as_sfc(bbox), crs = sf::st_crs(crs), ...))
}

.sf_centroid <- function(x,
                         ...,
                         crs = NULL,
                         allow_null = TRUE,
                         arg = caller_arg(x),
                         call = caller_env()) {
  .check_sfish(x, arg = arg, allow_null = alllow_null, call = call)

  if (is.null(x)) {
    return(x)
  }

  x <- suppressWarnings(sf::st_centroid(x, ...))
  .sf_transform(x, crs = crs, allow_null = allow_null)
}

.sf_point_on_surface <- function(x,
                                 ...,
                                 crs = NULL,
                                 allow_null = TRUE,
                                 arg = caller_arg(x),
                                 call = caller_env()) {
  .check_sfish(x, arg = arg, allow_null = alllow_null, call = call)

  if (is.null(x)) {
    return(x)
  }

  x <- suppressWarnings(sf::st_point_on_surface(x, ...))
  .sf_transform(x, crs = crs, allow_null = allow_null)
}

.sf_buffer <- function(x,
                       dist = NULL,
                       unit = NULL,
                       ...,
                       crs = NULL,
                       allow_null = TRUE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  .check_sfish(x, arg = arg, allow_null = alllow_null, call = call)

  if (is.null(x)) {
    return(x)
  }

  if (!is.null(dist)) {
    if (!is.null(unit) && !.is_units(dist)) {
      dist <- units::as_units(dist, unit)
    }

    x <- sf::st_buffer(x, dist = dist, ...)
  }

  .sf_transform(x, crs = crs)
}

# nocov end
