# ---
# repo: elipousson/sfext
# file: standalone-sfext-utils.R
# last-updated: 2024-10-08
# license: https://creativecommons.org/publicdomain/zero/1.0/
# imports: [cli, rlang, sf, units]
# ---
#
# This file provides a set of helper functions for check the class of sf objects
# and related object types and wrappes for key `{sf}` functions that suppress
# excess warnings and allow NULL inputs
#
# ## Changelog
#
# 2024-10-08:
# * Correct typos for allow_null argument.
# * Correct typo in `.bbox_transform()` function (reference to non-existent bbox argument instead of x).
# * Use standard `sf::st_transform()` to transform `bbox` if sf (>=1.0-17) is installed.
# * Remove `.sf_point_on_surface()` and `.sf_centroid()` functions and add replacement `.sf_point()` function.
# * Add helper `.st_fn()` function.
#
# 2023-09-08:
# * First version of simple helpers and wrappers.
#
# nocov start

# Is x a sfg object?
.is_sfg <- function(x) {
  inherits(x, "sfg")
}

# Is x a sfc object?
.is_sfc <- function(x) {
  inherits(x, "sfc")
}

# Is x a sf object?
.is_sf <- function(x) {
  inherits(x, "sf")
}

# Is x a bbox object?
.is_bbox <- function(x) {
  inherits(x, "bbox")
}

# Is x a units object?
.is_units <- function(x) {
  inherits(x, "units")
}

# Check if x is a sf object or another allowed class
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

  if (!allow_empty && inherits_any(x, c("sf", "sfc")) && any(sf::st_is_empty(x))) {
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
    message <- "{.arg {arg}} must be a {.or {allow_class}} object,
    not a {.obj_simple_type {x}}"
  }

  cli::cli_abort(message, ..., call = call)
}

# Check if x is a sf, sfc, or bbox object
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

# Tranform a sf, sfc, or bbox object to a supplied CRS
.sf_transform <- function(x,
                          crs = NULL,
                          ...,
                          allow_null = TRUE,
                          arg = caller_arg(x),
                          call = caller_env()) {
  .check_sfish(
    x,
    arg = arg,
    allow_null = allow_null,
    allow_bbox = TRUE,
    call = call
  )

  if (is.null(x) || is.null(crs)) {
    return(x)
  }

  if (.is_bbox(x)) {
    return(.bbox_transform(x, crs = crs, ..., arg = arg, call = call))
  }

  sf::st_transform(x, crs = sf::st_crs(crs), ...)
}


# Tranform a bbox object to a supplied CRS
.bbox_transform <- function(x, crs = NULL, ..., allow_null = TRUE) {
  if (allow_null && (is_null(crs) || is_null(x))) {
    return(x)
  }

  if (is_installed("sf", version = "1.0-17")) {
    return(sf::st_transform(x, crs = crs))
  }

  sf::st_bbox(sf::st_transform(sf::st_as_sfc(x), crs = sf::st_crs(crs), ...))
}

# Convert a sf, sfc, or bbox object to a data frame of coordinates
.sf_coords <- function(x,
                       ...,
                       coords = c("lon", "lat"),
                       placement = c("surface", "centroid"),
                       .f = NULL,
                       crs = 4326,
                       allow_null = TRUE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  x <- .sf_point(
    x = x,
    placement = placement,
    .f = .f,
    ...,
    crs = crs,
    allow_null = allow_null,
    arg = arg,
    call = call
  )

  set_names(
    as.data.frame(sf::st_coordinates(x)),
    coords
  )
}

# Convert a sf or sfc object to POINT geometry with a supplied placement or
# function
.sf_point <- function(x,
                      ...,
                      placement = c("surface", "centroid"),
                      .f = NULL,
                      crs = NULL,
                      allow_null = TRUE,
                      arg = caller_arg(x),
                      call = caller_env()) {
  if (is.null(.f)) {
    .f <- switch(placement,
      surface = sf::st_point_on_surface,
      centroid = sf::st_centroid
    )
  }

  .sf_fn(x = x, .f = .f, ..., crs = crs, allow_null = allow_null, arg = arg, call = call)
}

# Apply a spatial transformation function while suppressing warnings
.sf_fn <- function(x,
                   ...,
                   .f = sf::st_centroid,
                   crs = NULL,
                   allow_null = TRUE,
                   arg = caller_arg(x),
                   call = caller_env()) {
  .check_sfish(x, arg = arg, allow_null = allow_null, call = call)

  if (is.null(x)) {
    return(x)
  }

  x <- suppressWarnings(.f(x, ...))
  .sf_transform(x, crs = crs, allow_null = allow_null, arg = arg, call = call)
}

# Apply a buffer
.sf_buffer <- function(x,
                       dist = NULL,
                       unit = NULL,
                       ...,
                       crs = NULL,
                       allow_null = TRUE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  .check_sfish(x, arg = arg, allow_null = allow_null, call = call)

  if (is.null(x)) {
    return(x)
  }

  if (!is.null(dist)) {
    if (!is.null(unit) && !.is_units(dist)) {
      dist <- units::as_units(dist, unit)
    }

    x <- sf::st_buffer(x, dist = dist, ...)
  }

  .sf_transform(x, crs = crs, arg = arg, call = call)
}

# nocov end
