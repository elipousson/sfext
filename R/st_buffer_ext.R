#' Buffer a simple feature or bounding box object
#'
#' Return an sf object with a buffer based on `dist` or a proportion of the
#' diagonal distance defined by `diag_ratio`. If x uses geographic coordinates,
#' the coordinate reference system is transformed into EPSG:3857 and then
#' transformed back into the original CRS after the buffer has been applied.
#'
#' [st_edge()] is a variation on [st_buffer_ext()] where dist or diag_ratio is used to
#' define the width of the edge to return either outside the existing geometry
#' (for positive dist values) or inside the existing geometry (for negative dist
#' values).
#'
#' @param x A `sf`, `sfc`, or `bbox` object or a list of `sf` objects.
#' @param dist buffer distance in units. Optional.
#' @param diag_ratio ratio of diagonal distance of area's bounding box used as
#'   buffer distance. e.g. if the diagonal distance is 3000 meters and the
#'   "diag_ratio = 0.1"  a 300 meter will be used. Ignored when `dist` is
#'   provided.
#' @param unit Units for buffer. Supported options include "meter", "foot",
#'   "kilometer", and "mile", "nautical mile" Common abbreviations (e.g. "km"
#'   instead of "kilometer") are also supported. Distance in units is converted
#'   to units matching GDAL units for x; defaults to "meter"
#' @param dist_limits Numeric vector of any length (minimum and maximum values
#'   used as lower and upper limits on distance buffer). Units must match the
#'   provided units; defaults to `NULL`.
#' @param end_style "round" (default), "flat", or "square" passed to the
#'   endCapStyle parameter of [sf::st_buffer()].
#' @param join_style "round" (default), "mitre", or "bevel" passed to the
#'   joinStyle parameter of [sf::st_buffer()].
#' @param single_side If `TRUE`, single-sided buffers are returned for linear
#'   geometries, in which case negative dist values give buffers on the
#'   right-hand side, positive on the left.
#' @param allow_null If `TRUE` (default) and x is `NULL`, a `NULL` value is
#'   returned with no error.
#' @param allow_list If `TRUE` (default), allow sf list objects as an input and use
#'   [purrr::map()] to apply the provided parameters to each object within the
#'   list to return as a new sf list object.
#' @param ... Additional parameters passed to [sf::st_buffer()]
#' @export
#' @importFrom sf st_is_longlat st_crs st_transform st_bbox st_buffer
#' @importFrom units set_units drop_units
st_buffer_ext <- function(x,
                          dist = NULL,
                          diag_ratio = NULL,
                          unit = "meter",
                          dist_limits = NULL,
                          end_style = NULL,
                          join_style = NULL,
                          single_side = FALSE,
                          allow_null = TRUE,
                          allow_list = TRUE,
                          ...) {
  UseMethod("st_buffer_ext")
}

#' @name st_buffer_ext
#' @export
st_buffer_ext.default <- function(x,
                                  dist = NULL,
                                  diag_ratio = NULL,
                                  unit = "meter",
                                  dist_limits = NULL,
                                  end_style = NULL,
                                  join_style = NULL,
                                  single_side = FALSE,
                                  allow_null = TRUE,
                                  ...) {
  # If dist is NULL and diag_ratio is NULL return x (with bbox converted to sf
  # if no buffer applied)
  if (allow_null && is_null(x)) {
    return(x)
  }

  if (is_null(dist) && is_null(diag_ratio)) {
    return(x)
  }

  check_sf(x, ext = "sfc")

  # If longlat, save crs and transform to suggested crs
  is_lonlat <- sf::st_is_longlat(x)

  if (is_lonlat) {
    lonlat_crs <- sf::st_crs(x)
    x <- sf::st_transform(x, 3857)
  }

  # Get crs and rename gdal units to match options for set_units
  crs <- sf::st_crs(x)

  dist <- dist %||% sf_bbox_diag_ratio_to_dist(as_bbox(x), diag_ratio)

  if (is_units(dist)) {
    unit <- unit %||% get_dist_units(dist, quiet = TRUE)
  }

  units_gdal <- crs$units_gdal

  dist <- convert_dist_units(dist = dist, from = unit, to = units_gdal)

  dist <- limit_dist(
    dist = dist,
    dist_limits = dist_limits,
    unit = unit,
    crs = crs
  )

  # FIXME: Why is this needed?
  if (!is_null(end_style)) {
    dist <- as.numeric(dist)
  }

  end_style <- end_style %||% "round"
  join_style <- join_style %||% "round"

  end_style <- arg_match(end_style, c("round", "flat", "square"))
  join_style <- arg_match(join_style, c("round", "mitre", "bevel"))

  x <-
    sf::st_buffer(
      x = x,
      dist = dist,
      singleSide = single_side,
      endCapStyle = toupper(end_style),
      joinStyle = toupper(join_style),
      ...
    )

  if (!is_lonlat) {
    return(x)
  }

  sf::st_transform(x, lonlat_crs)
}

#' @name st_buffer_ext
#' @export
st_buffer_ext.bbox <- function(x,
                               dist = NULL,
                               diag_ratio = NULL,
                               unit = "meter",
                               dist_limits = NULL,
                               end_style = NULL,
                               join_style = NULL,
                               single_side = FALSE,
                               ...) {
  sf::st_bbox(
    st_buffer_ext.default(
      x = sf_bbox_to_sfc(x),
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      dist_limits = dist_limits,
      end_style = end_style,
      join_style = join_style,
      single_side = single_side,
      ...
    )
  )
}

#' @name st_buffer_ext
#' @export
st_buffer_ext.list <- function(x,
                               dist = NULL,
                               diag_ratio = NULL,
                               unit = "meter",
                               dist_limits = NULL,
                               end_style = NULL,
                               join_style = NULL,
                               single_side = FALSE,
                               allow_null = TRUE,
                               allow_list = TRUE,
                               ...) {
  if (!allow_list) {
    cli_abort(
      "{.arg allow_list} must be {.code TRUE} is {.arg x} is a {.cls list}."
    )
  }

  map(
    x,
    function(x, ...) {
      st_buffer_ext.default(
        x,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        dist_limits = dist_limits,
        end_style = end_style,
        join_style = join_style,
        single_side = single_side,
        allow_null = allow_null,
        ...
      )
    }
  )
}

#' @name st_buffer_ext
#' @export
st_buffer_ext.sf_list <- function(x,
                                  dist = NULL,
                                  diag_ratio = NULL,
                                  unit = "meter",
                                  dist_limits = NULL,
                                  end_style = NULL,
                                  join_style = NULL,
                                  single_side = FALSE,
                                  allow_null = TRUE,
                                  allow_list = TRUE,
                                  ...) {
  if (!allow_list) {
    cli_abort(
      "{.arg allow_list} must be {.code TRUE} is {.arg x} is a {.cls sf_list}."
    )
  }

  map_as_sf_list(
    x,
    function(x, ...) {
      st_buffer_ext.default(
        x,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        dist_limits = dist_limits,
        end_style = end_style,
        join_style = join_style,
        single_side = single_side,
        allow_null = allow_null,
        ...
      )
    }
  )
}

#' Limit distance to the min/max values of dist_limits
#'
#' @noRd
#' @importFrom dplyr between
#' @importFrom cli cli_alert_info
limit_dist <- function(dist = NULL,
                       dist_limits = NULL,
                       unit = NULL,
                       crs = NULL,
                       between.ok = FALSE,
                       call = caller_env()) {
  if (is_null(dist_limits)) {
    return(dist)
  }

  units_gdal <- crs$units_gdal

  # dist should already be a units object
  input_dist <-
    convert_dist_units(
      dist = dist,
      from = unit,
      to = units_gdal
    )

  dist_limits <-
    convert_dist_units(
      dist = dist_limits,
      from = unit,
      to = units_gdal
    )

  cli_abort_ifnot(
    (length(dist_limits) >= 2) && is_units(dist_limits),
    message = "{.arg dist_limits} must be length 2 or greater
    and a {.code units} class object.",
    call = call
  )

  min_limit <- min(dist_limits)
  max_limit <- max(dist_limits)

  dist_between <-
    suppressWarnings(
      dplyr::between(
        input_dist,
        min_limit,
        max_limit
      )
    )

  if (dist_between && between.ok) {
    return(dist)
  }

  compared_to <-
    dplyr::case_when(
      dist_between ~ "between",
      (input_dist < min_limit) ~ "below",
      (input_dist > max_limit) ~ "above"
    )

  dist <-
    switch(compared_to,
      "between" = dist_limits[[which.min(abs(dist_limits - input_dist))]],
      "below" = min_limit,
      "above" = max_limit
    )

  input_label <- dist_unit_label(input_dist, to = unit)
  limit_label <- dist_unit_label(dist, to = unit)

  message <-
    switch(compared_to,
      "between" = c("The buffer dist ({input_label}) is between the min/max distance limits.",
        "v" = "Replacing with nearest distance limit ({limit_label})."
      ),
      "below" = "Replacing buffer dist ({input_label}) with the minimum limit ({limit_label}).",
      "above" = "Replacing buffer dist ({input_label}) with the maximum limit ({limit_label})."
    )

  cli_inform(
    message = message
  )

  dist
}

#' @noRd
dist_unit_label <- function(x, to = NULL) {
  if (!is_null(to)) {
    x <- convert_dist_units(x, to = to)
    unit_label <- to
  } else {
    unit_label <- get_dist_units(x)
  }

  paste(as.character(x), unit_label)
}

#' @rdname st_buffer_ext
#' @name st_edge
#' @export
st_edge <- function(x,
                    dist = NULL,
                    diag_ratio = NULL,
                    unit = "meter",
                    ...) {
  if (is_null(dist) && is_null(diag_ratio)) {
    return(x)
  }

  x_dist <- st_buffer_ext(
    x,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit,
    ...
  )

  if (is_true(dist > 0 || diag_ratio > 0)) {
    return(st_erase(x_dist, x))
  }

  st_erase(x, x_dist)
}
