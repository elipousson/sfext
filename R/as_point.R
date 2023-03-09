#' Convert an sf, numeric, or other object to a POINT (sfg) or POINT,
#' MULTIPOINT, LINESTRING, or MULTILINESTRING (sfc) object
#'
#' Works with sf, sfc, and bbox objects using [sf::st_centroid()]. Works with
#' [sf_bbox_point()]
#'
#' @details Using as_point:
#'
#' [as_point()] always returns a single point sfg object. The ... parameter is
#' passed to [sf::st_centroid()] if ... is a sf, sfc, or bbox object,
#' [sf_bbox_point()] includes a bbox object and a string indicating the
#' requested point position, or [sf::st_point()] if ... includes a numeric
#' vector.
#'
#' @details Using as_points:
#'
#' [as_points()] always returns an sfc object. The parameters are passed to
#' as_point using [purrr::map] and then converted to sfc using
#' [sf::st_as_sfc()]. The ... parameters must include a crs, otherwise the crs
#' will be NA for the resulting sfc object.
#'
#' @rdname as_point
#' @name as_points
#' @param ... See details.
#' @param to The geometry type to return, either POINT or MULTIPOINT or
#'   LINESTRING or MULTILINESTRING.
#' @param call Passed as the call parameter for [cli::cli_abort] or
#'   [rlang::arg_match] to improve error messages when function is used
#'   internally.
#' @export
#' @importFrom sf st_union st_centroid st_point st_cast
as_point <- function(..., to = "POINT") {
  params <- list2(...)

  if (length(params) == 1) {
    params <- params[[1]]
  }

  if (is_sfg(params)) {
    params <- as_sfc(params)
  }

  if (is_sf(params, ext = TRUE)) {
    if (is_bbox(params)) {
      params <- sf_bbox_to_sfc(params)
    }

    params <- sf::st_union(params)

    return(sf::st_centroid(params)[[1]])
  }

  if (is_any(params, is_bbox)) {
    params[["crs"]] <- NA_character_
    return(exec(sf_bbox_point, !!!params))
  }

  params <- sf::st_point(params)

  sf::st_cast(params, to = to)
}

#' @rdname as_point
#' @name as_points
#' @export
#' @importFrom purrr map
#' @importFrom sf st_as_sfc st_cast
as_points <- function(..., to = "POINT", call = caller_env()) {
  params <- list2(...)
  to <- arg_match(to, c("POINT", "MULTIPOINT"), error_call = call)

  if ((is_point(params) && (to == "POINT")) | (is_multipoint(params) && (to == "MULTIPOINT"))) {
    return(params)
  }

  crs <- NULL
  if (has_name(params, "crs")) {
    crs <- as_crs(params$crs, check = TRUE, call = call)
    params <- params[names(params) != "crs"]
  } else if (is_sf(params[[1]], ext = TRUE)) {
    crs <- sf::st_crs(params[[1]])
  }
  pts <-
    map(
      params,
      ~ as_point(.x)
    )

  if (is.null(crs)) {
    x <- sf::st_as_sfc(pts)
  } else {
    x <- sf::st_as_sfc(pts, crs = crs)
  }

  # See https://github.com/r-spatial/sf/issues/114
  sf::st_cast(x, to = to)
}

#' @details Using [as_startpoint] and [as_endpoint]:
#'
#' [as_startpoint()] and [as_endpoint()] require a LINESTRING OR MULTILINESTRING
#' geometry type sf or sfc object that is passed to [lwgeom::st_startpoint()] or
#' [lwgeom::st_endpoint()] respectively. Both functions always return a sfc
#' object matching the CRS of the input geometry.
#'
#' @name as_startpoints
#' @rdname as_point
#' @export
as_startpoint <- function(x) {
  rlang::check_installed("lwgeom")

  cliExtras::cli_abort_ifnot(
    "Must have LINESTRING or MULTILINESTRING geometry.",
    condition = is_line(x) | is_multiline(x)
  )

  if (is_multiline(x)) {
    x <- suppressWarnings(sf::st_cast(as_sf(x), "LINESTRING"))
    x <- x[1, ]
  }

  lwgeom::st_startpoint(x)
}

#' @name as_endpoints
#' @rdname as_point
#' @export
as_endpoint <- function(x) {
  rlang::check_installed("lwgeom")

  cliExtras::cli_abort_ifnot(
    "Must have LINESTRING or MULTILINESTRING geometry.",
    condition = is_line(x) | is_multiline(x)
  )

  if (is_multiline(x)) {
    x <- suppressWarnings(sf::st_cast(as_sf(x), "LINESTRING"))
    x <- x[nrow(x), ]
  }

  lwgeom::st_endpoint(x)
}

#' @details Using [as_lines]:
#'
#' If params do not have POINT or MULTIPOINT geometry, they are passed to
#' [as_points()] to convert to an `sfc` object. If the parameters have  POINT
#' geometry, they are combined to create a MULTIPOINT geometry.
#'
#' For [as_lines()] the ... parameters are passed to [as_points()] and/or
#' [sf::st_cast()].
#'
#' Both as_line and as_lines do not consistently retain the coordinate reference
#' system of the original object but this should be improved in the future.
#'
#' @name as_line
#' @rdname as_point
#' @export
#' @importFrom sf st_crs st_combine st_cast
#' @importFrom purrr map_dfr
as_line <- function(..., to = "LINESTRING", call = caller_env()) {
  params <- list2(...)
  to <- arg_match(to, c("LINESTRING", "MULTILINESTRING"), error_call = call)

  if (all(sapply(params, is_line)) | all(sapply(params, is_multiline))) {
    return(purrr::map_dfr(params, ~ as_sf(.x, crs = crs)))
  }

  crs <- NULL
  if (has_name(params, "crs")) {
    crs <- params$crs
    params$crs <- NULL
  } else if (is_sf(params[[1]], ext = TRUE)) {
    crs <- sf::st_crs(params[[1]])
  }

  params <- pluck_len1(params)

  if (!any(c(is_point(params), is_multipoint(params)))) {
    params <- exec(as_points, !!!params, crs = crs)
  }

  if (is_point(params)) {
    params <- sf::st_combine(params)
  }

  stopifnot(
    is_multipoint(params)
  )

  as_sf(sf::st_cast(params, to = to), crs = crs)
}

#' @name as_lines
#' @rdname as_point
#' @export
#' @importFrom sf st_crs st_cast
#' @importFrom purrr map_dfr
as_lines <- function(..., to = "LINESTRING") {
  params <- list2(...)
  crs <- NULL
  if (has_name(params, "crs")) {
    crs <- params$crs
    params$crs <- NULL
  } else if (is_sf(params[[1]], ext = TRUE)) {
    crs <- sf::st_crs(params[[1]])
  }

  if (all(map_lgl(params, ~ is_line(.x) | is_multiline(.x)))) {
    return(purrr::map_dfr(params, ~ as_sf(.x)))
  }

  params <-
    purrr::map_dfr(
      params,
      ~ as_sf(as_line(.x))
    )

  as_sf(sf::st_cast(params, to = to), crs = crs)
}

#' @name as_polygons
#' @rdname as_point
#' @export
#' @importFrom purrr map_dfr map_lgl
as_polygons <- function(..., to = "POLYGON") {
  params <- list2(...)
  crs <- NULL
  if (has_name(params, "crs")) {
    crs <- params$crs
    params$crs <- NULL
  } else if (is_sf(params[[1]], ext = TRUE)) {
    crs <- sf::st_crs(params[[1]])
  }

  if (all(purrr::map_lgl(params, ~ is_polygon(.x)))) {
    return(purrr::map_dfr(params, ~ as_sf(.x)))
  }

  params <- purrr::map_dfr(params, ~ as_sf(.x))

  suppressWarnings(
    as_sf(sf::st_cast(params, to = to), crs = crs)
  )
}

#' @details Using [as_centroid()]
#'
#' [as_centroid()] always returns a sfc object with the same length and crs as
#' the input object.
#'
#' @name as_centroid
#' @rdname as_point
#' @param x A `sf`, `sfc`, or `bbox` object.
#' @export
#' @importFrom sf st_centroid st_geometry
as_centroid <- function(x, ...) {
  if (is_bbox(x)) {
    x <- sf_bbox_to_sfc(x)
  }

  suppressWarnings(sf::st_geometry(sf::st_centroid(x, ...)))
}
