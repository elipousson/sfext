#' Convert an object to a simple feature or bounding box object
#'
#' Both functions will pass a NULL value without returning an error. If a POINT
#' or MULTIPOINT object is passed to [as_bbox] a 0.00000001 meter buffer is
#' applied.
#'
#' @param x A `sf`, `bbox`, `sfc`, `raster`, `sp`, or data frame object that can
#'   be converted into a simple feature or bounding box object. [as_bbox()] can
#'   also convert a vector with xmin, ymin, xmax, and ymax values.
#'   [as_sf_list()] only supports sf objects or a data frames with a sf list
#'   column named "data" (typically created by using [dplyr::group_nest()] on an
#'   sf object.
#' @param sf_col A column name to use for the geometry column created by
#'   [as_sf]; defaults to "geometry".
#' @param crs Coordinate reference system for `sf`, `bbox`, `sfc` or `sf` list
#'   object to return.
#' @param ... Additional parameters passed to [sf::st_bbox()] when calling
#'   [as_bbox()] or passed to [sf::st_sf()], [sf::st_as_sf()], or [df_to_sf()]
#'   for [as_sf()] (depending on class of x)
#' @export
#' @importFrom sf st_sf st_as_sfc st_bbox st_as_sf st_geometry
#' @importFrom dplyr bind_rows rename
as_sf <- function(x, crs = NULL, sf_col = "geometry", ...) {
  if (is_sf(x)) {
    return(as_crs(x, crs = crs))
  }

  # Convert objects to sf if needed
  x_is <-
    dplyr::case_when(
      is_bbox(x) ~ "bbox",
      is_sfg(x) ~ "sfg",
      is_sfc(x) ~ "sfc",
      is_sf_list(x) ~ "sf_list",
      is_raster(x) ~ "raster",
      is_sp(x) ~ "sp",
      is.data.frame(x) ~ "df",
      # FIXME: Converting character strings to sf using as_sf may be an inappropriate pattern
      is_state_name(x) | is_state_geoid(x) ~ "state",
      is_county_geoid(x) | is_county_name(x) ~ "county",
      # FIXME: Is there any better way of testing an address than just confirming it is a character?
      is.character(x) ~ "address"
    )

  if (length(x_is) > 1) {
    x_is <- unique(x_is)
  }

  x <-
    switch(x_is,
      "bbox" = sf_bbox_to_sf(x, ...),
      "sfg" = sf::st_sf(sf::st_sfc(x), ...),
      "sfc" = sf::st_sf(x, ...),
      "sf_list" = dplyr::bind_rows(x),
      "raster" = sf::st_sf(sf::st_as_sfc(sf::st_bbox(x)), ...),
      "sp" = sf::st_as_sf(x, ...),
      "df" = df_to_sf(x, ...),
      "state" = get_states(x, class = "sf"),
      "county" = get_counties(x, class = "sf"),
      "address" = address_to_sf(x)
    )

  if (!is.null(sf_col)) {
    sf::st_geometry(x) <- sf_col
  }

  as_crs(x, crs = crs)
}

#' @name as_bbox
#' @rdname as_sf
#' @export
#' @importFrom sf st_bbox st_as_sf
#' @importFrom dplyr bind_rows
#' @importFrom rlang has_length
as_bbox <- function(x, crs = NULL, ...) {
  if (is_bbox(x)) {
    return(sf_bbox_transform(bbox = x, crs = crs))
  }

  # Convert objects to sf if needed
  x_is <-
    dplyr::case_when(
      rlang::has_length(x, 4) && all(is.numeric(x)) ~ "num_bbox",
      is_geom_type(x, type = c("POINT", "MULTIPOINT")) ~ "sf_pt",
      is_sf(x, ext = TRUE) ~ "sf_or_sfc",
      TRUE ~ "other"
    )

  x <-
    switch(x_is,
      "sf_pt" = sf::st_bbox(st_buffer_ext(x, dist = 0.00000001), ...),
      "sf_or_sfc" = sf::st_bbox(x, ...),
      "num_bbox" = sf::st_bbox(c(
        xmin = x[1], ymin = x[2],
        xmax = x[3], ymax = x[4]
      ),
      crs = crs, ...
      ),
      "other" = sf::st_bbox(as_sf(x), ...)
    )

  sf_bbox_transform(bbox = x, crs = crs)
}


#' @name as_sfc
#' @rdname as_sf
#' @export
#' @importFrom sf st_geometry st_as_sfc
as_sfc <- function(x, crs = NULL, ...) {
  if (is_sfc(x)) {
    return(as_crs(x, crs = crs))
  }

  x_is <-
    dplyr::case_when(
      is_sf(x) ~ "sf",
      is_sfg(x) ~ "sfg",
      TRUE ~ "other"
    )

  x <-
    switch(x_is,
      "sfg" = sf::st_sfc(x, ...),
      "sf" = sf::st_geometry(x, ...),
      "other" = sf::st_geometry(as_sf(x, ...))
    )

  as_crs(x, crs = crs)
}

#' Lightweight alternative to st_transform_ext that assumes x is a sf or sfc
#' object
#'
#' @noRd
#' @importFrom sf st_crs st_transform
as_crs <- function(x, crs = NULL, ...) {
  if (is.null(crs)) {
    return(x)
  }

  if (is_sf(crs, ext = TRUE)) {
    crs <- sf::st_crs(crs)
  }

  if (is.na(sf::st_crs(x))) {
    sf::st_crs(x) <- crs
    return(x)
  }

  if (is_same_crs(x, crs)) {
    return(x)
  }

  sf::st_transform(x, crs = crs, ...)
}

#' @name as_sf_list
#' @rdname as_sf
#' @param nm For [as_sf_list], name(s) for sf list; defaults to "data". If col
#'   is provided, the values of the grouping column are used as names.
#' @param col For [as_sf_list], the name of the column used to group data if x
#'   is a sf object or used to group and nest data before passing to x.
#' @param clean_names If `TRUE`, clean names provided to nm or created based on
#'   value of col using [janitor::clean_names]. If `FALSE`, use names as
#'   provided.
#' @export
#' @importFrom dplyr summarize group_keys group_nest
#' @importFrom janitor make_clean_names
as_sf_list <- function(x, nm = "data", col = NULL, crs = NULL, clean_names = TRUE) {
  stopifnot(
    is.null(col) || (is.character(col) && (length(col) == 1)),
    !is.null(x)
  )

  x_is_sf <- is_sf(x)
  x_is_sf_list <- is_sf_list(x, ext = TRUE)

  if (!x_is_sf_list) {

    # data frame with nested list column named data
    # produced by group_nest w/ keep_all = TRUE
    if (is.data.frame(x) && (rlang::has_name(x, "data")) && is_sf_list(x$data)) {
      if (nm == "data") {
        nm <- NULL
      }

      if (is.character(col)) {
        nm <- dplyr::summarize(x, .data[[col]])[[1]]
      }

      x <- x$data
    } else if (x_is_sf) {
      if (is.null(col)) {
        x <- list(x) # coercible sf object in list length 1
      } else {
        x <- group_by_col(data = x, col = col)
        nm <- dplyr::group_keys(x)[[col]]
        x <- dplyr::group_nest(x, keep = TRUE)
        x <- x$data
      }
    }
  }

  stopifnot(
    is_sf_list(x, ext = TRUE)
  )

  if (is.null(names(x)) && !is.null(nm)) {
    if (clean_names) {
      nm <- janitor::make_clean_names(nm)
    }

    names(x) <- nm
  }

  if (!is.null(crs)) {
    if (is_sf(crs, ext = TRUE)) {
      crs <- sf::st_crs(crs)
    }

    # FIXME: This sets up the possibility of an error if the sf list is bounding
    # boxes
    if (x_is_sf || x_is_sf_list) {
      x <- purrr::map(x, ~ as_crs(.x, crs = crs))
    }
  }

  x
}

#' Convert data to a different class
#'
#' @param data Data that can be converted to sf, sfc, bbox or a sf list object.
#' @param class A class to convert data to; defaults to NULL (which returns
#'   "sf")
#' @param crs coordinate reference system
#' @param ... Additional parameters passed to [as_sf], [as_sfc], [as_bbox], or
#'   [as_sf_list]
#' @noRd
as_sf_class <- function(x, class = NULL, crs = NULL, ...) {
  if (is.null(class)) {
    return(x)
  }

  class <- match.arg(class, c("sf", "sfc", "bbox", "list"))

  switch(class,
    "sf" = as_sf(x, crs = crs, ...),
    "sfc" = as_sfc(x, crs = crs, ...),
    "bbox" = as_bbox(x, crs = crs, ...),
    "list" = as_sf_list(x, crs = crs, ...)
  )
}


#' Convert an sf, numeric, or other object to a POINT (sfg) or POINT, MULTIPOINT, LINESTRING, or MULTILINESTRING (sfc) object
#'
#' Works with sf, sfc, and bbox objects using [sf::st_centroid]. Works with
#' [sf_bbox_point]
#'
#' @details Using as_point:
#'
#' For [as_point], ... is passed to [sf::st_centroid] if ... is a sf, sfc,
#' or bbox object, [sf_bbox_point] includes a bbox object and a string
#' indicating the requested point position, or [sf::st_point] if ... includes a
#' numeric vector.
#'
#' @details Using as_points:
#'
#' For [as_points] parameters are passed to as_point using [purrr::map] and then
#' converted to sfc using [sf::st_as_sfc]. The ... parameters must include a
#' crs, otherwise the crs will be NA for the resulting sfc object.
#'
#' @rdname as_point
#' @name as_points
#' @param ... See details.
#' @param to The geometry type to return, either POINT or MULTIPOINT or
#'   LINESTRING or MULTILINESTRING.
#' @export
#' @importFrom rlang list2 exec
#' @importFrom sf st_union st_centroid st_point st_cast
as_point <- function(..., to = "POINT") {
  params <-
    rlang::list2(...)

  if (length(params) == 1) {
    params <- params[[1]]
  }

  if (is_sfg(params)) {
    params <- as_sfc(params)
  }

  if (is_sf(params, ext = TRUE)) {
    if (is_bbox(params)) {
      params <- as_sfc(params)
    }

    params <- sf::st_union(params)

    return(sf::st_centroid(params)[[1]])
  }

  if (any(sapply(params, is_bbox))) {
    return(rlang::exec(sf_bbox_point, !!!params))
  }

  params <- sf::st_point(params)

  sf::st_cast(params, to = to)
}

#' @rdname as_point
#' @name as_points
#' @export
#' @importFrom rlang list2 has_name
#' @importFrom purrr map
#' @importFrom sf st_as_sfc st_cast
as_points <- function(..., to = "POINT") {
  params <- rlang::list2(...)
  to <- match.arg(to, c("POINT", "MULTIPOINT"))

  if ((is_point(params) && (to == "POINT")) | (is_multipoint(params) && (to == "MULTIPOINT"))) {
    return(params)
  }

  crs <- NULL
  if (rlang::has_name(params, "crs")) {
    crs <- params$crs
    params <- params[names(params) != "crs"]
  } else if (is_sf(params[[1]], ext = TRUE)) {
    crs <- sf::st_crs(params[[1]])
  }
  pts <-
    purrr::map(
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
#' [as_startpoint] and [as_endpoint] require a line parameter that is passed to
#' [lwgeom::st_startpoint] or [lwgeom::st_endpoint] respectively. Both functions
#' always return a sfc object matching the CRS of the input geometry.
#'
#' @name as_startpoints
#' @rdname as_point
#' @export
#' @importFrom rlang list2
as_startpoint <- function(...) {
  is_pkg_installed("lwgeom")
  params <- rlang::list2(...)
  stopifnot(all(sapply(params, is_line)))
  rlang::exec(lwgeom::st_startpoint, !!!params)
}

#' @name as_endpoints
#' @rdname as_point
#' @export
#' @importFrom rlang list2
as_endpoint <- function(...) {
  is_pkg_installed("lwgeom")
  params <- rlang::list2(...)
  stopifnot(all(sapply(params, is_line)))
  rlang::exec(lwgeom::st_endpoint, !!!params)
}

#' @details Using [as_lines]:
#'
#' If params do not have POINT or MULTIPOINT geometry, they are passed to
#' [as_points] to convert to an `sfc` object. If the parameters have  POINT
#' geometry, they are combined to create a MULTIPOINT geometry.
#'
#' For [as_lines] the ... parameters are passed to [as_points] and/or
#' [sf::st_cast].
#'
#' Both as_line and as_lines do not consistently retain the coordinate reference
#' system of the original object but this should be improved in the future.
#'
#' @name as_line
#' @rdname as_point
#' @export
#' @importFrom rlang list2
#' @importFrom sf st_combine st_cast
as_line <- function(..., to = "LINESTRING") {
  params <- rlang::list2(...)
  to <- match.arg(to, c("LINESTRING", "MULTILINESTRING"))

  if (all(sapply(params, is_line)) | all(sapply(params, is_multiline))) {
    return(purrr::map_dfr(params, ~ as_sf(.x, crs = crs)))
  }

  crs <- NULL
  if (rlang::has_name(params, "crs")) {
    crs <- params$crs
    params$crs <- NULL
  } else if (is_sf(params[[1]], ext = TRUE)) {
    crs <- sf::st_crs(params[[1]])
  }

  params <- pluck_len1(params)

  if (!any(c(is_point(params), is_multipoint(params)))) {
    params <- rlang::exec(as_points, !!!params, crs = crs)
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
#' @importFrom rlang list2
#' @importFrom purrr map_dfr
as_lines <- function(..., to = "LINESTRING") {
  params <- rlang::list2(...)
  crs <- NULL
  if (rlang::has_name(params, "crs")) {
    crs <- params$crs
    params$crs <- NULL
  } else if (is_sf(params[[1]], ext = TRUE)) {
    crs <- sf::st_crs(params[[1]])
  }

  if (all(purrr::map_lgl(params, ~ is_line(.x) | is_multiline(.x)))) {
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
#' @importFrom rlang list2
#' @importFrom purrr map_dfr
as_polygons <- function(..., to = "POLYGON") {
  params <- rlang::list2(...)
  crs <- NULL
  if (rlang::has_name(params, "crs")) {
    crs <- params$crs
    params$crs <- NULL
  } else if (is_sf(params[[1]], ext = TRUE)) {
    crs <- sf::st_crs(params[[1]])
  }

  if (all(purrr::map_lgl(params, ~ is_polygon(.x)))) {
    return(purrr::map_dfr(params, ~ as_sf(.x)))
  }

  params <-
    purrr::map_dfr(
      params,
      ~ as_sf(.x)
    )

  suppressWarnings(
    as_sf(sf::st_cast(params, to = to), crs = crs)
  )
}

#' @noRd
pluck_len1 <- function(x) {
  if (length(x) == 1) {
    return(x[[1]])
  }

  return(x)
}


#' Get a list of start and end points for an object
#'
#' @noRd
as_start_end_points <- function(x, crs = 4326, class = "df") {
  is_pkg_installed("lwgeom")

  if (!is_line(x)) {
    x <- as_line(x)
  }

  pts <-
    list(
      start = as_sf(lwgeom::st_startpoint(x)),
      end = as_sf(lwgeom::st_endpoint(x))
    )

  if (!is.null(class) && class == "df") {
    pts$start <- sf_to_df(pts$start, crs = crs)
    pts$end <- sf_to_df(pts$end, crs = crs)
  }

  return(pts)
}

#' Create a LINESTRING based on start and end points
#'
#' @noRd
get_start_end_line <- function(x) {
  pts <- get_start_end_point(x, class = NULL)
  # FIXME: What does as_lines do with similar geometry?
  pts_combined <-
    purrr::map2(
      pts$start,
      pts$end,
      ~ sf::st_cast(sf::st_combine(
        c(.x, .y)
      ), "LINESTRING")
    )

  return(pts_combined)
}
