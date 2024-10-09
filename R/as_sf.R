#' Convert an object to a simple feature or bounding box object
#'
#' Both functions will pass a `NULL` value without returning an error. If a
#' POINT or MULTIPOINT object is passed to [as_bbox()] a 0.00000001 meter buffer
#' is applied. If a character object is passed to [as_bbox()] it is passed to
#' [osmdata::getbb()] using `format_out = "matrix"` which is converted into a
#' bounding box.
#'
#' @param x A `sf`, `bbox`, `sfc`, `raster`, `sp`, or data frame object that can
#'   be converted into a simple feature or bounding box object. [as_bbox()] can
#'   also convert a vector with xmin, ymin, xmax, and ymax values.
#'   [as_sf_list()] only supports sf objects or a data frames with a sf list
#'   column named "data" (typically created by using [dplyr::group_nest()] on an
#'   sf object.
#' @param crs Coordinate reference system for `sf`, `bbox`, `sfc` or `sf` list
#'   object to return.
#' @param sf_col A column name to use for the geometry column created by
#'   [as_sf]; defaults to "geometry".
#' @param ext If `TRUE`, [as_sf] will convert a data frame or character vector
#'   of addresses to an sf object using [df_to_sf] or [address_to_sf]. If
#'   `FALSE`, only spatial objects (bbox, sfg, sfc, sf list, raster, or sp
#'   objects) can be converted. Defaults to `TRUE`.
#' @inheritParams rlang::args_error_context
#' @param ... Additional parameters passed to [sf::st_bbox()] when calling
#'   [as_bbox()] or passed to [sf::st_sf()], [sf::st_as_sf()], or [df_to_sf()]
#'   for [as_sf()] (depending on class of x)
#' @param as_tibble If `TRUE`, always return sf object as a tibble. If `FALSE`,
#'   some conversions may still return a tibble object.
#' @example examples/as_sf.R
#' @export
#' @importFrom dplyr case_when bind_rows
#' @importFrom sf st_sf st_sfc st_as_sfc st_bbox st_as_sf st_geometry
as_sf <- function(x,
                  crs = NULL,
                  sf_col = "geometry",
                  ext = TRUE,
                  ...,
                  as_tibble = TRUE,
                  call = caller_env()) {
  if (is_sf(x)) {
    return(transform_sf(x, crs = crs))
  }

  # Convert objects to sf if needed
  x_is <-
    dplyr::case_when(
      is_bbox(x) ~ "bbox",
      is_sfg(x) ~ "sfg",
      is_sfc(x) ~ "sfc",
      is_sf_list(x) ~ "sf_list",
      is_geo_coords(x) ~ "geo_coords",
      ext && is.data.frame(x) ~ "data.frame",
      # FIXME: Is there any better way of testing an address than just
      # confirming it is a character?
      ext && is.character(x) ~ "address",
      is_raster(x) ~ "raster",
      TRUE ~ "other"
    )

  x <-
    switch(x_is,
      "bbox" = sf_bbox_to_sf(x, ...),
      "sfg" = sf::st_sf(sf::st_sfc(x), ...),
      "sfc" = sf::st_sf(x, ...),
      "sf_list" = sf_list_rbind(x),
      "geo_coords" = sf::st_sf(lonlat_to_sfc(x, ...)),
      "data.frame" = df_to_sf(x, ...),
      "address" = address_to_sf(x, ...),
      "raster" = sf::st_sf(sf::st_as_sfc(sf::st_bbox(x)), ...),
      "other" = try_st_as_sf(x, ..., call = call)
    )

  if (as_tibble) {
    x <- as_sf_tibble(x)
  }

  if (!is_null(sf_col)) {
    sf::st_geometry(x) <- sf_col
  }

  transform_sf(x, crs = crs)
}

#' @noRd
try_st_as_sf <- function(x, ..., call = caller_env()) {
  try_fetch(
    sf::st_as_sf(x, ...),
    error = function(cnd) {
      cli_abort(
        "{.arg x} can't be converted to a {.cls sf} object.",
        parent = cnd,
        call = call
      )
    }
  )
}

#' @name as_bbox
#' @rdname as_sf
#' @export
#' @importFrom sf st_bbox st_as_sf
#' @importFrom dplyr bind_rows
as_bbox <- function(x,
                    crs = NULL,
                    ext = TRUE,
                    ...,
                    call = caller_env()) {
  if (is_bbox(x)) {
    return(sf_bbox_transform(bbox = x, crs = crs))
  }

  if (is_character(x) && has_length(x, 1)) {
    check_installed(
      "osmdata",
      reason = "{.pkg osmdata} must be installed to convert
      {.arg x} to a {.cls bbox}."
    )
    x <- osmdata::getbb(x, format_out = "sf_polygon")
  }

  # Convert objects to sf if needed
  x_is <-
    dplyr::case_when(
      has_length(x, 4) && all(is.numeric(x)) ~ "num_bbox",
      is_geom_type(x, type = c("POINT", "MULTIPOINT")) ~ "sf_pt",
      is_sf(x, ext = TRUE) ~ "sf_or_sfc",
      TRUE ~ "other"
    )

  x <-
    switch(x_is,
      "sf_pt" = sf::st_bbox(st_buffer_ext(x, dist = 0.00000001), ...),
      "sf_or_sfc" = sf::st_bbox(x, ...),
      "num_bbox" = sf::st_bbox(
        c(
          xmin = x[1], ymin = x[2],
          xmax = x[3], ymax = x[4]
        ),
        crs = crs, ...
      ),
      "other" = try_st_bbox(x, ...)
    )

  if (!is_bbox(x)) {
    cli_abort(
      "{.arg x} can't be converted to a {.cls bbox} object.",
      call = call
    )
  }

  sf_bbox_transform(bbox = x, crs = crs)
}

#' @noRd
try_st_bbox <- function(x, ..., call = caller_env()) {
  try_fetch(
    sf::st_bbox(x, ...),
    error = function(cnd) {
      cli_abort(
        "{.arg x} can't be converted to a {.cls bbox} object.",
        parent = cnd,
        call = call
      )
    }
  )
}

#' @name as_sfc
#' @rdname as_sf
#' @export
#' @importFrom dplyr case_when
#' @importFrom sf st_geometry st_sfc st_as_sfc
as_sfc <- function(x, crs = NULL, ext = TRUE, ..., call = caller_env()) {
  if (is_sfc(x)) {
    return(transform_sf(x, crs = crs))
  }

  x_is <-
    dplyr::case_when(
      is_sf(x) ~ "sf",
      # is.list(x) && is_all(x, is_sfc) ~ "sfc_list",
      is_sfg(x) ~ "sfg",
      is.data.frame(x) ~ "df",
      is_geo_coords(x) ~ "geo_coords",
      TRUE ~ "other"
    )

  x <-
    switch(x_is,
      "sf" = sf::st_geometry(x, ...),
      "sfg" = sf::st_sfc(x, ...),
      # "sfc_list" =
      "df" = sf::st_geometry(as_sf(x, ext = ext, ..., call = call)),
      "geo_coords" = lonlat_to_sfc(x, ...),
      "other" = try_st_as_sfc(x, ..., call = call)
    )

  transform_sf(x, crs = crs)
}

#' @noRd
try_st_as_sfc <- function(x, ..., call = caller_env()) {
  try_fetch(
    sf::st_as_sfc(x, ...),
    error = function(cnd) {
      cli_abort(
        "{.arg x} can't be converted to a {.cls sfc} object.",
        parent = cnd,
        call = call
      )
    }
  )
}

#' Convert data to a different class
#'
#' @param x Object to convert to an sf, sfc, bbox or a sf list object.
#' @param class A class to convert data to; defaults to NULL (which returns
#'   "sf")
#' @param allow_null For [as_sf_class], if class is `NULL` and allow_null is `TRUE`,
#'   return x without any class conversion or checks. Defaults to `TRUE`.
#' @param ... Additional parameters passed to [as_sf], [as_sfc], [as_bbox], or
#'   [as_sf_list]
#' @name as_sf_class
#' @rdname as_sf
#' @export
as_sf_class <- function(x,
                        class = NULL,
                        allow_null = TRUE,
                        ...,
                        call = caller_env()) {
  if (allow_null && is_null(class)) {
    return(x)
  }

  class <-
    arg_match0(
      class,
      c("sf", "sfc", "bbox", "list", "data.frame"),
      error_call = call
    )

  if (is_what(x, class)) {

    params <- list2(...)

    if (!is.null(params[["crs"]])) {
      x <- st_transform_ext(x, crs = params[["crs"]])
    }

    return(x)
  }

  cli_abort_ifnot(
    (class != "data.frame") || (is_sf(x) && class == "data.frame"),
    message = "{.arg x} must be an {.arg sf} object if {.arg class} is {.val data.frame}.",
    call = call
  )

  switch(class,
    "sf" = as_sf(x, ..., call = call),
    "sfc" = as_sfc(x, ..., call = call),
    "bbox" = as_bbox(x, ..., call = call),
    "list" = as_sf_list(x, ...),
    "data.frame" = sf_to_df(x, ...)
  )
}

#' Convert data to a data frame with X/Y coordinate pairs
#'
#' Wraps [as_points()], [as_sfc()], and [sf_bbox_point()] to allow the
#' conversion of sf, sfc, or sfg objects into a simple data frame with X and Y
#' columns matching the provided nm parameter.
#'
#' @param x  A length 2 character string or numeric coordinate pair or a `sf`,
#'   `sfc`, or a `bbox` object. If x is a character string (e.g. c("xmin",
#'   "ymax")), a sf, sfc, or bbox object must be provided to data argument.
#' @param bbox A bbox object or object that can be converted with [as_bbox()]
#'   that is passed as bbox to [sf_bbox_point()] when x is passed to the point
#'   parameter.
#' @inheritParams st_transform_ext
#' @param nm Column names to use for X and Y columns.
#' @param ... Additional parameters passed to [sf_bbox_point()], [as_points()],
#'   or [as_sfc()].
#' @name as_xy
#' @export
#' @importFrom dplyr case_when
#' @importFrom sf st_coordinates
as_xy <- function(x,
                  bbox = NULL,
                  crs = NULL,
                  nm = c("x", "y"),
                  ...) {
  type <-
    dplyr::case_when(
      is.character(x) && is_sf(bbox, ext = TRUE) ~ "sf_bbox_point",
      is.numeric(x) | is_sf(x, ext = TRUE) ~ "as_point",
      is_point(x) ~ "point"
    )

  x <-
    switch(type,
      "sf_bbox_point" = sf_bbox_point(as_bbox(bbox),
        point = x, crs = crs, ...
      ),
      "as_point" = as_points(x, crs = crs, ...),
      "point" = as_sfc(x, crs = crs, ...)
    )

  set_names(
    as.data.frame(sf::st_coordinates(x)),
    nm
  )
}

#' @noRd
pluck_len1 <- function(x) {
  if (length(x) == 1) {
    return(x[[1]])
  }

  x
}

#' Get a list of start and end points for an object
#'
#' @noRd
as_start_end_points <- function(x, crs = 4326, class = "data.frame") {
  check_installed("lwgeom")

  if (!is_line(x)) {
    x <- as_line(x)
  }

  pts <-
    list(
      start = as_sf(lwgeom::st_startpoint(x)),
      end = as_sf(lwgeom::st_endpoint(x))
    )

  if (!is_null(class) && class == "data.frame") {
    pts$start <- sf_to_df(pts$start, crs = crs)
    pts$end <- sf_to_df(pts$end, crs = crs)
  }

  pts
}

#' Create a LINESTRING based on start and end points
#'
#' @noRd
#' @importFrom sf st_cast st_combine
get_start_end_line <- function(x) {
  pts <- as_start_end_points(x, class = NULL)
  # FIXME: What does as_lines do with similar geometry?
  map2(
    pts$start,
    pts$end,
    ~ sf::st_cast(sf::st_combine(
      c(.x, .y)
    ), "LINESTRING")
  )
}
