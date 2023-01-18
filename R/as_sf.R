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
#' @param ext If `TRUE`, [as_sf] will convert a data frame or character vector
#'   of addresses to an sf object using [df_to_sf] or [address_to_sf]. If
#'   `FALSE`, only spatial objects (bbox, sfg, sfc, sf list, raster, or sp
#'   objects) can be converted. Defaults to `TRUE`.
#' @param sf_col A column name to use for the geometry column created by
#'   [as_sf]; defaults to "geometry".
#' @param crs Coordinate reference system for `sf`, `bbox`, `sfc` or `sf` list
#'   object to return.
#' @param call Passed as the call parameter for [cli::cli_abort] or
#'   [rlang::arg_match] to improve error messages when function is used
#'   internally.
#' @param ... Additional parameters passed to [sf::st_bbox()] when calling
#'   [as_bbox()] or passed to [sf::st_sf()], [sf::st_as_sf()], or [df_to_sf()]
#'   for [as_sf()] (depending on class of x)
#' @export
#' @importFrom sf st_sf st_as_sfc st_bbox st_as_sf st_geometry
#' @importFrom dplyr bind_rows rename
as_sf <- function(x, crs = NULL, sf_col = "geometry", ext = TRUE, ...) {
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
      is_raster(x) ~ "raster",
      is_sp(x) ~ "sp",
      ext && is.data.frame(x) ~ "data.frame",
      # FIXME: Is there any better way of testing an address than just confirming it is a character?
      ext && is.character(x) ~ "address"
    )

  # FIXME: When is x_is more than length 1?
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
      "data.frame" = df_to_sf(x, ...),
      "address" = address_to_sf(x, ...)
    )

  if (!is.null(sf_col)) {
    sf::st_geometry(x) <- sf_col
  }

  transform_sf(x, crs = crs)
}

#' @name as_bbox
#' @rdname as_sf
#' @export
#' @importFrom sf st_bbox st_as_sf
#' @importFrom dplyr bind_rows
as_bbox <- function(x, crs = NULL, ext = TRUE, ...) {
  if (is_bbox(x)) {
    return(sf_bbox_transform(bbox = x, crs = crs))
  }

  if (is.character(x) && rlang::has_length(x, 1)) {
    rlang::check_installed("osmdata")
    x <- osmdata::getbb(x, format_out = "matrix", ...)
    stopifnot(
      !any(is.na(x))
    )
    crs <- 4326
  }

  # Convert objects to sf if needed
  x_is <-
    dplyr::case_when(
      has_length(x, 4) && all(is.numeric(x)) ~ "num_bbox",
      is_geom_type(x, type = c("POINT", "MULTIPOINT")) ~ "sf_pt",
      is_sf(x, ext = TRUE) ~ "sf_or_sfc",
      is.character(x) ~ "char",
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
      "char" = osmdata::getbb(x, ...),
      "other" = sf::st_bbox(as_sf(x, ext = ext), ...)
    )

  sf_bbox_transform(bbox = x, crs = crs)
}


#' @name as_sfc
#' @rdname as_sf
#' @export
#' @importFrom sf st_geometry st_as_sfc
as_sfc <- function(x, crs = NULL, ext = TRUE, ...) {
  if (is_sfc(x)) {
    return(transform_sf(x, crs = crs))
  }

  x_is <-
    dplyr::case_when(
      is_sf(x) ~ "sf",
      # is.list(x) && is_all(x, is_sfc) ~ "sfc_list",
      is_sfg(x) ~ "sfg",
      is.data.frame(x) ~ "df",
      TRUE ~ "other"
    )

  x <-
    switch(x_is,
      "sf" = sf::st_geometry(x, ...),
      "sfg" = sf::st_sfc(x, ...),
      # "sfc_list" =
      "df" = sf::st_geometry(as_sf(x, ext = ext, ...)),
      "other" = sf::st_as_sfc(x, ...)
    )

  transform_sf(x, crs = crs)
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
  check_null(x)
  check_character(col, null.ok = TRUE)
  check_len(col, len = 1, null.ok = TRUE)

  if (!is_sf_list(x, ext = TRUE)) {
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
    } else if (is_sf(x)) {
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

  if (!is_sf_list(x, ext = TRUE)) {
    cli_abort(
      c("{.arg x} must be a list of {.cls sf} objects or a {.cls sf} object that
      can be converted to a list.",
        "i" = "The provided {.arg x} is class {.cls {class(x)}}."
      )
    )
  }

  if (is.null(names(x)) && !is.null(nm)) {
    if (clean_names) {
      nm <- janitor::make_clean_names(nm)
    }

    cli_abort_ifnot(
      c("{.arg x} and {.arg nm} must be the same length.",
        "i" = "{.arg x} is length {.val {length(x)}}.",
        "i" = "{.arg nm} is length {.val {length(nm)}}."
      ),
      condition = length(nm) == length(x)
    )

    names(x) <- nm
  }

  st_transform_ext(x, crs = crs)
}

#' Make a sf list by grid position
#'
#' Create a grid with [st_make_grid_ext()] and
#'
#' @name make_sf_grid_list
#' @inheritParams st_make_grid_ext
#' @inheritParams as_sf_list
#' @export
make_sf_grid_list <- function(x, style = "rect", ncol = 2, nrow = 2, .id = "grid_id", crs = NULL, ...) {
  grid <- st_make_grid_ext(x, style = style, ncol = ncol, nrow = nrow, .id = .id, ...)

  x <- st_join_ext(x, grid, largest = TRUE)

  x <-
    relocate_sf_col(
      dplyr::mutate(
        x,
        "{.id}_col_row" := as.character(glue("col_{col}_row_{row}"))
      )
    )

  as_sf_list(x, col = paste0(.id, "_col_row"), crs = crs)
}

#' Convert data to a different class
#'
#' @param data Data that can be converted to sf, sfc, bbox or a sf list object.
#' @param class A class to convert data to; defaults to NULL (which returns
#'   "sf")
#' @param null.ok For [as_sf_class], if class is `NULL` and null.ok is `TRUE`,
#'   return x without any class conversion or checks. Defaults to `TRUE`.
#' @param ... Additional parameters passed to [as_sf], [as_sfc], [as_bbox], or
#'   [as_sf_list]
#' @name as_sf_class
#' @rdname as_sf
#' @export
as_sf_class <- function(x, class = NULL, null.ok = TRUE, call = caller_env(), ...) {
  if (is.null(class) && null.ok) {
    return(x)
  }

  class <-
    arg_match(class, c("sf", "sfc", "bbox", "list", "data.frame"), error_call = call)

  if (is_class(x, class)) {
    return(x)
  }

  cli_abort_ifnot(
    "{.arg x} must be an {.arg sf} object if {.arg class} is {.val data.frame}.",
    condition = (class != "data.frame") | (is_sf(x) && class == "data.frame")
  )

  switch(class,
    "sf" = as_sf(x, ...),
    "sfc" = as_sfc(x, ...),
    "bbox" = as_bbox(x, ...),
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
#' @param nm Column names to use for X and Y columns.
#' @name as_xy
#' @export
#' @importFrom dplyr case_when
#' @importFrom sf st_coordinates
#' @importFrom rlang set_names
as_xy <- function(x,
                  data = NULL,
                  crs = NULL,
                  nm = c("x", "y"),
                  ...) {
  type <-
    dplyr::case_when(
      is.character(x) && is_sf(data, ext = TRUE) ~ "sf_bbox_point",
      is.numeric(x) | is_sf(x, ext = TRUE) ~ "as_point",
      is_point(x) ~ "point"
    )

  x <-
    switch(type,
      "sf_bbox_point" = sf_bbox_point(as_bbox(data),
        point = x, crs = crs, ...
      ),
      "as_point" = as_points(x, crs = crs, ...),
      "point" = as_sfc(x, crs = crs, ...)
    )

  rlang::set_names(
    as.data.frame(sf::st_coordinates(x)),
    nm
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
as_start_end_points <- function(x, crs = 4326, class = "data.frame") {
  rlang::check_installed("lwgeom")

  if (!is_line(x)) {
    x <- as_line(x)
  }

  pts <-
    list(
      start = as_sf(lwgeom::st_startpoint(x)),
      end = as_sf(lwgeom::st_endpoint(x))
    )

  if (!is.null(class) && class == "data.frame") {
    pts$start <- sf_to_df(pts$start, crs = crs)
    pts$end <- sf_to_df(pts$end, crs = crs)
  }

  return(pts)
}

#' Create a LINESTRING based on start and end points
#'
#' @noRd
#' @importFrom purrr map2
#' @importFrom sf st_cast st_combine
get_start_end_line <- function(x) {
  pts <- as_start_end_points(x, class = NULL)
  # FIXME: What does as_lines do with similar geometry?
  purrr::map2(
    pts$start,
    pts$end,
    ~ sf::st_cast(sf::st_combine(
      c(.x, .y)
    ), "LINESTRING")
  )
}
