#' Convert between simple feature and data frame objects
#'
#' @description
#' [sf_to_df()] converts a simple feature object to a data.frame by dropping
#' geometry or, using [get_coords()], converting geometry to well known text, or
#' (if the geometry type is not POINT) getting coordinates for a centroid or
#' point on surface. If an sfc object is provided,the "drop" geometry option is
#' not supported.
#'
#' [df_to_sf()] converts a data.frame to a simple feature object using a
#' geometry column (named "geometry"), address column (name matching the address
#' parameter), well known text column (named "wkt"), or coords (must have
#' columns matching coords values). Related helper functions include
#' [check_coords()] to suggest the appropriate coordinate column names based on
#' the column names in the provided data frame; [has_coords()] to check if a
#' data frame has the specified coordinates.
#'
#' @param x A `sf` or `sfc` object or a data frame with lat/lon coordinates in a
#'   single column or two separated columns.
#' @param crs Cordinate reference system to return, Default: 4326 for
#'   [sf_to_df()] and `NULL` for [df_to_sf()].
#' @param geometry Type of geometry to include in data frame. options include
#'   "drop", "wkt", "centroid", "point", Default: 'centroid'.
#' @inheritParams coords_to_sf
#' @inheritParams dplyr::left_join
#' @return [sf_to_df()] returns a data frame with geometry dropped or converted
#'   to wkt or coordinates for the centroid or point on surface; [df_to_sf()]
#'   returns a simple feature object with POINT geometry.
#' @param keep_all If `FALSE`, drop all columns other than those named in
#'   coords, Default: `TRUE`.
#' @seealso [sf::st_coordinates()]
#' @example examples/sf_to_df.R
#' @rdname sf_to_df
#' @export
sf_to_df <- function(x,
                     crs = 4326,
                     coords = c("lon", "lat"),
                     geometry = "centroid",
                     keep_all = TRUE) {
  if (geometry == "drop") {
    return(sf::st_drop_geometry(x))
  }

  suppressWarnings(
    get_coords(
      x,
      geometry = geometry,
      crs = crs,
      coords = coords,
      keep_all = keep_all,
      drop = TRUE
    )
  )
}

#' @rdname sf_to_df
#' @name df_to_sf
#' @param from_crs For [df_to_sf()], coordinate reference system used by
#'   coordinates or well known text in data frame.
#' @param geo If `TRUE`, use [address_to_sf()] to geocode address column; defaults
#'   to `FALSE`.
#' @param address Address column name passed to [tidygeocoder::geocode()] or
#'   [tidygeocoder::geo]
#' @param y A sf object passed as y argument to [dplyr::left_join()].
#' @param by A character vector of variables to join by passed to
#'   [dplyr::left_join()].
#' @inheritParams rlang::args_error_context
#' @seealso
#'  [ggspatial::df_spatial()]
#'  [sf::st_as_sf()]
#' @export
#' @importFrom sf st_sf st_geometry st_as_sf
#' @importFrom rlang has_length has_name
df_to_sf <- function(x,
                     crs = NULL,
                     coords = c("lon", "lat"),
                     from_crs = 4326,
                     into = NULL,
                     sep = ",",
                     rev = TRUE,
                     remove_coords = FALSE,
                     geo = FALSE,
                     address = "address",
                     y = NULL,
                     by = NULL,
                     ...,
                     as_tibble = TRUE,
                     call = caller_env()) {
  check_data_frame(x, call = call)

  type <-
    dplyr::case_when(
      has_name(x, "geometry") && !all(has_name(x, coords)) ~ "geometry_df",
      geo && has_name(x, address) && !all(has_name(x, coords)) ~ "address_df",
      identical(coords, "wkt") || has_name(x, "wkt") ~ "wkt_df",
      !is_null(y) ~ "join_sf",
      TRUE ~ "coords_df"
    )

  x <-
    switch(type,
      "geometry_df" = sf::st_as_sf(x),
      "join_sf" = join_sf_to_df(x, y, by = by, ...),
      "address_df" = address_to_sf(
        x,
        address = address,
        coords = coords,
        crs = crs,
        remove_coords = remove_coords,
        ...,
        call = call
      ),
      "wkt_df" = wkt_df_to_sf(x, crs = from_crs),
      "coords_df" = coords_to_sf(
        x,
        coords = coords,
        crs = from_crs,
        into = into,
        sep = sep,
        rev = rev,
        remove_coords = remove_coords,
        call = call
      ),
    )

  if (as_tibble && !is_tibble(x)) {
    x <- sf::st_as_sf(as_tibble(x))
  }

  st_transform_ext(x = x, crs = crs, class = "sf")
}

#' Convert a data frame with a geometry list column to an sf object
#' @noRd
join_sf_to_df <- function(x, y, by = NULL, ...) {
  # FIXME: y could also be a dataframe that could be converted into an sf object
  check_sf(y)

  x <- dplyr::left_join(x = x, y = y, by = by, ...)

  sf::st_as_sf(x)
}

#' Convert a data frame with a wkt column to an sf object
#' @noRd
#' @importFrom sf st_geometry st_as_sfc
wkt_df_to_sf <- function(x, crs = NULL) {
  sf::st_geometry(x) <- sf::st_as_sfc(x$wkt, crs = crs)
  x$wkt <- NULL
  sf::st_as_sf(x, crs = crs)
}

#' Use tidygeocoder to convert an address or data frame with an address column
#' to an sf object
#'
#' Wraps [tidygeocoder::geo()] and [tidygeocoder::geocode()] to convert a
#' character string or a data frame with an address column. Additional
#' parameters passed to [tidygeocoder::geocode()] which passes `...` parameters to
#' [tidygeocoder::geo()].
#'
#' @param x Data frame with an address column. Multiple address columns are not
#'   currently supported.
#' @param address Address column name, Default: 'address'
#' @inheritParams tidygeocoder::geo
#' @inheritParams df_to_sf
#' @inheritDotParams tidygeocoder::geocode
#' @inheritParams rlang::args_error_context
#' @return A `sf` object with POINT geometry for all geocoded addresses with valid coordinates.
#' @seealso
#'  [tidygeocoder::geo()], [tidygeocoder::geocode()]
#' @rdname address_to_sf
#' @export
address_to_sf <- function(x,
                          address = "address",
                          method = "osm",
                          coords = c("lon", "lat"),
                          remove_coords = FALSE,
                          crs = NULL,
                          full_results = FALSE,
                          ...,
                          call = caller_env()) {
  check_installed("tidygeocoder")

  if (is.character(x)) {
    # Make vector into address column
    x <- tibble::as_tibble_col(x, address)
  }

  check_data_frame(x, call = call)

  x <- has_same_name_col(x, col = "lon")
  x <- has_same_name_col(x, col = "lat")

  # Geocode the address column
  x <- tidygeocoder::geocode(
    x,
    address = address,
    long = "lon",
    lat = "lat",
    full_results = full_results,
    quiet = is_interactive(),
    ...
  )

  cli_abort_ifnot(
    (nrow(x) > 0),
    message = "No addresses from {.arg x} could be geocoded.",
    call = call
  )

  x <-
    dplyr::rename(
      x,
      "{coords[[1]]}" := "lon",
      "{coords[[2]]}" := "lat"
    )

  # Convert address df to sf
  df_to_sf(
    x,
    coords = coords,
    from_crs = 4326,
    remove_coords = remove_coords,
    crs = crs
  )
}
