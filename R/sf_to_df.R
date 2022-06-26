#' Convert between simple feature and data frame objects
#'
#' Helper function to convert a simple feature object to data frame by dropping
#' geometry, converting geometry to well known text, or (if the geometry type is
#' not POINT) getting coordinates for a centroid or point on surface. If an sfc
#' object is provided,the "drop" geometry option is not supported.
#'
#' [check_coords()] is a helper function used by [df_to_sf()] to suggest the
#' appropriate coordinate column names based on the column names in the provided
#' data frame.
#'
#' @param x A `sf` or `sfc` object or a data frame with lat/lon coordinates in a
#'   single column or two separated columns.
#' @param crs Cordinate reference system to return, Default: 4326 for [sf_to_df]
#'   and NULL for [df_to_sf]
#' @param geometry Type of geometry to include in data frame. options include
#'   "drop", "wkt", "centroid", "point", Default: 'centroid'.
#' @param coords Coordinate columns for input dataframe or output sf object (if
#'   geometry is 'centroid' or 'point') Default: c("lon", "lat").
#' @param remove_coords For [df_to_sf], if `TRUE`, remove the coordinate columns
#'   after converting a data frame to simple feature object; defaults to
#'   `FALSE`.
#' @param keep_all If `FALSE`, drop all columns other than those named in
#'   coords, Default: `TRUE`.
#' @param into If coords is a single column name with both longitude and
#'   latitude, `into` is used as the names of the new columns that coords is
#'   separated into. Passed to [tidyr::separate].
#' @param sep If coords is a single column name with both longitude and
#'   latitude, `sep` is used as the separator between coordinate values. Passed
#'   to [tidyr::separate].
#' @return [sf_to_df()] returns a data frame with geometry dropped or converted
#'   to wkt or coordinates for the centroid or point on surface; [df_to_sf()]
#'   returns a simple feature object with POINT geometry.
#' @seealso [sf::st_coordinates()]
#' @example examples/sf_to_df.R
#' @rdname sf_to_df
#' @export
sf_to_df <- function(x,
                     crs = 4326,
                     coords = c("lon", "lat"),
                     geometry = "centroid",
                     keep_all = TRUE) {
  x <-
    st_transform_ext(x, crs = crs)

  get_coords(
    x,
    geometry = geometry,
    coords = coords,
    keep_all = keep_all,
    drop = TRUE
  )
}

#' @rdname sf_to_df
#' @name df_to_sf
#' @param from_crs For [df_to_sf], coordinate reference system used by
#'   coordinates or well known text in data frame.
#' @param geo If `TRUE`, use [address_to_sf] to geocode address column; defaults
#'   to `FALSE`.
#' @param address Address column name passed to [tidygeocoder::geocode] or [tidygeocoder::geo]
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
                     address = "address") {
  if (is_sf(x)) {
    cli::cli_warn("df_to_sf requires a data frame. Dropping geometry from the provided simple feature object.")
    x <- sf::st_drop_geometry(x)
  }

  type <-
    dplyr::case_when(
      rlang::has_name(x, "geometry") && !all(rlang::has_name(x, coords)) ~ "geometry_df",
      geo && rlang::has_name(x, address) && !all(rlang::has_name(x, coords)) ~ "address_df",
      rlang::has_name(x, "wkt") ~ "wkt_df",
      TRUE ~ "coords_df"
    )

  x <-
    switch(type,
      "geometry_df" = geometry_df_to_sf(x),
      "address_df" = address_to_sf(x, address = address, coords = coords, crs = crs, remove_coords = remove_coords),
      "wkt_df" = wkt_df_to_sf(x, crs = from_crs),
      "coords_df" = coords_df_to_sf(x, coords = coords, crs = from_crs, into = into, sep = sep, rev = rev, remove_coords = remove_coords),
    )

  st_transform_ext(x = x, crs = crs, class = "sf")
}

#' Convert a data frame with a geometry list column to an sf object
#' @name coords_df_to_sf
#' @noRd
#' @importFrom rlang has_length
#' @importFrom sf st_as_sf
coords_df_to_sf <- function(x, coords = c("lon", "lat"), into = NULL, sep = ",", rev = FALSE, remove_coords = FALSE, crs = 4326) {
  if (rlang::has_length(coords, 1) && rlang::has_length(into, 2)) {
    x <- separate_coords(x = x, coords = coords, into = into, sep = sep)
    coords <- into
  } else {
    coords <- check_coords(x = x, coords = coords, rev = rev)
  }

  x <- format_coords(x, coords = coords)

  sf::st_as_sf(
    x,
    coords = c(coords[[1]], coords[[2]]),
    agr = "constant",
    crs = crs,
    stringsAsFactors = FALSE,
    remove = remove_coords
  )
}


#' @rdname sf_to_df
#' @name check_coords
#' @param default c("lon", "lat").
#' @param rev If `TRUE`, reverse c("lat", "lon") coords to c("lon", "lat").
#'   [check_coords] only.
#' @export
#' @importFrom janitor make_clean_names
check_coords <- function(x = NULL, coords = NULL, default = c("lon", "lat"), rev = FALSE) {

  # If x is a data frame
  if (!is.null(x) && is.data.frame(x)) {
    if (!has_coords(x, coords = coords, value = FALSE)) {
      if (!is.null(coords)) {
        cli::cli_warn(
          "The provided coordinates do not appear to match the data and no standard coordinate column names could be found.
        Replacing coordinates with default values."
        )

        coords <- default
      }
    } else {
      replace_coords <- has_coords(x, coords = coords, value = TRUE)

      if (!setequal(coords, replace_coords)) {
        coords <- replace_coords
      }
    }
  }

  # If X is NUll or not a dataframe check_coords just validates coord pairs or sets a default value
  if (is.null(coords)) {
    coords <- default
  }

  stopifnot(
    length(coords) == 2,
    is.character(coords) || is.numeric(coords)
  )

  # FIXME: This automatic reversal needs to be documented
  if (rev && grepl("LAT|lat|Y|y", coords[1])) {
    coords <- rev(coords)
  }

  coords
}

#' @rdname sf_to_df
#' @name has_coords
#' @param value If TRUE, return the value of the coordinate column names. Used by [has_coords].
#' @export
#' @importFrom janitor clean_names
#' @importFrom dplyr case_when
#' @importFrom rlang has_name
has_coords <- function(x, coords = NULL, value = TRUE) {
  stopifnot(
    !is.null(x) && is.data.frame(x)
  )

  x_nm <- names(x)
  x <- janitor::clean_names(x)

  x_coords <- NULL

  x_coords <-
    dplyr::case_when(
      all(rlang::has_name(x, coords)) ~ coords,
      rlang::has_name(x, "lon") ~ c("lon", "lat"),
      rlang::has_name(x, "long") ~ c("long", "lat"),
      rlang::has_name(x, "longitude") ~ c("longitude", "latitude"),
      rlang::has_name(x, "y") ~ c("y", "x"),
      rlang::has_name(x, "geo_longitude") ~ c("geo_longitude", "geo_latitude")
    )

  x_has_coords <-
    grep(
      paste0(paste0("^", x_coords, "$"), collapse = "|"),
      x_nm,
      ignore.case = TRUE,
      value = value
    )

  if (value) {
    return(x_has_coords)
  }

  return(length(x_has_coords) == length(x_coords))
}

#' Separate coordinates from a single combined column into two columns
#' @noRd
#' @importFrom tidyr separate
#' @importFrom tidyselect all_of
#' @importFrom dplyr mutate across
#' @importFrom readr parse_number
separate_coords <- function(x, coords, into, sep) {
  into <- check_coords(x = NULL, coords = into)

  x <-
    tidyr::separate(
      x,
      col = tidyselect::all_of(coords),
      into = into,
      sep = sep
    )

  x <-
    dplyr::mutate(
      x,
      dplyr::across(
        .cols = tidyselect::all_of(into),
        ~ readr::parse_number(.x)
      )
    )

  x
}

#' Convert a data frame with a geometry list column to an sf object
#' @noRd
#' @importFrom sf st_as_sf
geometry_df_to_sf <- function(x) {
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

#' Format coordinates as numeric values and remove missing coordinates from data frame
#'
#' @noRd
#' @importFrom cli cli_alert_info
format_coords <- function(x, coords = c("lon", "lat")) {
  lon <- coords[[1]]
  lat <- coords[[2]]

  stopifnot(
    is.data.frame(x)
  )

  # Check that lat/lon are numeric
  if (any(!is.numeric(x[[lon]])) | any(!is.numeric(x[[lat]]))) {
    x[[lon]] <- as.numeric(x[[lon]])
    x[[lat]] <- as.numeric(x[[lat]])
  }

  # Check for missing coordinates
  na_coords <- is.na(x[[lon]] | x[[lat]])
  num_na_coords <- sum(na_coords)

  if (num_na_coords > 0) {
    cli::cli_alert_info("Removing {num_na_coords} rows with missing coordinates.")
    # Exclude rows with missing coordinates
    x <- x[!na_coords, ]
  }

  x
}

#' Use tidygeocoder to convert an address or data frame with an address column
#' to an sf object
#'
#' Wraps [tidygeocoder::geo] and [tidygeocoder::geocode] to convert a character
#' string or a data frame with an address column.
#'
#' @param x Data frame with an address column. Multiple address columns are not currently supported.
#' @param address Address column name, Default: 'address'
#' @inheritParams df_to_sf
#' @param ... Additional parameters passed to [tidygeocoder::geo] or [tidygeocoder::geocode]
#' @return A `sf` object with POINT geometry for all geocoded addresses with valid coordinates.
#' @seealso
#'  \code{\link[tidygeocoder]{geo}}, \code{\link[tidygeocoder]{geocode}}
#' @rdname address_to_sf
#' @export
#' @importFrom rlang is_interactive
address_to_sf <- function(x, address = "address", coords = c("lon", "lat"), remove_coords = FALSE, crs = NULL, ...) {
  is_pkg_installed("tidygeocoder")

  if (is.character(x)) {
    # Make vector into address column
    x <- tibble::as_tibble_col(x, address)
  }

  stopifnot(is.data.frame(x))

  x <- has_same_name_col(x, col = "lon")
  x <- has_same_name_col(x, col = "lat")

  # Geocode the address column
  x <-
    tidygeocoder::geocode(
      x,
      address = address,
      long = "lon",
      lat = "lat",
      quiet = rlang::is_interactive(),
      ...
    )

  if (nrow(x) > 0) {
    x <-
      dplyr::rename(
        x,
        "{coords[[1]]}" := "lon",
        "{coords[[2]]}" := "lat"
      )

    # Convert address df to sf
    x <-
      df_to_sf(
        x,
        coords = coords,
        from_crs = 4326,
        remove_coords = remove_coords,
        crs = crs
      )
  } else {
    # FIXME: Add warning or error here if geocoding does not work
  }

  x
}
