#' Convert a data frame with one or more coordinate columns to an sf object
#'
#' @description
#' - [coords_to_sf()]: Convert a data frame with coordinates into a simple
#' feature object
#' - [check_coords()]: Check if a provided vector with coordinate column names
#' are valid for the provided data frame
#' - [separate_coords()]: Separate coordinates from a single combined column
#' into two columns
#' - [format_coords()]: Format coordinates as numeric values and remove missing
#' coordinates from data frame
#' - [has_coords()]: Suggests a coordinate pair by comparing common values to
#' the column names for a provided data frame
#' - [rev_coords()]: Reverse a vector with coordinate column names if latitude
#' or y appear first
#'
#' @rdname coords_to_sf
#' @param coords Coordinate columns for input data.frame or output sf object (if
#'   geometry is 'centroid' or 'point') Default: c("lon", "lat").
#' @param remove_coords For [df_to_sf()], if `TRUE`, remove the coordinate columns
#'   after converting a data frame to simple feature object; defaults to
#'   `FALSE`.
#' @param keep_all If `FALSE`, drop all columns other than those named in
#'   coords, Default: `TRUE`.
#' @param crs Coordinate reference system used by the coordinates in the
#'   provided data frame.
#' @export
#' @importFrom sf st_as_sf
#' @importFrom rlang caller_env has_length
coords_to_sf <- function(x,
                         coords = c("lon", "lat"),
                         into = NULL,
                         sep = ",",
                         rev = FALSE,
                         remove_coords = FALSE,
                         crs = 4326,
                         call = caller_env()) {
  if (has_length(coords, 1) && has_length(into, 2)) {
    x <- separate_coords(x = x, coords = coords, into = into, sep = sep)
    coords <- into
  } else {
    coords <- check_coords(x = x, coords = coords, rev = rev)
  }

  if (identical(has_coords(x, coords), character(0))) {
    cli_warn(
      c("{.arg x} can't be converted to a {.cls sf} object.",
        " " = "Returning a {.cls {class(x)}} object."
      ),
      call = call
    )
    return(x)
  }

  x <- format_coords(x, coords = coords, call = call)

  sf::st_as_sf(
    x,
    coords = c(coords[[1]], coords[[2]]),
    agr = "constant",
    crs = crs,
    stringsAsFactors = FALSE,
    remove = remove_coords
  )
}

#' @rdname coords_to_sf
#' @name check_coords
#' @param default Default coordinate values; defaults to `c("lon", "lat")`.
#' @param rev If `TRUE`, reverse `c("lat", "lon")` coords to `c("lon", "lat")`.
#'   [check_coords()] only.
#' @export
#' @importFrom janitor make_clean_names
check_coords <- function(x = NULL,
                         coords = NULL,
                         default = c("lon", "lat"),
                         rev = FALSE,
                         call = caller_env()) {
  # If x is a data frame
  if (!is.null(x) && is.data.frame(x)) {
    x_has_coords <-
      has_coords(x, coords = coords, value = FALSE)

    if (x_has_coords) {
      coords <- has_coords(x, coords = coords, value = TRUE)
    } else if (!is.null(coords) &&
      !identical(has_coords(x, default), character(0))) {
      cli_warn(
        c(
          "{.arg coords} ({.val {coords}}) can't be found in {.arg x}.",
          " " = "Replacing {.arg coords} with {.arg default} ({.val {default}})."
        ),
        call = call
      )

      coords <- NULL
    }
  }

  # If x is NULL or not a data.frame check_coords just validates coord pairs or
  # sets a default value
  coords <- coords %||% default

  cli_abort_ifnot(
    # FIXME: What about the coord_col value where coordinates are split in two?
    "{.arg coords} must be a length 2 {.cls character}
    or {.cls numeric} vector.",
    condition = length(coords) == 2 &&
      (is.character(coords) || is.numeric(coords)),
    call = call
  )

  if (rev) {
    coords <- rev_coords(coords)
  }

  coords
}

#' @rdname coords_to_sf
#' @name rev_coords
#' @param pattern Pattern passed to [grepl()] used to match vectors that are
#'   reversed.
#' @param ignore.case If `TRUE`, pattern matching is not case sensitive.
#' @export
rev_coords <- function(coords, pattern = c("lat", "^y"), ignore.case = TRUE) {
  if (grepl(
    pattern = paste0(pattern, collapse = "|"),
    x = coords[1],
    ignore.case = ignore.case
  )) {
    return(rev(coords))
  }

  coords
}

#' @rdname coords_to_sf
#' @name has_coords
#' @param value If `TRUE`, return the value of the coordinate column names. Used
#'   by [has_coords()].
#' @export
#' @importFrom janitor clean_names
#' @importFrom dplyr case_when
#' @importFrom rlang has_name
has_coords <- function(x, coords = NULL, value = TRUE) {
  stopifnot(
    !is.null(x) && is.data.frame(x)
  )

  x_names <- names(x)
  x <- janitor::clean_names(x)

  x_coords <- NULL

  x_coords <-
    dplyr::case_when(
      all(coords %in% x_names) ~ coords,
      all(has_name(x, coords)) ~ coords,
      has_name(x, "lon") ~ c("lon", "lat"),
      has_name(x, "long") ~ c("long", "lat"),
      has_name(x, "longitude") ~ c("longitude", "latitude"),
      has_name(x, "y") ~ c("y", "x"),
      has_name(x, "geo_longitude") ~ c("geo_longitude", "geo_latitude")
    )

  x_has_coords <-
    grep(
      paste0(paste0("^", x_coords, "$"), collapse = "|"),
      x_names,
      ignore.case = TRUE,
      value = value
    )

  if (value) {
    return(x_has_coords)
  }

  length(x_has_coords) == length(x_coords)
}


#' @rdname coords_to_sf
#' @name format_coords
#' @export
#' @importFrom cli cli_inform
format_coords <- function(x,
                          coords = c("lon", "lat"),
                          keep_missing = FALSE,
                          call = caller_env()) {
  cli_abort_ifnot(
    "{.arg coords} can't be {.val NULL} or {.val character(0)}.",
    condition = !is.null(coords) && !identical(coords, character(0)),
    call = call
  )

  cli_abort_ifnot(
    "{.arg x} must be a {.cls data.frame} with columns named {.val {coords}}.",
    condition = is.data.frame(x) && all(rlang::has_name(x, coords)),
    call = call
  )

  lon <- coords[[1]]
  lat <- coords[[2]]

  if (!all(is.numeric(x[[lon]])) || !all(is.numeric(x[[lat]]))) {
    x[[lon]] <- as.numeric(x[[lon]])
    x[[lat]] <- as.numeric(x[[lat]])
  }

  missing_coords <- (is.na(x[[lon]]) | is.na(x[[lat]]))
  n_missing_coords <- sum(missing_coords)
  has_missing_coords <- n_missing_coords > 0

  if (n_missing_coords == nrow(x)) {
    cli_abort(
      "{.arg x} must have one or more coordinate pairs in
      column{?s} {.val {coords}}.",
      call = call
    )
  }

  if (has_missing_coords && !keep_missing) {
    # Exclude rows with missing coordinates
    cli_inform(
      "Removing {.val {n_missing_coords}} row{?s} with
      missing coordinates from {.arg x}.",
      call = call
    )

    return(x[!missing_coords, ])
  }

  if (has_missing_coords) {
    cli_inform(
      "{.arg x} has {.val {n_missing_coords}} row{?s} with missing coordinates.",
      call = call
    )
  }

  x
}

#' @rdname coords_to_sf
#' @name separate_coords
#' @param into If coords is a single column name with both longitude and
#'   latitude, `into` is used as the names of the new columns that coords is
#'   separated into. Passed to [tidyr::separate()].
#' @param sep If coords is a single column name with both longitude and
#'   latitude, `sep` is used as the separator between coordinate values. Passed
#'   to [tidyr::separate()].
#' @export
#' @importFrom dplyr mutate across all_of
separate_coords <- function(x, coords, into = c("lon", "lat"), sep = ",") {
  into <- check_coords(x = NULL, coords = into)

  is_pkg_installed("readr")
  is_pkg_installed("tidyr")

  x <-
    tidyr::separate(
      x,
      col = dplyr::all_of(coords),
      into = into,
      sep = sep
    )

  dplyr::mutate(
    x,
    dplyr::across(
      .cols = dplyr::all_of(into),
      ~ readr::parse_number(.x)
    )
  )
}
