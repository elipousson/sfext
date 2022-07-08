#' Transform or convert coordinates of a simple feature or bounding box object
#'
#' Take a `sf`, `sfc`, or `bbox` object and transform to coordinate reference
#' system to match the object provided to `crs`.
#'
#' @param x An `sf`, `sfc`, or `bbox` object, a list of sf objects.
#' @param crs A character or numeric reference to a coordinate reference system
#'   supported by [sf::st_crs()] or another  `sf`, `sfc`, or `bbox` object that
#'   is used to provide crs.
#' @param class Class of object to return (`sf` or `bbox`). If x is an sf list,
#'   the returned object remains a list but may be converted to `bbox` if class
#'   = "sf".
#' @param rotate If rotate is greater or less than 0, [st_transform_ext] calls
#'   [st_omerc] and returns an object with the oblique mercator projection
#'   passing the value of rotate to the gamma parameter of the projection.
#'   rotate must be between -45 and 45 degrees.
#' @return An `sf`, `sfc`, or `bbox` object transformed to a new coordinate
#'   reference system.
#' @seealso [sf::st_transform()],[sf::st_crs()]
#' @rdname st_transform_ext
#' @export
#' @importFrom sf st_transform st_crs
st_transform_ext <- function(x,
                             crs = NULL,
                             class = NULL,
                             rotate = 0,
                             null.ok = FALSE,
                             list.ok = TRUE) {
  check_sf(x, ext = TRUE, null.ok = null.ok, list.ok = list.ok)

  type <-
    dplyr::case_when(
      is.null(x) ~ "null_x",
      is.null(crs) && is.null(class) ~ "null_crs",
      is.null(crs) && !is.null(class) ~ "as_class",
      is_sf_list(x, ext = TRUE) ~ "list",
      rotate != 0 ~ "omerc",
      is_same_crs(x, crs) ~ "same_crs",
      is_bbox(x) ~ "bbox",
      is_sf(x, ext = TRUE) ~ "sf"
    )

  if (type %in% c("null_x", "null_crs")) {
    return(x)
  }

  x <-
    switch(type,
      "as_class" = x,
      "same_crs" = x,
      "list" = purrr::map(x, ~ st_transform_ext(.x, crs, class, rotate)),
      "bbox" = sf_bbox_transform(x, crs = crs),
      "omerc" = st_omerc(x, rotate = rotate),
      "sf" = sf_transform(x, crs = crs)
    )

  as_sf_class(x, class = class)
}

#' Helper function for sf and sfc objects
#'
#' @noRd
sf_transform <- function(x, crs = NULL, null.ok = TRUE, ...) {
  if ((is.null(crs) && null.ok) | is_same_crs(x, crs)) {
    return(x)
  }

  if (is.na(sf::st_crs(x))) {
    sf::st_crs(x) <- crs
    return(x)
  }

  if (is_sf(crs, ext = TRUE)) {
    crs <- sf::st_crs(x = crs)
  }

  sf::st_transform(x, crs, ...)
}

#' @name st_transform_omerc
#' @rdname st_transform_ext
#' @export
#' @importFrom dplyr between
#' @importFrom sf st_transform
st_omerc <- function(x, rotate = 0) {
  cli_abort_ifnot(
    condition = is_sf(x) | is_sfc(x)
  )

  cli_warn_ifnot(
    "{.fn st_omerc} may return an error when {.arg rotate} is greater than or less than 45 degrees.",
    condition = dplyr::between(rotate, -45, 45)
  )

  coords <-
    get_coords(sf::st_union(x), keep_all = FALSE, crs = 4326)

  crs <-
    glue(
      "+proj=omerc +lat_0={coords$lat} +lonc={coords$lon} +datum=WGS84 +units=m +no_defs +gamma={rotate}"
    )

  sf::st_transform(x = x, crs = crs)
}
