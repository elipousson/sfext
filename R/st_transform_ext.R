#' Transform or convert coordinates of a simple feature or bounding box object
#'
#' This function wraps [sf::st_transform()] but supports a wider range of input
#' objects and, using the [as_sf_class()] function, returns a wider range of
#' objects. Typically, takes a `sf`, `sfc`, or `bbox` object and transform to
#' coordinate reference system to match the value of crs or the object provided
#' to crs. If `x` is a data.frame or if `x` is `NULL` and `allow_null` is `TRUE`
#' (defaults to `FALSE`) it is returned as is.
#'
#' @param x An `sf`, `sfc`, or `bbox` object, a list of sf objects, or a
#'   `data.frame` object (always returned as is).
#' @param crs A character or numeric reference to a coordinate reference system
#'   supported by [sf::st_crs()] or another  `sf`, `sfc`, or `bbox` object that
#'   is used to provide crs.
#' @param class Class of object to return (`sf` or `bbox`). If x is an sf list,
#'   the returned object remains a list but may be converted to `bbox` if `class
#'   = "sf"`.
#' @param rotate If rotate is greater or less than 0, [st_transform_ext()] calls
#'   [st_omerc()] and returns an object with the Oblique Mercator projection
#'   passing the value of rotate to the gamma parameter of the projection.
#'   rotate must be between -45 and 45 degrees.
#' @param allow_null If `TRUE` and x is `NULL` return x without an error.
#'   Defaults to `FALSE`.
#' @param allow_list If `TRUE`, x can be a list of `sf`, `sfc`, or `bbox` objects.
#'   If `FALSE`, only `sf`, `sfc`, or `bbox` objects are supported. Defaults to
#'   `TRUE`.
#' @return An `sf`, `sfc`, or `bbox` object transformed to a new coordinate
#'   reference system.
#' @example examples/st_transform_ext.R
#' @seealso [sf::st_transform()],[sf::st_crs()]
#' @rdname st_transform_ext
#' @export
#' @importFrom dplyr case_when
st_transform_ext <- function(x,
                             crs = NULL,
                             class = NULL,
                             rotate = 0,
                             allow_null = FALSE,
                             allow_list = TRUE) {
  if (is.data.frame(x) && !is_sf(x)) {
    if (!is_null(crs)) {
      cli_warn(
        "{.arg x} can't be transformed to {.arg crs} because
      {.arg x} is a {.cls data.frame}."
      )
    }

    return(x)
  }

  check_sf(x, ext = TRUE, allow_null = allow_null, allow_list = allow_list)

  if (is_null(x)) {
    return(x)
  }

  type <-
    dplyr::case_when(
      is_null(crs) ~ "as_class",
      is_sf_list(x, ext = TRUE) ~ "list",
      rotate != 0 ~ "omerc",
      is_bbox(x) ~ "bbox",
      is_sf(x, ext = TRUE) ~ "sf"
    )

  x <-
    switch(type,
      "list" = map(x, ~ st_transform_ext(.x, crs, class, rotate, allow_null)),
      "bbox" = sf_bbox_transform(x, crs = crs),
      "omerc" = st_omerc(x, rotate = rotate),
      "sf" = transform_sf(x, crs = crs),
      x
    )

  as_sf_class(x, class = class)
}

#' @name st_omerc
#' @rdname st_transform_ext
#' @aliases st_transform_omerc
#' @export
#' @importFrom dplyr between
#' @importFrom sf st_union st_transform
st_omerc <- function(x, rotate = 0) {
  check_sf(x, ext = "sfc")

  cli_warn_ifnot(
    "{.fn st_omerc} may return an error when {.arg rotate} is
    greater than or less than 45 degrees.",
    condition = dplyr::between(rotate, -45, 45)
  )

  coords <-
    get_coords(sf::st_union(x), keep_all = FALSE, crs = 4326)

  crs <-
    glue(
      "+proj=omerc +lat_0={coords$lat} +lonc={coords$lon}
      +datum=WGS84 +units=m +no_defs +gamma={rotate}"
    )

  sf::st_transform(x = x, crs = crs)
}

#' @name st_wgs84
#' @rdname st_transform_ext
#' @aliases as_wgs84
#' @export
st_wgs84 <- function(x) {
  if (is_wgs84(x)) {
    return(x)
  }

  st_transform_ext(x, 4326)
}
