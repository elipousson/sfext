#' Transform or convert coordinates of a simple feature or bounding box object
#'
#' This function wraps [sf::st_transform()] but supports a wider range of input
#' objects and, using the [as_sf_class()] function, returns a wider range of
#' objects. Typically, takes a `sf`, `sfc`, or `bbox` object and transform to
#' coordinate reference system to match the value of crs or the object provided
#' to crs. If `x` is a data.frame or if `x` is `NULL` and `null.ok` is `TRUE`
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
#' @param null.ok If `TRUE` and x is `NULL` return x without an error. Defaults
#'   to `FALSE`.
#' @param list.ok If `TRUE`, x can be a list of `sf`, `sfc`, or `bbox` objects.
#'   If `FALSE`, only `sf`, `sfc`, or `bbox` objects are supported. Defaults to
#'   `TRUE`.
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
  if (any(c(is.data.frame(x) && !is_sf(x), is.null(x) && null.ok))) {
    return(x)
  }

  check_sf(x, ext = TRUE, null.ok = null.ok, list.ok = list.ok)

  type <-
    dplyr::case_when(
      is.null(crs) ~ "as_class",
      is_sf_list(x, ext = TRUE) ~ "list",
      rotate != 0 ~ "omerc",
      is_bbox(x) ~ "bbox",
      is_sf(x, ext = TRUE) ~ "sf"
    )

  x <-
    switch(type,
      "list" = map(x, ~ st_transform_ext(.x, crs, class, rotate)),
      "bbox" = sf_bbox_transform(x, crs = crs),
      "omerc" = st_omerc(x, rotate = rotate),
      "sf" = transform_sf(x, crs = crs),
      x
    )

  as_sf_class(x, class = class)
}

#' @name st_transform_omerc
#' @rdname st_transform_ext
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
