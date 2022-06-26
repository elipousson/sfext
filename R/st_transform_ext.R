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
st_transform_ext <- function(x = NULL,
                             crs = NULL,
                             class = NULL,
                             rotate = 0) {
  if (is_sf_list(x, ext = TRUE)) {
    x <-
      purrr::map(
        x,
        ~ st_transform_ext(
          x = .x,
          crs = crs,
          class = class
        )
      )

    return(x)
  }

  stopifnot(
    is_sf(x, ext = TRUE)
  )

  if (rotate != 0) {
    crs <- NULL
    x <- st_omerc(x, rotate = rotate)
  }

  # if x has a different crs than the sf object passed to crs
  if (is_sf(crs, ext = TRUE)) {
    crs <- sf::st_crs(x = crs)
  }

  if (is.null(crs) || is_same_crs(x = x, y = crs)) {
    return(as_sf_class(x, class = class))
  }

  if (is_bbox(x)) {
    # If x is a bbox
    x <- sf_bbox_transform(bbox = x, crs = crs)
  } else {
    # If x is an sf or sfc object
    x <- sf::st_transform(x = x, crs = crs)
  }

  as_sf_class(x, class = class)
}

#' @name st_transform_omerc
#' @rdname st_transform_ext
#' @export
#' @importFrom dplyr between
#' @importFrom cli cli_alert_warning
#' @importFrom glue glue
#' @importFrom sf st_transform
st_omerc <- function(x, rotate = 0) {
  if (!dplyr::between(rotate, -45, 45)) {
    cli::cli_alert_warning(
      "st_omerc may have an error with rotate values greater than or less than 45 degrees."
    )
  }

  coords <-
    get_coords(sf::st_union(x), keep_all = FALSE, crs = 4326)

  crs <-
    glue::glue(
      "+proj=omerc +lat_0={coords$lat} +lonc={coords$lon} +datum=WGS84 +units=m +no_defs +gamma={rotate}"
    )

  sf::st_transform(x = x, crs = crs)
}
