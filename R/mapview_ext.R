#' Use mapview to interactively explore spatial data
#'
#' A wrapper for [mapview::mapview()] that drops list columns and makes it
#' easier to quickly specify a zcol value.
#'
#' @inheritParams mapview::mapview
#' @inheritDotParams mapview::mapview
#' @param remove_na If `TRUE` and `zcol` is not `NULL`, filter `NA` values from
#'   the data frame column defined by `zcol` before passing to
#'   [mapview::mapview()]
#' @inheritParams mapview_popup_img
#' @seealso
#'  [mapview::mapview()]
#' @export
#' @importFrom dplyr filter
mapview_ext <- function(x, zcol = NULL, remove_na = FALSE, ...) {
  check_installed("mapview")

  if (!inherits(x, "sf")) {
    return(mapview::mapview(x, ...))
  }

  nm <- names(x)[unlist(lapply(x, is.list))]

  if (length(nm) > 1) {
    nm <- nm[nm != attr(x, "sf_column")]
    cli::cli_alert(
      "Dropping list columns: {.val {nm}}"
    )
    x <- x[, !(names(x) %in% nm)]
  }

  if (remove_na && !is_null(zcol)) {
    x <- x[!is.na(x[[zcol]]), ]
  }

  mapview::mapview(
    x = x,
    zcol = zcol,
    ...
  )
}

#' @rdname mapview_ext
#' @name mapview_exif
#' @inheritParams read_sf_exif
#' @param fileext File extension. Defaults to "jpeg".
#' @export
mapview_exif <- function(path = NULL,
                         fileext = "jpeg",
                         popup = TRUE,
                         tooltip = FALSE,
                         images = NULL,
                         width = 320,
                         ...) {
  images <- images %||% read_sf_exif(
    path = path,
    fileext = fileext,
    ...
  )

  mapview_popup_img(
    images = images,
    width = width,
    tooltip = tooltip,
    popup = popup
  )
}

#' @param popup If `TRUE`, add a popup image to a leaflet map; defaults `TRUE`.
#' @param images A simple feature object with columns for the image path/url,
#'   image width, and image height.
#' @name mapview_popup_img
#' @rdname mapview_ext
#' @inheritParams leafpop::addPopupImages
#' @inheritParams rlang::args_error_context
#' @export
mapview_popup_img <- function(images,
                              popup = TRUE,
                              tooltip = FALSE,
                              map = NULL,
                              width = 320,
                              ...,
                              call = caller_env()) {
  check_installed("leaflet", call = call)
  check_installed("leafpop", call = call)

  if (!is_sf(images)) {
    images <- as_sf(images, call = call)
  }

  map <- map %||%
    leaflet::addCircleMarkers(
      leaflet::addTiles(leaflet::leaflet()),
      data = images,
      group = "images"
    )

  if (!popup) {
    return(map)
  }

  stopifnot(
    all(has_name(images, c("img_width", "img_height"))),
    any(has_name(images, c("path", "img_url")))
  )

  if (has_name(images, "img_url")) {
    image <- images$img_url
  } else if (has_name(images, "path")) {
    image <- images$path
  }

  asp <- images$img_width / images$img_height
  height <- width / asp

  leafpop::addPopupImages(
    map = map,
    image = image,
    width = width,
    height = height,
    group = "images"
  )
}
