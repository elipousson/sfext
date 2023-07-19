#' Use mapview to interactively explore spatial data
#'
#' A wrapper for [mapview::mapview()] that drops list columns and makes it
#' easier to quickly specify a zcol value.
#'
#' @inheritParams mapview::mapview
#' @inheritDotParams mapview::mapview
#' @param remove_na If TRUE and zcol is not `NULL`, filter `NA` values from the
#'   zcol before passing to [mapview::mapview()]
#' @inheritParams make_img_leafpop
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
#' @export
#' @inheritParams read_sf_exif
mapview_exif <- function(path = NULL,
                         fileext = "jpeg",
                         popup = TRUE,
                         images = NULL,
                         width = 320,
                         ...) {
  images <- images %||% read_sf_exif(
      path = path,
      fileext = fileext,
      ...
    )

  if (!is_sf(images)) {
    images <-as_sf(images)
  }

  make_img_leafpop(
    images = images,
    width = width,
    popup = popup
  )
}

#' @param popup If `TRUE`, add a popup image to a leaflet map; defaults `TRUE`.
#' @rdname mapview_ext
#' @param images A simple feature object with columns for the image path/url,
#'   image width, and image height.
#' @name make_img_leafpop
#' @export
make_img_leafpop <- function(images,
                             popup = TRUE,
                             width = 320) {
  check_installed("leaflet")
  check_installed("leafpop")

  stopifnot(
    all(has_name(images, c("img_width", "img_height"))),
    any(has_name(images, c("path", "img_url"))),
    is_sf(images)
  )

  leaflet_map <-
    leaflet::addCircleMarkers(
      leaflet::addTiles(leaflet::leaflet()),
      data = images,
      group = "images"
    )

  if (has_name(images, "img_url")) {
    image <- images$img_url
  } else if (has_name(images, "path")) {
    image <- images$path
  }

  asp <- images$img_width / images$img_height
  height <- width / asp

  if (!popup) {
    return(leaflet_map)
  }

  leafpop::addPopupImages(
    map = leaflet_map,
    image = image,
    width = width,
    height = height,
    group = "images"
  )
}
