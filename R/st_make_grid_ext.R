#' Make a grid over a simple feature bounding box
#'
#' Create a grid with an id column and optionally a set number of columns and
#' rows. This documentation is incomplete the function may change.
#'
#' @param x A `sf`, `sfc`, or `bbox` object, Default: `NULL`. Required.
#' @inheritParams st_bbox_ext
#' @inheritDotParams st_bbox_ext -class -null.ok
#' @param ncol,nrow Used to set n if either are not `NULL`; defaults to `NULL`.
#'   row and id are added as columns to the grid if they are provided.
#' @param gutter Distance in units between each column cell; gutter effectively
#'   serves as a margin as the negative buffer is applied to all cells
#'   (including those at the edges of the grid).
#' @param desc If TRUE, reverse standard order of cell id numbering; defaults
#'   `FALSE`
#' @param n If n is NULL and square is `TRUE`, the grid is set automatically to
#'   be 10 cells wide, Default: `NULL`
#' @param what "polygons", "corners", "centers"; set to centers automatically if
#'   style is "circle", "circle_offset" but a buffer is applied to return
#'   circular polygons.
#' @param style Style of cell to return with options including "rect", "square",
#'   "hex", "flat_top_hex", "circle", "circle_offset"
#' @param .id A name to use for the cell id column. Defaults to "id".
#' @inheritParams sf::st_make_grid
#' @param filter If `TRUE` (or if trim is `TRUE`) filter grid geometry by x
#'   using [st_filter_ext]
#' @inheritParams st_filter_ext
#' @example examples/st_make_grid_ext.R
#' @seealso [sf::st_make_grid]
#' @rdname st_make_grid_ext
#' @export
#' @importFrom sf st_make_grid st_filter
#' @importFrom dplyr mutate arrange row_number everything
st_make_grid_ext <- function(x,
                             ...,
                             unit = NULL,
                             crs = NULL,
                             ncol = NULL,
                             nrow = NULL,
                             n = NULL,
                             gutter = 0,
                             desc = FALSE,
                             cellsize = NULL,
                             what = NULL,
                             style = "rect",
                             .id = "id",
                             filter = FALSE,
                             trim = FALSE) {
  check_sf(x, ext = TRUE)

  if (!is_sf(x)) {
    x <- as_sf(x)
  }

  style <- arg_match(style, c("rect", "square", "hex", "flat_top_hex", "circle", "circle_offset"))

  lonlat_crs <- NULL

  if ((style %in% c("circle", "circle_offset")) && sf::st_is_longlat(x)) {
    lonlat_crs <- sf::st_crs(x)
    x <- st_transform_ext(x, 3857)
  }

  # Get adjusted bounding box using any adjustment variables provided
  bbox <-
    st_bbox_ext(
      x = x,
      ...,
      unit = unit
    )

  params <-
    get_grid_params(
      bbox = bbox,
      cellsize = cellsize,
      unit = unit,
      n = n,
      what = what,
      ncol = ncol,
      nrow = nrow,
      style = style
    )

  grid <-
    sf::st_make_grid(
      x = as_sf(bbox),
      cellsize = params$cellsize,
      n = params$n,
      what = params$what,
      square = params$square,
      flat_topped = params$flat_topped
    )

  grid <- as_sf(grid, crs = x)
  # grid <- st_filter_ext(x = grid, y = as_sfc(bbox))

  if (style %in% c("rect", "square", "circle")) {
    grid <-
      dplyr::mutate(
        grid,
        col = rep(sort(seq(params$ncol), decreasing = desc), params$nrow),
        row = sort(rep(seq(params$nrow), params$ncol), decreasing = !desc)
      )

    grid <- dplyr::arrange(grid, row, col)
  }

  grid <-
    dplyr::mutate(
      grid,
      "{.id}" := dplyr::row_number(),
      .before = dplyr::everything()
    )

  grid <- relocate_sf_col(grid)

  if (style %in% c("circle", "circle_offset")) {
    grid <-
      st_buffer_ext(
        grid,
        dist = (sf_bbox_xdist(bbox) / params$ncol) / 2
      )
  }

  if (!is.null(gutter) && (gutter != 0)) {
    grid <-
      st_buffer_ext(
        x = grid,
        dist = abs(gutter) * -0.5,
        unit = unit
      )
  }

  if (filter | trim) {
    grid <- st_filter_ext(grid, x, crop = FALSE, trim = trim)
  }

  if (!is.null(lonlat_crs)) {
    grid <- st_transform_ext(grid, crs = lonlat_crs)
  }

  transform_sf(grid, crs = crs)
}

#' Get parameters for make_location_grid
#'
#' @param base default value used for n if ncol, nrow, and cellsize are all
#'   `NULL`.
#' @noRd
#' @importFrom rlang has_length
#' @importFrom cli cli_alert_danger cli_alert_info
#' @importFrom dplyr case_when
get_grid_params <- function(bbox,
                            cellsize = NULL,
                            unit = NULL,
                            n = NULL,
                            what = NULL,
                            ncol = NULL,
                            nrow = NULL,
                            base = 10,
                            style = NULL) {
  what <- match.arg(what, c("polygons", "corners", "centers"))
  style <- match.arg(style, c("rect", "square", "hex", "flat_top_hex", "circle", "circle_offset"))

  if (!is.null(cellsize)) {
    if (rlang::has_length(n, 1)) {
      cellsize <- rep(cellsize, 2)
    }

    if (!is.null(ncol) && is_longer(ncol * cellsize[1], sf_bbox_xdist(bbox))) {
      cli::cli_alert_danger("The cellsize will not fit within the width of the bounding box with the number of columns requested.")
    }

    if (!is.null(nrow) && is_longer(nrow * cellsize[2], sf_bbox_ydist(bbox))) {
      cli::cli_alert_danger("The specified cellsize will not fit within the height of the bounding box with the number of nrow requested.")
    }
  }

  bbox_asp <- sf_bbox_asp(bbox)

  if (is.null(n) && is.null(ncol) && is.null(nrow)) {
    ncol <- base
    nrow <- base
  }

  if (is.null(n) && !is.null(cellsize)) {
    diff_bbox <- as.numeric(c(diff(bbox[c(1, 3)]), diff(bbox[c(2, 4)])))
    n <- diff_bbox / cellsize
  }

  if (is.null(n) && is.null(cellsize)) {
    n <-
      dplyr::case_when(
        (!is.null(ncol) && (style == "square")) ~ c(ncol, ncol / bbox_asp),
        (!is.null(ncol) && is.null(nrow)) ~ c(ncol, ncol),
        (!is.null(ncol) && !is.null(nrow)) ~ c(ncol, nrow)
      )

    n <-
      dplyr::case_when(
        (is.null(ncol) && !is.null(nrow) && (style == "square")) ~ c(nrow * bbox_asp, nrow),
        (is.null(ncol) && !is.null(nrow)) ~ c(nrow, nrow),
        TRUE ~ n
      )

    # FIXME: Check this and see what the issue actually is and if a warning is needed
    cli_warn_ifnot(
      "The row and columns values for the output simple feature grid are inconsistent when style is {style}.",
      condition = !(style %in% c("hex", "flat_top_hex"))
    )
  } else if (!is.null(n)) {
    n <-
      dplyr::case_when(
        rlang::has_length(n, 1) && (style == "square") ~ c(n, n / sf_bbox_asp(bbox)),
        rlang::has_length(n, 1) ~ c(n, n)
      )
  }

  if (is.null(cellsize)) {
    diff_bbox <- as.numeric(c(diff(bbox[c(1, 3)]), diff(bbox[c(2, 4)])))
    cellsize <- diff_bbox / unique(n)
  }

  if (style %in% c("rect", "square", "circle")) {
    square <- TRUE
  } else if (style %in% c("hex", "flat_top_hex", "circle_offset")) {
    square <- FALSE
  }

  flat_topped <- FALSE

  if (style == "flat_top_hex") {
    flat_topped <- TRUE
  }

  if (style %in% c("circle", "circle_offset")) {
    what <- "centers"
  }

  list(
    cellsize = cellsize,
    n = n,
    ncol = n[1], nrow = n[2],
    square = square,
    what = what,
    flat_topped = flat_topped
  )
}
