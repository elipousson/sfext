
#' Get standard paper and image sizes
#'
#' Use the "paper" parameter (matching name from [paper_sizes]), standard
#' (optionally including series and size) parameter, or width, height and units.
#' May return multiple paper sizes depending on parameters.
#'
#' If margin is provided, a block_width, block_height, and block_asp are
#' calculated and included as columns in the returned data frame.
#'
#' Paper can also be a data frame with "width", "height", "orientation", and
#' "units" columns.
#'
#' @param paper Paper, Default: 'letter'.
#' @param orientation Orientation "portrait", "landscape", or "square", Default:
#'   'portrait'.
#' @param standard Size standard, "ANSI", "ISO", "British Imperial", "JIS",
#'   "USPS", "Facebook", "Instagram", or "Twitter".
#' @param series Size series (e.g. A), Default: `NULL`
#' @param size Size number (only used for "ISO" and "JIS" series). Standard,
#'   series, and size may all be required to return a single paper when using
#'   these parameters.
#' @param width,height Width and height in units, Default: `NULL`.
#' @param units Paper size units, either "in", "mm", or "px"; defaults to `NULL`
#'   (using "in" if width or height are provided).
#' @param ncol,nrow Number of expected columns and rows in paper; used to
#'   determine row_height and section_asp in paper data frame returned by
#'   get_paper if nrow or ncol is greater than 1; defaults to `NULL`.
#' @param gutter Gutter distance in units. Gutter is used as the spacing between
#'   nrow and columns (variable spacing is not currently supported); defaults to 0.
#' @param margin A numeric vector or ggplot2 margin object.
#' @param bbox A bounding box to use to get orientation using [sf_bbox_asp()] with orientation = TRUE.
#' @param ... Additional parameters passed to get_margin. plot_width can only be
#'   passed in these parameters if paper has only a single row. margin is returned as a list column.
#' @return Data frame with one or more paper/image sizes.
#' @example examples/get_paper.R
#' @rdname get_paper
#' @export
#' @importFrom dplyr filter select mutate
#' @importFrom rlang .data
get_paper <- function(paper = "letter",
                      orientation = "portrait",
                      standard = NULL,
                      series = NULL,
                      size = NULL,
                      width = NULL,
                      height = NULL,
                      units = NULL,
                      ncol = 1,
                      nrow = 1,
                      gutter = 0,
                      bbox = NULL,
                      margin = NULL,
                      ...) {
  type <-
    dplyr::case_when(
      is.data.frame(paper) && is_df_paper(paper, ext = FALSE) ~ "paper",
      is.character(paper) ~ "name",
      !is.null(standard) ~ "standard",
      !is.null(width) | !is.null(height) | !is.null(units) ~ "dims"
    )

  paper <-
    switch(type,
      "paper" = paper,
      "name" = get_paper_name(paper),
      "standard" = get_paper_standard(standard = standard, series = series, size = size),
      "dims" = get_paper_dims(width = width, height = height, units = units)
    )

  paper <- set_paper_orientation(paper, orientation = orientation, bbox = bbox)

  ncol <- ncol %||% 1
  nrow <- nrow %||% 1
  gutter <- gutter %||% 0

  paper <-
    dplyr::mutate(
      paper,
      ncol = ncol,
      nrow = nrow,
      col_width = (width - gutter) / ncol,
      row_height = (height - gutter) / nrow,
      gutter = gutter,
      section_asp = col_width / row_height,
      .after = asp
    )

  if (is.null(margin)) {
    return(paper)
  }

  margin <- get_margin(margin = margin, ...)
  margin_num <- as.numeric(margin)
  margin_width <- margin_num[[2]] + margin_num[[4]]
  margin_height <- margin_num[[1]] + margin_num[[3]]

  dplyr::mutate(
    paper,
    block_width = width - margin_width,
    block_height = height - margin_height,
    block_asp = block_width / block_height,
    col_width = (block_width - gutter) / ncol,
    row_height = (block_height - gutter) / nrow,
    section_asp = col_width / row_height,
    margin = list(margin)
  )
}

#' Set paper orientation and swap width/height if needed
#'
#' @noRd
set_paper_orientation <- function(paper, orientation = NULL, bbox = NULL) {
  if (!is.null(bbox)) {
    orientation <- sf_bbox_asp(bbox = bbox, orientation = TRUE)
  } else {
    orientation <-
      match.arg(
        orientation,
        c("portrait", "landscape", "square"),
        several.ok = TRUE
      )
  }

  paper_orientation <- unique(paper$orientation)

  # Save width and height before checking orientation
  if (!is.null(orientation) && (length(paper_orientation) == 1)) {
    paper_width <- paper$width
    paper_height <- paper$height

    if ((paper_orientation == "portrait") && (orientation == "landscape")) {
      # width and height for most papers are assumed to be in a portrait format
      paper$width <- paper_height
      paper$height <- paper_width
      paper$orientation <- orientation
    } else if ((paper_orientation == "landscape") && (orientation == "portrait")) {
      # width and height for most papers are assumed to be in a portrait format
      paper$width <- paper_height
      paper$height <- paper_width
      paper$orientation <- orientation
    }
  }

  dplyr::mutate(
    paper,
    asp = width / height,
    .after = height
  )
}

#' Get paper using name
#'
#' @noRd
#' @importFrom dplyr filter
get_paper_name <- function(paper) {
  paper_sizes[tolower(paper_sizes[["name"]]) %in% tolower(paper), ]
}

#' Get paper using standard and optional series or size
#'
#' @noRd
#' @importFrom dplyr filter
get_paper_standard <- function(standard, series = NULL, size = NULL) {
  standard <- match.arg(standard, c("ANSI", "ISO", "British Imperial", "JIS", "USPS", "Facebook", "Instagram", "Twitter"), several.ok = TRUE)
  paper <- paper_sizes[paper_sizes[["standard"]] %in% standard, ]

  if (!is.null(series)) {
    series <- match.arg(series, c("A", "B", "C", "Engineering", "Architecture", "EDDM"), several.ok = TRUE)
    paper <- paper[paper[["series"]] %in% series, ]
  }

  if (!is.null(size)) {
    paper_series <- match.arg(series, c("A", "B", "C", "Engineering", "Architecture", "EDDM"), several.ok = TRUE)
    paper <- paper[paper[["size"]] %in% size, ]
  }

  return(paper)
}

#' Get paper using width, height, or both
#'
#' @noRd
#' @importFrom dplyr filter
get_paper_dims <- function(width = NULL, height = NULL, units = NULL) {
  units <- match.arg(tolower(units), c("in", "mm", "px"))
  paper <- paper_sizes[paper_sizes[["units"]] %in% units, ]

  if (!is.null(width)) {
    paper <- paper[paper[["width"]] %in% width, ]
  }

  if (!is.null(height)) {
    paper <- paper[paper[["height"]] %in% height, ]
  }

  paper
}

#' Get social media image size to match platform and format
#'
#' See `paper_sizes[paper_sizes$type == "social",]$name` for support image
#' options.
#'
#' @param image Image size name, Default: `NULL`
#' @param platform Social media platform, "Instagram", "Facebook", or "Twitter",
#'   Default: `NULL`
#' @param format Image format, "post", "story", or "cover", Default: `NULL`
#' @param orientation Image orientation, Default: `NULL`.
#' @rdname get_social_image
#' @export
get_social_image <- function(image = NULL, platform = NULL, format = NULL, orientation = NULL) {
  image_sizes <- paper_sizes[paper_sizes$type == "social", ]

  if (!is.null(platform)) {
    platform <- arg_match(platform, unique(image_sizes$standard))
    image_sizes <- image_sizes[image_sizes$standard %in% platform,]
  }

  if (!is.null(format)) {
    format <- arg_match(format, unique(image_sizes$size))
    image_sizes <- image_sizes[image_sizes$size %in% format,]
  }

  image <- image %||% image_sizes$name[1]
  image <- arg_match(image, image_sizes$name)

  get_paper(
    paper = image,
    orientation = orientation
  )
}
