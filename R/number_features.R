#' Sort and number features by coordinates or distance
#'
#' @description
#' Used with [maplayer::layer_numbers()]. Supports multiple types of sorting including
#' sorting:
#'
#' - by centroid coordinates ("lon", "lat") appended with [get_coords()]
#' - by one or more bounding box min or max values ("xmin", "ymin", "xmax",
#' "ymax") appended with [get_minmax()]
#' - by distance from the corner, side midpoint, or center of a bounding box
#' ("dist_xmin_ymin", "dist_xmax_ymax", "dist_xmin_ymax", "dist_xmax_ymin",
#' "dist_xmin_ymid", "dist_xmax_ymid", "dist_xmid_ymin", "dist_xmid_ymax",
#' "dist_xmid_ymid")
#' - by distance to a point (or `sf`, `sfc`, or `bbox` object) passed to the to
#' parameter
#'
#' For example, in the eastern United States, you can sort and number features
#' from the top-left corner of the map to the bottom right by setting sort to
#' "dist_xmin_ymax" (default).
#'
#' [number_features] also supports a range of different numbering styles
#' designed to match the standard enumeration options available in LaTeX.
#'
#' @param x A `sf` or `sfc` object.
#' @param num_style Style of enumeration, either "arabic", "alph", "Alph",
#'   "roman", "Roman".
#' @param num_start Starting number; defaults to 1.
#' @param suffix Character to appended to "number" column. (e.g. "." for "1." or
#'   ":" for "1:"). Can also be a character vector with the same length as the
#'   number column.
#' @param .id Name of the column to use for the feature numbers; defaults to
#'   "number".
#' @return A `sf` object with a number column ordered by sort values.
#' @export
#' @importFrom dplyr mutate row_number everything
#' @importFrom utils as.roman
number_features <- function(x,
                            col = NULL,
                            sort = "dist_xmin_ymax",
                            to = NULL,
                            desc = FALSE,
                            crs = NULL,
                            num_style = "arabic",
                            num_start = 1,
                            suffix = NULL,
                            .id = "number") {
  check_sf(x, ext = TRUE)

  if (!is_sf(x)) {
    x <- as_sf(x)
  }

  x <- has_same_name_col(x, col = .id)

  if (!is.null(sort)) {
    x <-
      sort_features(
        x,
        col = col,
        sort = sort,
        to = to,
        desc = desc,
        crs = crs
      )
  }

  x <- as_numbered_labels(x, labels = num_style, cols = .id, start = num_start)

  if (tolower(num_style) == "arabic") {
    x[[.id]] <- as.integer(x[[.id]])
  }

  x <- dplyr::relocate(x, dplyr::all_of(.id), .before = dplyr::everything())

  x
}

#' @name number_sf
#' @rdname number_features
#' @export
number_sf <- number_features


#' @name sort_features
#' @rdname number_features
#' @param col Group column name, Default: `NULL`
#' @param sort Sort column name, Default: "dist_xmin_ymax".
#' @param to A `sf` object used to determine sort order based on distance from a
#'   feature to the center of the "to" object.
#' @param desc If `TRUE`, sort descending; default `FALSE`.
#' @param crs Coordinate reference to use with [get_coords()] or [get_minmax()]
#'   if "sort" any of the following: "lon", "lat", "longitude", "latitude",
#'   "xmin", "ymin", "xmax", "ymax"
#' @export
#' @importFrom dplyr arrange desc across all_of
sort_features <- function(x,
                          col = NULL,
                          sort = c("lon", "lat"),
                          to = NULL,
                          desc = FALSE,
                          crs = NULL) {
  latlon_opts <- c("longitude", "latitude", "lon", "lat")
  minmax_opts <- c("xmin", "ymin", "xmax", "ymax")

  if (any(sort %in% c(latlon_opts, minmax_opts))) {
    sort <- arg_match(sort, c(latlon_opts, minmax_opts), multiple = TRUE)

    if ((sort %in% latlon_opts) && !all(has_name(x, sort))) {
      x <-
        get_coords(
          x,
          geometry = "centroid",
          crs = crs,
          keep_all = TRUE,
          drop = FALSE
        )
    } else if ((sort %in% minmax_opts) && !all(has_name(x, sort))) {
      x <-
        get_minmax(
          x,
          crs = crs,
          keep_all = TRUE,
          drop = FALSE
        )
    }
  }

  dist_opts <-
    c(
      "dist_xmin_ymin", "dist_xmax_ymax",
      "dist_xmin_ymax", "dist_xmax_ymin",
      "dist_xmin_ymid", "dist_xmax_ymid",
      "dist_xmid_ymin", "dist_xmid_ymax",
      "dist_xmid_ymid"
    )

  if (any(sort %in% c(dist_opts)) || !is.null(to)) {
    if (is.null(to)) {
      # FIXME: Shouldn't this split the sort string first and then match to the options?
      sort <- arg_match(sort, dist_opts, multiple = FALSE)
      to <- strsplit(sort, "_")[[1]][2:3]
    } else if (any(sort %in% c(dist_opts))) {
      cli_warn(
        "If {.arg sort} and {.arg to} are both provided, the value of {.arg sort} ({.val {sort}}) is ignored."
      )
    }

    x <-
      get_dist(
        x,
        to = to,
        keep_all = TRUE,
        drop = FALSE
      )

    sort <- "dist"
  }

  cli_warn_ifnot(
    "{.arg sort} column ({.val {sort}}) can't be found in {.arg x}.",
    condition = has_name(x, sort)
  )

  by_group <- FALSE

  if (!is.null(col)) {
    x <- group_by_col(x, col = col)
    # FIXME: Is there a reason to allow grouping by column even if not numbering by group
    by_group <- TRUE
  }

  if (desc) {
    return(dplyr::arrange(x, dplyr::desc(dplyr::across(dplyr::all_of(sort))), .by_group = by_group))
  }

  dplyr::arrange(x, dplyr::across(dplyr::all_of(sort)), .by_group = by_group)
}

#' @name sort_sf
#' @rdname number_features
#' @export
sort_sf <- sort_features
