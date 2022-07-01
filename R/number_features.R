#' Sort and number features by coordinates or distance
#'
#' Used with [layer_numbers()]. Supports multiple types of sorting including
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
#' @param data Marker data
#' @param num_style Style of enumeration, either "arabic", "alph", "Alph",
#'   "roman", "Roman"
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
number_features <- function(data,
                            col = NULL,
                            sort = "dist_xmin_ymax",
                            to = NULL,
                            desc = FALSE,
                            crs = NULL,
                            num_style = "arabic",
                            num_start = 1,
                            suffix = NULL,
                            .id = "number") {
  data <- has_same_name_col(data, col = .id)

  if (!is.null(sort)) {
    data <-
      sort_features(
        data,
        col = col,
        sort = sort,
        to = to,
        desc = desc,
        crs = crs
      )
  }

  data <-
    dplyr::mutate(
      data,
      "{.id}" := dplyr::row_number() + (num_start - 1),
      .before = dplyr::everything()
    )

  num_style <- match.arg(num_style, c("arabic", "alph", "Alph", "roman", "Roman"))

  data$number <-
    switch(num_style,
      "arabic" = data$number,
      "alph" = tolower(sapply(data[[.id]], int_to_alph)),
      "Alph" = toupper(sapply(data[[.id]], int_to_alph)),
      "roman" = tolower(utils::as.roman(data[[.id]])),
      "Roman" = toupper(utils::as.roman(data[[.id]]))
    )

  if (!is.null(suffix)) {
    data[[.id]] <- paste0(data[[.id]], suffix)
  }

  data
}

#' Adapted from https://stackoverflow.com/a/44274075
#'
#' @noRd
int_to_alph <- function(num, suffix = NULL, base = 26) {
  rest <- (num - 1) %/% base

  suffix <-
    paste0(
      LETTERS[((num - 1) %% base) + 1],
      suffix
    )

  if (rest > 0) {
    return(Recall(rest, base, suffix))
  }

  suffix
}

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
#' @importFrom rlang has_name
#' @importFrom dplyr arrange desc across all_of
sort_features <- function(data,
                          col = NULL,
                          sort = c("lon", "lat"),
                          to = NULL,
                          desc = FALSE,
                          crs = NULL) {
  latlon_opts <- c("longitude", "latitude", "lon", "lat")
  minmax_opts <- c("xmin", "ymin", "xmax", "ymax")

  if (any(sort %in% c(latlon_opts, minmax_opts))) {
    sort <- match.arg(sort, choices = c(latlon_opts, minmax_opts), several.ok = TRUE)

    if ((sort %in% latlon_opts) && !all(rlang::has_name(data, sort))) {
      data <-
        get_coords(
          data,
          geometry = "centroid",
          crs = crs,
          keep_all = TRUE,
          drop = FALSE
        )
    } else if ((sort %in% minmax_opts) && !all(rlang::has_name(data, sort))) {
      data <-
        get_minmax(
          data,
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

  if (any(sort %in% c(dist_opts)) | !is.null(to)) {
    if (is.null(to)) {
      # FIXME: Shouldn't this split the sort string first and then match to the options?
      sort <- match.arg(sort, dist_opts, several.ok = FALSE)
      to <-
        strsplit(sort, "_")[[1]][2:3]
    } else if (any(sort %in% c(dist_opts))) {
      cli::cli_alert_danger(
        "If the {.field {'sort'}} and {.field {'to'}} are both provided,
        the value of {.field {'sort'}} ({.val {sort}}) is ignored."
      )
    }

    data <-
      get_dist(
        data,
        to = to,
        keep_all = TRUE,
        drop = FALSE
      )

    sort <- "dist"
  }


  if (!rlang::has_name(data, sort)) {
    cli::cli_warn("The provided value for {.field {'sort'}} ({.val {sort}}) is not found in the data.")
  }

  by_group <- FALSE

  if (!is.null(col)) {
    data <- group_by_col(data, col = col)
    # FIXME: Is there a reason to allow grouping by column even if not numbering by group
    by_group <- TRUE
  }

  if (desc) {
    return(dplyr::arrange(data, dplyr::desc(dplyr::across(dplyr::all_of(sort))), .by_group = by_group))
  }

  dplyr::arrange(data, dplyr::across(dplyr::all_of(sort)), .by_group = by_group)
}
