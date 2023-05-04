#' Shift sides, contract, or expand a bounding box
#'
#' @description
#' - Shift, expand, or contract a bounding box ([sf_bbox_shift()],
#' [sf_bbox_expand()], [sf_bbox_contract()])
#'
#' @name sf_bbox_shift
#' @param bbox A `bbox` object.
#' @param x_nudge,y_nudge Length 1 or 2 numeric vector; unitless. Used by or
#'   passed to [sf_bbox_shift()]. Required for  [sf_bbox_shift()].
#' @param side one or more sides to shift: "top", "bottom", "left", "right", or
#'   "all". Required for [sf_bbox_shift()].
#' @param dir If "in", contract the `bbox` by x_nudge and y_nudge. If "out",
#'   expand the bbox by x_nudge and y_nudge. If dir is not `NULL`; absolute
#'   values are used for x_nudge and y_nudge. Defaults to `NULL`. Optional
#'   [sf_bbox_shift()].
#' @inheritParams rlang::args_error_context
#' @export
#' @importFrom rlang caller_env arg_match has_length
sf_bbox_shift <- function(bbox,
                          x_nudge = 0,
                          y_nudge = 0,
                          side = c("all", "top", "bottom", "left", "right"),
                          dir = NULL,
                          call = caller_env()) {

  xy_nudge <- set_xy_nudge_list(x_nudge, y_nudge, dir)

  side <- arg_match(side, multiple = TRUE, error_call = call)

  check_side <- function(x, y) {
    is_any_in(c(x, "all"), y)
  }

  if (check_side("left", side)) {
    bbox <- nudge_bbox(bbox, xy_nudge[["x_nudge"]], "x", "min")
  }

  if (check_side("right", side)) {
    bbox <- nudge_bbox(bbox, xy_nudge[["x_nudge"]], "x", "max")
  }

  if (check_side("bottom", side)) {
    bbox <- nudge_bbox(bbox, xy_nudge[["y_nudge"]], "y", "min")
  }

  if (check_side("top", side)) {
    bbox <- nudge_bbox(bbox, xy_nudge[["y_nudge"]], "y", "max")
  }

  bbox
}

#' @noRd
set_xy_nudge_list <- function(x_nudge = 0,
                              y_nudge = 0,
                              dir = NULL,
                              call = caller_env()) {
  dir <- set_shift_dir(dir, call = call)

  check_number_decimal(x_nudge, call = call)
  check_number_decimal(y_nudge, call = call)

  if (has_length(x_nudge, 1) && has_length(y_nudge, 1)) {
    if (is_null(dir)) {
      x_nudge <- rep(x_nudge, 2)
      y_nudge <- rep(y_nudge, 2)
    } else if (is.numeric(dir)) {
      x_nudge <- dir * abs(x_nudge)
      y_nudge <- dir * abs(y_nudge)
    }
  }

  list(
    "x_nudge" = set_names(as.list(x_nudge), c("min", "max")),
    "y_nudge" = set_names(as.list(y_nudge), c("min", "max"))
  )
}

#' @noRd
nudge_bbox <- function(bb, nudge, dim = "x", side = "min") {
  dim <- paste0(dim, side)
  bb[[dim]] <- bb[[dim]] + nudge[[side]]
  bb
}

#' @noRd
set_shift_dir <- function(dir, allow_null = TRUE, call = caller_env()) {
  if (allow_null && is_null(dir)) {
    return(dir)
  }

  if (is.numeric(dir)) {
    return(c(dir * -1, dir))
  }

  dir <- arg_match0(dir, c("in", "out"), error_call = call)

  switch(dir,
    "in" = c(1, -1),
    "out" = c(-1, 1)
  )
}


#' @name sf_bbox_contract
#' @rdname sf_bbox_shift
#' @export
sf_bbox_contract <- function(bbox,
                             x_nudge = 0,
                             y_nudge = 0) {
  sf_bbox_shift(
    bbox = bbox,
    x_nudge = x_nudge,
    y_nudge = y_nudge,
    side = "all",
    dir = -1
  )
}

#' @name sf_bbox_expand
#' @rdname sf_bbox_shift
#' @export
sf_bbox_expand <- function(bbox,
                           x_nudge = 0,
                           y_nudge = 0) {
  sf_bbox_shift(
    bbox = bbox,
    x_nudge = x_nudge,
    y_nudge = y_nudge,
    side = "all",
    dir = 1
  )
}
