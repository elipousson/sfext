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
#' @param call Passed as the error_call parameter for [rlang::arg_match] to
#'   improve error messages when function is used internally.
#' @export
#' @importFrom rlang caller_env arg_match has_length
sf_bbox_shift <- function(bbox,
                          x_nudge = 0,
                          y_nudge = 0,
                          side = c("all", "top", "bottom", "left", "right"),
                          dir = NULL,
                          call = caller_env()) {
  side <- arg_match(side, multiple = TRUE, error_call = call)

  if (is.character(dir)) {
    dir <-
      switch(dir,
        "in" = c(1, -1),
        "out" = c(-1, 1)
      )
  } else if (is.numeric(dir)) {
    dir <- c(dir * -1, dir)
  }

  stopifnot(
    is.numeric(x_nudge) && is.numeric(y_nudge),
    (length(x_nudge) <= 2) && (length(y_nudge) <= 2)
  )

  if ((length(x_nudge) == 1) && (length(y_nudge) == 1)) {
    if (is.null(dir)) {
      x_nudge <- as.list(rep(x_nudge, 2))
      y_nudge <- as.list(rep(y_nudge, 2))
    } else if (is.numeric(dir)) {
      x_nudge <- as.list(dir * abs(x_nudge))
      y_nudge <- as.list(dir * abs(y_nudge))
    }
  } else if ((length(x_nudge) == 2) && (length(y_nudge) == 2)) {
    x_nudge <- as.list(x_nudge)
    y_nudge <- as.list(y_nudge)
  }

  nm <- c("min", "max")
  x_nudge <- rlang::set_names(x_nudge, nm)
  y_nudge <- rlang::set_names(y_nudge, nm)

  check_side <- function(x, y) {
    is_any_in(c(x, "all"), y)
  }

  nudge_bbox <- function(bb, nudge, dim = "x", side = "min") {
    dim <- paste0(dim, side)
    bb[[dim]] <- bb[[dim]] + nudge[[side]]
    bb
  }

  if (check_side("left", side)) {
    bbox <- nudge_bbox(bbox, x_nudge, "x", "min")
  }

  if (check_side("right", side)) {
    bbox <- nudge_bbox(bbox, x_nudge, "x", "max")
  }

  if (check_side("bottom", side)) {
    bbox <- nudge_bbox(bbox, y_nudge, "y", "min")
  }

  if (check_side("top", side)) {
    bbox <- nudge_bbox(bbox, y_nudge, "y", "max")
  }

  bbox
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
