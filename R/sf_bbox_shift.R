#' Shift sides, contract, or expand a bounding box
#'
#' @description
#' - Shift, expand, or contract a bounding box ([sf_bbox_shift()],
#' [sf_bbox_expand()], [sf_bbox_contract()])
#'
#' @name sf_bbox_shift
#' @param bbox A `bbox` object.
#' @param nudge_x,nudge_y Length 1 or 2 numeric vector; unitless. Used by or
#'   passed to [sf_bbox_shift()]. Required for  [sf_bbox_shift()].
#' @param side one or more sides to shift: "top", "bottom", "left", "right", or
#'   "all". Required for [sf_bbox_shift()].
#' @param dir If "in", contract the `bbox` by nudge_x and nudge_y. If "out",
#'   expand the bbox by nudge_x and nudge_y. If dir is not `NULL`; absolute
#'   values are used for nudge_x and nudge_y. Defaults to `NULL`. Optional
#'   [sf_bbox_shift()].
#' @inheritParams rlang::args_error_context
#' @export
#' @importFrom rlang caller_env arg_match has_length
sf_bbox_shift <- function(bbox,
                          nudge_x = 0,
                          nudge_y = 0,
                          side = c("all", "top", "bottom", "left", "right"),
                          dir = NULL,
                          call = caller_env()) {
  dir <- set_shift_dir(dir)

  nudge_x <- set_bbox_nudge(nudge_x, dir)
  nudge_y <- set_bbox_nudge(nudge_y, dir)

  has_side <- function(x, y) {
    any(c(x, "all") %in% y)
  }

  nudge_bbox <- function(bb, nudge, dim = "x", side = "min") {
    dim <- paste0(dim, side)
    bb[[dim]] <- bb[[dim]] + nudge[[side]]
    bb
  }

  side <- arg_match(side, multiple = TRUE, error_call = call)

  if (has_side("left", side)) {
    bbox <- nudge_bbox(bbox, nudge_x, "x", "min")
  }

  if (has_side("right", side)) {
    bbox <- nudge_bbox(bbox, nudge_x, "x", "max")
  }

  if (has_side("bottom", side)) {
    bbox <- nudge_bbox(bbox, nudge_y, "y", "min")
  }

  if (has_side("top", side)) {
    bbox <- nudge_bbox(bbox, nudge_y, "y", "max")
  }

  bbox
}

#' @noRd
set_bbox_nudge <- function(nudge, dir) {
  if (has_length(nudge, 1)) {
    nudge <- rep(nudge, 2)
  }

  if (is.numeric(dir)) {
    nudge <- dir * abs(nudge)
  }

  set_names(as.list(nudge), c("min", "max"))
}


#' @noRd
set_shift_dir <- function(dir = NULL,
                          allow_null = TRUE,
                          call = caller_env()) {
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
                             nudge_x = 0,
                             nudge_y = 0) {
  sf_bbox_shift(
    bbox = bbox,
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    side = "all",
    dir = "in"
  )
}

#' @name sf_bbox_expand
#' @rdname sf_bbox_shift
#' @export
sf_bbox_expand <- function(bbox,
                           nudge_x = 0,
                           nudge_y = 0) {
  sf_bbox_shift(
    bbox = bbox,
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    side = "all",
    dir = "out"
  )
}
