#' Convert distance from scale to actual units
#'
#' This function converts scale distances to actual units based on named
#' [standard_scales].
#'
#' @param dist distance to convert. If paper is provided, dist is optional and
#'   paper width and height are used as dist.
#' @inheritParams get_scale
#' @param scale_standard,scale_series Passed to standard and scale parameters of
#'   [get_scale()].
#' @param scale_unit "mm" (converted to cm by dividing by 10), "cm", "px"
#'   (converted to inches by dividing by dpi), or "in".
#' @param scale_factor factor for converting from scale_unit to actual_unit,
#'   e.g. if 1" = 1', the scale factor is 12. optional if scale if provided;
#'   defaults to `NULL`.
#' @param actual_unit any unit supported by [convert_dist_units()]
#' @param dpi dots per square inch (used as conversion factor for "px" to "in")
#' @inheritParams get_paper
#' @inheritDotParams get_paper
#' @returns
#' - If paper is not provided, return a vector of dist values converted from
#' scale_unit to actual_unit based on scale_factor or information from
#' [standard_scales] data.
#' - If paper is provided, return a data.frame with converted distances appends
#' as columns named actual_width and actual_height.
#' @family dist
#' @export
convert_dist_scale <- function(dist = NULL,
                               scale = NULL,
                               scale_standard = NULL,
                               scale_series = NULL,
                               scale_unit = "in",
                               scale_factor = NULL,
                               actual_unit = NULL,
                               dpi = 120,
                               paper = NULL,
                               orientation = NULL,
                               ...) {
  if (is.character(scale) && has_length(scale, 1)) {
    scale_nm <- scale
    scale <- get_scale(scale = scale, standard = scale_standard, series = scale_series)

    cli_abort_ifnot(
      nrow(scale) == 1,
      message = c("{.arg scale} {.val {scale_nm}} returned {nrow(scale)} scales from {.code standard_scales}.",
        "i" = "Provide {.arg scale_standard} and {.arg scale_series} parameters to return only 1 scale."
      )
    )

    cli_warn_ifnot(
      is.null(actual_unit),
      message = c("{.arg actual_unit} is ignored if {.arg scale} is provided.")
    )
  }

  if (!is.null(paper) && is.null(dist)) {
    paper_nm <- paper
    paper <- get_paper(paper = paper, orientation = orientation, ...)

    cli_abort_ifnot(
      nrow(paper) == 1,
      message = c("{.arg paper} {.val {paper_nm}} returned {nrow(paper)} options from {.code paper_sizes}.",
        "i" = "Provide {.arg orientation} or other parameters for {.fn get_paper} to return 1 option."
      )
    )

    dist <- c(paper$width, paper$height)

    cli_warn_ifnot(
      is.null(scale_unit) || is.null(scale_factor) || (!is.null(scale_unit) && scale_unit == paper$units),
      message = c("{.arg scale_unit} and {.arg scale_factor} are ignored if {.arg paper} is provided.")
    )

    scale_unit <- paper$units
  }

  dist <- switch(scale_unit,
    "mm" = dist / 10,
    "cm" = dist,
    # FIXME: Double-check how this handles px
    "px" = dist / dpi,
    "in" = dist
  )

  if (scale_unit %in% c("mm", "cm")) {
    scale_unit <- "cm"
  } else if (scale_unit %in% c("px", "in")) {
    scale_unit <- "in"
  }

  if (is.data.frame(scale)) {
    actual_unit <- scale[[paste0("scale_", scale_unit, "_unit")]]
    scale_factor <- scale[[paste0("scale_", scale_unit)]]
  } else {
    stopifnot(
      is.numeric(scale_factor),
      !is.null(actual_unit)
    )
  }

  dist <- convert_dist_units(
    dist = as.numeric(dist * scale_factor),
    to = actual_unit
  )

  if (is.null(paper)) {
    return(dist)
  }

  paper$actual_width <- dist[[1]]
  paper$actual_height <- dist[[2]]
  paper$scale <- scale$scale

  paper
}
