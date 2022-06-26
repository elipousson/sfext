
#' Convert distance from scale to actual units
#'
#' Convert distance from scale to actual units based on named `standard_scales`
#'
#' @param dist distance to convert. If paper is provided, paper width and height
#'   are used as dist.
#' @inheritParams get_paper
#' @inheritParams get_scale
#' @param scale_unit "mm" (converted to cm by dividing by 10), "cm", "px"
#'   (converted to inches by dividing by dpi), or "in".
#' @param actual_unit any unit supported by convert_dist_units
#' @param scale_factor factor for converting from scale_unit to actual_unit,
#'   e.g. if 1" = 1', the scale factor is 12. optional if scale if provided;
#'   defaults to `NULL`.
#' @param dpi dots per square inch (used as conversion factor for "px" to "in")
#' @return dist values converted from scale_unit to actual_unit based on
#'   scale_factor or information from standard_scales object. If paper is
#'   provided, return a data frame with converted distances as actual_width and
#'   actual_height
#' @family dist
#' @export
convert_dist_scale <- function(dist = NULL,
                               paper = NULL,
                               orientation = NULL,
                               scale = NULL,
                               scale_unit = "in",
                               actual_unit = NULL,
                               scale_factor = NULL,
                               dpi = 120) {
  if (!is.null(scale)) {
    scale <- get_scale(scale = scale)
  }

  if (!is.null(paper) && is.null(dist)) {
    paper <- get_paper(paper = paper, orientation = orientation)
    dist <- c(paper$width, paper$height)
    scale_unit <- paper$units
  }

  dist <-
    switch(scale_unit,
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
    actual_unit <-
      scale[[paste0("scale_", scale_unit, "_unit")]]

    scale_factor <-
      scale[[paste0("scale_", scale_unit)]]
  } else {
    stopifnot(
      is.numeric(scale_factor),
      !is.null(actual_unit)
    )
  }

  dist <- convert_dist_units(
    dist = dist * scale_factor,
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
