#' Get margins for a ggplot2 plot or map based on style or distance
#'
#' This function works in combination with the [get_paper()] function to make it
#' easier to position a map on a page before saving to file. This is primarily
#' useful when using a map or plot created with ggplot2 as part of a print
#' document format that is composed outside of R using a page layout application
#' such as Adobe InDesign.
#'
#' @param margin Margin style (options include "extrawide", "wide", "standard",
#'   "narrow", "none"), Additional "auto" option to generate margin based on
#'   line length is planned but not yet implemented. Default: `NULL` (equivalent
#'   to "none").
#' @param dist Margin distance (single value used to all sides), Default: `NULL`
#' @param unit Unit for margin distance, Default: 'in'.
#' @param block_width Plot or map width in units. If `paper` and `block_width` are
#'   provided, margins are half the distance between the two evenly distributed.
#'   This sets the margin distance for height as well as width so does not work
#'   well with header and footers and should be improved in the future.
#' @param header,footer Header and footer height in units; defaults to 0. Please
#'   note: headers and footers are not currently supported for "px" units.
#' @inheritParams get_paper
#' @return A [ggplot2::margin()] element intended for use with
#'   [ggplot2::element_rect()] and the `plot.background` theme element.
#' @example examples/get_margin.R
#' @seealso
#'  [ggplot2::margin()]
#' @rdname get_margin
#' @export
get_margin <- function(margin = NULL,
                       paper = NULL,
                       orientation = NULL,
                       dist = NULL,
                       unit = "in",
                       block_width = NULL,
                       header = 0,
                       footer = 0) {
  is_pkg_installed("ggplot2")

  if (is.character(margin) || is.null(margin)) {
    margin <-
      match.arg(
        margin,
        c("none", "narrow", "standard", "extrawide", "wide")
      )
  }

  unit <-
    match.arg(
      unit,
      c(
        "in", "mm", "px", "cm", "npc", "picas",
        "pc", "pt", "lines", "char", "native"
      )
    )

  if (!is.null(paper)) {
    if (!is.null(paper) && is.character(paper)) {
      paper <- get_paper(paper = paper, orientation = orientation)

      if (!is.null(block_width)) {
        dist <- (paper$width - block_width) / 2
      }
    } else if (is.data.frame(paper)) {
      is_df_paper(paper, ext = TRUE)

      # FIXME: get_paper only passes to get_margin if margin is a character value
      # but the value of margin does not matter to set the dist if block_width is provided.
      if (!is.null(block_width) && (nrow(paper) == 1)) {
        dist <- (paper$width - block_width) / 2
      }
    }
  }

  if (is.null(dist)) {
    if (is.character(margin) && (margin != "auto")) {
      margin <- get_margin_type(paper = paper, type = margin, unit = unit)
    } else if (margin == "auto") {
      # TODO: implement margin settings that respond to font family and size and/or paper
      # e.g. LaTeX default is 1.5 in margin w/ 12pt text, 1.75 in for 11pt, 1.875 for 10pt
      # See https://practicaltypography.com/page-margins.html for more information on linelength and margins
    }
  } else if (unit != "px") {
    margin <- get_margin_dist(dist = dist, unit = unit)
  }

  # FIXME: What is this doing? I think it is adding the header/footer on top of
  # the margin but only if unit is not px because ggplot2::units does not
  # support px units
  if (unit != "px") {
    margin <- margin + ggplot2::unit(x = c(header, 0, 0, footer), unit = unit)
  }

  if (!is.data.frame(paper)) {
    return(margin)
  }

  # FIXME: This should no longer be the default because the needed functionality is moved to get_paper
  # add_margin_to_paper(paper = paper, margin = margin)
  margin
}

#' Get margin based on distance
#'
#' @noRd
get_margin_dist <- function(dist = NULL, unit = "in") {
  if (length(dist) == 1) {
    ggplot2::margin(t = dist, r = dist, b = dist, l = dist, unit = unit)
  } else if (length(dist) == 4) {
    ggplot2::margin(t = dist[[1]], r = dist[[2]], b = dist[[3]], l = dist[[4]], unit = unit)
  }
}

#' Get margin based on standard type
#'
#' @noRd
get_margin_type <- function(paper = NULL, type = "none", unit = "in") {
  if (unit == "in") {
    margin <-
      switch(type,
        "extrawide" = ggplot2::margin(t = 2, r = 2, b = 2, l = 2, unit = unit),
        "wide" = ggplot2::margin(t = 1.5, r = 1.5, b = 1.5, l = 1.5, unit = unit),
        "standard" = ggplot2::margin(t = 1, r = 1, b = 1, l = 1, unit = unit),
        "narrow" = ggplot2::margin(t = 0.75, r = 0.75, b = 0.75, l = 0.75, unit = unit),
        "none" = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = unit)
      )
  } else if (unit == "mm") {
    margin <-
      switch(type,
        "extrawide" = ggplot2::margin(t = 80, r = 80, b = 80, l = 80, unit = unit),
        "wide" = ggplot2::margin(t = 60, r = 60, b = 60, l = 60, unit = unit),
        "standard" = ggplot2::margin(t = 40, r = 40, b = 40, l = 40, unit = unit),
        "narrow" = ggplot2::margin(t = 20, r = 20, b = 20, l = 20, unit = unit),
        "none" = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = unit)
      )
  } else if (unit == "px" && !is.null(paper)) {
    px_to_npc_margins <-
      function(px) {
        list(
          "t" = 1 - (px / paper$height),
          "r" = 1 - (px / paper$width),
          "b" = (px / paper$height),
          "l" = (px / paper$width)
        )
      }

    margin <-
      switch(type,
        "extrawide" = ggplot2::margin(px_to_npc_margins(120), unit = "npc"), # 1080 / 6
        "wide" = ggplot2::margin(px_to_npc_margins(80), unit = "npc"), # 1080 / 8
        "standard" = ggplot2::margin(px_to_npc_margins(40), unit = "npc"), # 1080 / 12
        "narrow" = ggplot2::margin(px_to_npc_margins(20), unit = "npc"),
        "none" = ggplot2::margin(px_to_npc_margins(0), unit = "npc")
      )
  }

  margin
}

#' Get paper with margin
#'
#' @noRd
#' @importFrom dplyr mutate
add_margin_to_paper <- function(paper = NULL, margin = NULL) {
  # FIXME: This is mainly for the case of get_paper calling get_margin. It could be moved to get_paper or to a separate utility function.
  paper_margin <- margin
  margin_num <- as.numeric(paper_margin)
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
    margin = list(paper_margin)
  )
}
