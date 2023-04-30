#' Get aspect ratio from string or based on specific paper and margins
#'
#' @param asp Aspect ratio of width to height as a numeric value (e.g. 0.33) or
#'   character (e.g. "1:3"). If numeric, [get_asp()] returns the same value
#'   without modification.
#' @param block_asp If `TRUE`, and margin is not `NULL`, return the aspect ratio
#'   of the text or content block inside the page margins.
#' @param allow_null If `TRUE` and asp and paper are both `NULL`, return `NULL`
#'   without an error.
#' @return A numeric aspect ratio.
#' @rdname get_asp
#' @inheritParams get_paper
#' @inheritDotParams get_paper
#' @export
get_asp <- function(asp = NULL,
                    paper = NULL,
                    orientation = NULL,
                    bbox = NULL,
                    margin = NULL,
                    block_asp = FALSE,
                    allow_null = TRUE,
                    ...) {
  type <-
    dplyr::case_when(
      allow_null && is_null(asp) && is_null(paper) ~ "null",
      is.numeric(asp) ~ "num",
      is.character(asp) && grepl(":", asp) ~ "char",
      !is_null(paper) && is_null(asp) && !block_asp ~ "paper",
      !is_null(paper) && is_null(asp) && block_asp ~ "block",
      !is_null(bbox) ~ "bbox",
      TRUE ~ "abort"
    )

  cli_abort_ifnot(
    "A {.arg asp} or {.arg paper} input value must be provided.",
    condition = (type != "abort")
  )

  cli_warn_ifnot(
    "{.arg margin} is ignored if {.arg block_asp} is {.val FALSE}.",
    condition = !block_asp && is_null(margin)
  )

  switch(type,
    "null" = asp,
    "num" = asp,
    "char" = as.numeric(str_extract(asp, ".+(?=:)")) / as.numeric(str_extract(asp, "(?<=:).+")),
    "paper" = get_paper(paper = paper, orientation = orientation, bbox = bbox, ...)[["asp"]],
    "block" = get_paper(paper = paper, orientation = orientation, bbox = bbox, margin = margin, ...)[["block_asp"]],
    "bbox" = sf_bbox_asp(bbox)
  )
}
