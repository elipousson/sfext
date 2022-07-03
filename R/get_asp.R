
#' Get aspect ratio from string or based on specific paper and margins
#'
#'
#' @param asp Aspect ratio of width to height as a numeric value (e.g. 0.33) or
#'   character (e.g. "1:3"). If numeric, [get_asp()] returns the same value
#'   without modification.
#' @return A numeric aspect ratio.
#' @rdname get_asp
#' @export
#' @importFrom stringr str_extract
get_asp <- function(asp = NULL,
                    paper = NULL,
                    orientation = NULL,
                    margin = NULL,
                    unit = NULL,
                    block_asp = FALSE,
                    null.ok = TRUE) {
  type <-
    dplyr::case_when(
      is.null(asp) && is.null(paper) && null.ok ~ "null",
      is.numeric(asp) ~ "num",
      is.character(asp) && grepl(":", asp) ~ "char",
      !is.null(paper) && is.null(asp) && !block_asp ~ "paper",
      !is.null(paper) && is.null(asp) && block_asp ~ "block",
      TRUE ~ "other"
    )

  stopifnot(
    !is.na(type),
    type != "other"
  )

  switch(type,
    "null" = asp,
    "num" = asp,
    "char" = as.numeric(stringr::str_extract(asp, ".+(?=:)")) / as.numeric(stringr::str_extract(asp, "(?<=:).+")) # ,
    # "paper" = get_paper(paper = paper, orientation = orientation)[["asp"]],
    # "block" = get_block_asp(paper = get_paper(paper = paper, orientation = orientation), margin = margin, unit = unit)
  )
}

#' Get standard scales and convert to scale distances
#'
#' @name get_scale
#' @aliases get_standard_scale
#' @param scale Scale name
#' @param standard USGS, Engineering, or Architectural
#' @param series Map series
#' @export
#' @importFrom dplyr filter
get_scale <- function(scale = NULL,
                      standard = NULL,
                      series = NULL) {
  select_scale <- standard_scales

  if (!is.null(scale)) {
    select_scale <- dplyr::filter(select_scale, .data$scale %in% {{ scale }})
  }

  if (!is.null(standard)) {
    standard <- arg_match(standard, c("USGS", "Engineering", "Architectural"), multiple = TRUE)
    select_scale <- dplyr::filter(select_scale, .data$standard %in% {{ standard }})
  }

  if (!is.null(series)) {
    series <- arg_match(series, unique(standard_scales$series), multiple = TRUE)
    select_scale <- dplyr::filter(select_scale, .data$series %in% {{ series }})
  }

  select_scale
}
