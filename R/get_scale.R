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
