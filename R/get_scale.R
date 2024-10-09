#' Get standard scales and convert to scale distances
#'
#' This function returns a scale from [standard_scales] based on a provided
#' name, standard, and/or series.
#'
#' @name get_scale
#' @aliases get_standard_scale
#' @param scale Scale name from `standard_scales[["scale"]]`.
#' @param standard Scale standard. Options include "USGS", "Engineering", or
#'   "Architectural".
#' @param series Map series from `standard_scales[["series"]]`. Series is only
#'   available for USGS scales.
#' @return A tibble based on [standard_scales] with rows filtered to values that
#'   match parameters.
#' @export
#' @importFrom dplyr filter
get_scale <- function(scale = NULL,
                      standard = NULL,
                      series = NULL) {
  select_scale <- sfext::standard_scales

  if (!is_null(scale)) {
    select_scale <- dplyr::filter(select_scale, .data$scale %in% {{ scale }})
  }

  if (!is_null(standard)) {
    standard <- arg_match(standard, c("USGS", "Engineering", "Architectural"), multiple = TRUE)
    select_scale <- dplyr::filter(select_scale, .data$standard %in% {{ standard }})
  }

  if (!is_null(series)) {
    series_values <- unique(sfext::standard_scales$series)
    series <- arg_match(series, series_values, multiple = TRUE)
    select_scale <- dplyr::filter(select_scale, .data$series %in% {{ series }})
  }

  select_scale
}
