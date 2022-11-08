#' Distance units (data frame)
#'
#' A subset of units supported by the units package accessible through the
#' [units::valid_udunits()] function.
#'
#' @format A data frame with 15 rows and 11 variables:
#' \describe{
#'   \item{`symbol`}{symbols}
#'   \item{`symbol_aliases`}{symbol aliases}
#'   \item{`name_singular`}{singular names}
#'   \item{`name_singular_aliases`}{singular name aliases}
#'   \item{`name_plural`}{character plural names}
#'   \item{`name_plural_aliases`}{plural name aliases}
#'   \item{`def`}{short definition}
#'   \item{`definition`}{definition}
#'   \item{`comment`}{comment}
#'   \item{`dimensionless`}{logical indicator for dimensionless units}
#'   \item{`source_xml`}{source XML}
#' }
"dist_units"

#' Distance units (vector)
#'
#' A vector of supported distance units pulled from `dist_units`.
#'
#' @format A character vector with 60 names, plural names, aliases, and symbols
#'   for distance units.
"dist_unit_options"

#' Area units (vector)
#'
#' A vector of supported area units derived from `dist_units` and
#' [units::valid_udunits()].
#'
#' @format A character vector with 40 names, plural names, and aliases for area
#'   units.
"area_unit_options"

#' Standard map, architectural, and engineering scales
#'
#' Standard map scales derived from USGS 2002 report on map scales
#' <https://pubs.usgs.gov/fs/2002/0015/report.pdf>
#'
#' Common architectural and engineering scales derived from FEMA guide to using
#' scales
#' <https://www.usfa.fema.gov/downloads/pdf/nfa/engineer-architect-scales.pdf>
#'
#' @format A data frame with 32 rows and 16 variables:
#' \describe{
#'   \item{`scale`}{Scale name}
#'   \item{`standard`}{Standard (USGS, architectural, or engineering)}
#'   \item{`series`}{Series name (USGS map scales only)}
#'   \item{`actual_ft`}{Scale distance for 1 ft actual.}
#'   \item{`actual_ft_unit`}{Unit of scale for 1 ft actual.}
#'   \item{`scale_in`}{Actual distance for 1 in scale.}
#'   \item{`scale_in_unit`}{Unit of actual distance for 1 in scale.}
#'   \item{`scale_in_accuracy`}{Accuracy of 1 in scale (approximate or exact)}
#'   \item{`scale_cm`}{Actual distance for 1 cm scale.}
#'   \item{`scale_cm_unit`}{Unit of actual distance for 1 cm scale.}
#'   \item{`scale_cm_accuracy`}{Accuracy of 1 cm scale (approximate or exact)}
#'   \item{`size_latlon`}{Standard size in latitude/longitude}
#'   \item{`size_latlon_unit`}{Unit of latitude/longitude size (minutes or degrees)}
#'   \item{`area_approx`}{Approximate actual area}
#'   \item{`area_approx_unit`}{Approximate area unit}
#'   \item{`series_status`}{Series status (select USGS map series are "abandoned")}
#' }
"standard_scales"

#'  Standard paper and image sizes
#'
#' Reference table of standard paper, postcard, photo print, and social media image
#' sizes, for [get_paper()] and [get_social_image()] functions. Derived from
#' [visioguy/PaperSizes](https://github.com/visioguy/PaperSizes/) repo, [Adobe UK
#' guide to photo
#' sizes](https://www.adobe.com/uk/creativecloud/photography/discover/standard-photo-sizes.html)
#' and other sources.
#'
#' @format A data frame with 85 rows and 9 variables:
#' \describe{
#'   \item{`name`}{Name of paper}
#'   \item{`series`}{Series}
#'   \item{`standard`}{Standard}
#'   \item{`size`}{Size in series}
#'   \item{`units`}{Units ("in", "mm", or "px") for dimensions}
#'   \item{`width`}{Width in units}
#'   \item{`height`}{Height in units}
#'   \item{`orientation`}{Portrait (width less than height), landscape, or square}
#'   \item{`type`}{Type (paper, postcard, print, or social)}
#' }
"paper_sizes"
