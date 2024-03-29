% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_dist_scale.R
\name{convert_dist_scale}
\alias{convert_dist_scale}
\title{Convert distance from scale to actual units}
\usage{
convert_dist_scale(
  dist = NULL,
  scale = NULL,
  scale_standard = NULL,
  scale_series = NULL,
  scale_unit = "in",
  scale_factor = NULL,
  actual_unit = NULL,
  dpi = 120,
  paper = NULL,
  orientation = NULL,
  ...
)
}
\arguments{
\item{dist}{distance to convert. If paper is provided, dist is optional and
paper width and height are used as dist.}

\item{scale}{Scale name from \code{standard_scales[["scale"]]}.}

\item{scale_standard, scale_series}{Passed to standard and scale parameters of
\code{\link[=get_scale]{get_scale()}}.}

\item{scale_unit}{"mm" (converted to cm by dividing by 10), "cm", "px"
(converted to inches by dividing by dpi), or "in".}

\item{scale_factor}{factor for converting from scale_unit to actual_unit,
e.g. if 1" = 1', the scale factor is 12. optional if scale if provided;
defaults to \code{NULL}.}

\item{actual_unit}{any unit supported by \code{\link[=convert_dist_units]{convert_dist_units()}}}

\item{dpi}{dots per square inch (used as conversion factor for "px" to "in")}

\item{paper}{Paper, Default: 'letter'.}

\item{orientation}{Orientation "portrait", "landscape", or "square", Default:
'portrait'.}

\item{...}{
  Arguments passed on to \code{\link[=get_paper]{get_paper}}
  \describe{
    \item{\code{standard}}{Size standard, "ANSI", "ISO", "British Imperial", "JIS",
"USPS", "Facebook", "Instagram", or "Twitter".}
    \item{\code{series}}{Size series (e.g. A), Default: \code{NULL}}
    \item{\code{size}}{Size number (only used for "ISO" and "JIS" series). Standard,
series, and size may all be required to return a single paper when using
these parameters.}
    \item{\code{width,height}}{Width and height in units, Default: \code{NULL}.}
    \item{\code{units}}{Paper size units, either "in", "mm", or "px"; defaults to \code{NULL}
(using "in" if width or height are provided).}
    \item{\code{ncol,nrow}}{Number of expected columns and rows in paper; used to
determine row_height and section_asp in paper data frame returned by
get_paper if nrow or ncol is greater than 1; defaults to \code{NULL}.}
    \item{\code{gutter}}{Gutter distance in units. Gutter is used as the spacing between
nrow and columns (variable spacing is not currently supported); defaults to 0.}
    \item{\code{margin}}{A numeric vector or ggplot2 margin object.}
    \item{\code{bbox}}{A bounding box to use to get orientation using \code{\link[=sf_bbox_asp]{sf_bbox_asp()}} with orientation = TRUE.}
  }}
}
\value{
\itemize{
\item If paper is not provided, return a vector of dist values converted from
scale_unit to actual_unit based on scale_factor or information from
\link{standard_scales} data.
\item If paper is provided, return a data.frame with converted distances appends
as columns named actual_width and actual_height.
}
}
\description{
This function converts scale distances to actual units based on named
\link{standard_scales}.
}
\seealso{
Other dist: 
\code{\link{compare_dist}()},
\code{\link{convert_dist_units}()},
\code{\link{get_measurements}},
\code{\link{is_dist_units}()},
\code{\link{sf_bbox_dist}()}
}
\concept{dist}
