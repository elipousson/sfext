% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_bbox_ext.R
\name{st_bbox_ext}
\alias{st_bbox_ext}
\alias{st_bbox_adj}
\alias{st_bbox_asp}
\title{Get a bounding box buffered a set distance or to match an aspect ratio}
\usage{
st_bbox_ext(
  x,
  dist = NULL,
  diag_ratio = NULL,
  asp = NULL,
  unit = NULL,
  crs = NULL,
  class = "bbox",
  nudge = NULL,
  allow_null = TRUE,
  allow_list = TRUE
)

st_bbox_asp(
  x,
  asp = NULL,
  class = "bbox",
  allow_null = TRUE,
  allow_list = TRUE
)
}
\arguments{
\item{x}{A \code{sf}, \code{sfc}, \code{bbox}, \code{sfg}, \code{Raster}, \code{Spatial}, \code{Extent},
\code{numeric}, or \code{character} object (a place name passed to
\code{\link[osmdata:getbb]{osmdata::getbb()}}). See \code{\link[=as_bbox]{as_bbox()}} for more details.}

\item{dist}{buffer distance in units. Optional.}

\item{diag_ratio}{ratio of diagonal distance of area's bounding box used as
buffer distance. e.g. if the diagonal distance is 3000 meters and the
"diag_ratio = 0.1"  a 300 meter will be used. Ignored when \code{dist} is
provided.}

\item{asp}{Aspect ratio of width to height as a numeric value (e.g. 0.33) or
character (e.g. "1:3"). If numeric, \code{\link[=get_asp]{get_asp()}} returns the same value
without modification.}

\item{unit}{Units for buffer. Supported options include "meter", "foot",
"kilometer", and "mile", "nautical mile" Common abbreviations (e.g. "km"
instead of "kilometer") are also supported. Distance in units is converted
to units matching GDAL units for x; defaults to "meter"}

\item{crs}{Coordinate reference system of bounding box to return; defaults to
\code{NULL} which maintains the crs of the input object.}

\item{class}{Class of object to return passed to \code{\link[=as_sf_class]{as_sf_class()}}; defaults to
"bbox".}

\item{nudge}{Passed as to parameter \code{\link[=st_nudge]{st_nudge()}} when not \code{NULL}. A numeric
vector, a \code{sf} object, or any other object that can be converted to a
simple feature collection with \code{as_sfc()}..}

\item{allow_null}{If \code{TRUE} (default) and x is \code{NULL}, return \code{NULL} or, if
\code{FALSE}, abort function.}

\item{allow_list}{If \code{TRUE}, allow sf list objects as an input and use
\code{\link[purrr:map]{purrr::map()}} to apply the provided parameters to each object within the
list to return as a new sf list object.}
}
\value{
A \code{bbox} object converted to match the class of the class parameter.
}
\description{
\code{\link[=st_bbox_ext]{st_bbox_ext()}} converts the input to a bounding box with \code{\link[=as_bbox]{as_bbox()}},
applies a buffer (based on a specified distance or proportion of the diagonal
distance across the bounding box) and adjusts the bounding box aspect ratio
before returning a bounding box (or another class specified by the class
parameter). If the input object is a list or \code{sf_list} object, the function
always returns a \code{list} or \code{sf_list}.
}
\details{
\code{\link[=st_bbox_asp]{st_bbox_asp()}} supports aspect ratio adjustments without applying a buffer.
asp can be supplied as a number or a string matching the format of
"width:height". Common aspect ratios include "1:1" (1), "4:6" (0.666),
"8.5:11", "16:9" (1.777).
}
