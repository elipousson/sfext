% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_scale_rotate.R
\name{st_scale_rotate}
\alias{st_scale_rotate}
\title{Scale and rotate a simple feature object, simple feature collection, or
bounding box}
\usage{
st_scale_rotate(x, scale = 1, rotate = 0, call = caller_env())
}
\arguments{
\item{x}{A \code{sf}, \code{sfc}, or \code{bbox} object or another object coercible to a
simple feature collection with \code{\link[=as_sfc]{as_sfc()}}.}

\item{scale}{numeric; scale factor, Default: 1}

\item{rotate}{numeric; degrees to rotate (-360 to 360), Default: 0}

\item{call}{The execution environment of a currently
running function, e.g. \code{caller_env()}. The function will be
mentioned in error messages as the source of the error. See the
\code{call} argument of \code{\link[rlang:abort]{abort()}} for more information.}
}
\description{
Scale or rotate a simple feature or bounding box object using affine
transformations.
}
\examples{
nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
nc <- sf::st_transform(nc, crs = 3857)

plot(st_scale_rotate(nc, scale = 0.75, rotate = 15), max.plot = 1)

}
