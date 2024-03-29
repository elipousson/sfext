% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_filter_ext.R
\name{st_filter_ext}
\alias{st_filter_ext}
\alias{st_filter_geom_type}
\title{Filter, crop, trim, or erase a simple feature object or list}
\usage{
st_filter_ext(
  x,
  y = NULL,
  crop = FALSE,
  trim = FALSE,
  erase = FALSE,
  crs = NULL,
  .predicate = sf::st_intersects,
  type = NULL,
  allow_list = TRUE,
  ...
)

st_filter_geom_type(x, type = NULL)
}
\arguments{
\item{x, y}{A \code{sf}, \code{sfc}, or \code{bbox} object. x may also be a \code{sf} list
objects. If x is an \code{sf} list, additional parameters in \code{...} will be
ignored.}

\item{crop}{If \code{TRUE}, x is cropped to y using \code{\link[sf:st_crop]{sf::st_crop()}}.}

\item{trim}{If \code{TRUE}, x is trimmed to y with \code{\link[=st_trim]{st_trim()}}.}

\item{erase}{If \code{TRUE}, x is erased by y with \code{\link[=st_erase]{st_erase()}}.}

\item{crs}{Coordinate reference system to return.}

\item{.predicate}{geometry predicate function with the same profile as
\code{\link[sf:geos_binary_pred]{sf::st_intersects()}}; see details for \code{\link[sf:st_join]{sf::st_filter()}} for more options.}

\item{type}{Geometry type.}

\item{allow_list}{If \code{TRUE}, x can be a list of \code{sf}, \code{sfc}, or \code{bbox} objects.
If \code{FALSE}, only \code{sf}, \code{sfc}, or \code{bbox} objects are supported. Defaults to
\code{TRUE}.}

\item{...}{
  Arguments passed on to \code{\link[sf:st_join]{sf::st_filter}}
  \describe{
    \item{\code{}}{}
  }}
}
\description{
Extend \code{\link[sf:st_join]{sf::st_filter()}} to filter a sf list or a \code{sf}, \code{sfc}, or \code{bbox} with
options to crop, trim or erase the geometry of the input object based on a
predicate function. Returns x transformed to match crs if y is \code{NULL}.
}
\examples{
nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

plot(
  st_filter_ext(
    nc,
    nc[c(1:10), ]
  ),
  max.plot = 1
)

plot(
  st_filter_ext(
    nc,
    nc[c(1:10), ],
    crop = TRUE
  ),
  max.plot = 1
)


plot(
  st_filter_ext(
    nc,
    nc[c(1:10), ],
    erase = TRUE
  ),
  max.plot = 1
)

plot(
  st_filter_ext(
    nc,
    sf::st_union(nc[c(1:10), ]),
    .predicate = sf::st_disjoint
  ),
  max.plot = 1
)

st_filter_geom_type(nc, "POINT")

st_filter_geom_type(nc, "MULTIPOLYGON")
}
