% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf_to_df.R
\name{sf_to_df}
\alias{sf_to_df}
\alias{df_to_sf}
\title{Convert between simple feature and data frame objects}
\usage{
sf_to_df(
  x,
  crs = 4326,
  coords = c("lon", "lat"),
  geometry = "centroid",
  keep_all = TRUE
)

df_to_sf(
  x,
  crs = NULL,
  coords = c("lon", "lat"),
  from_crs = 4326,
  into = NULL,
  sep = ",",
  rev = TRUE,
  remove_coords = FALSE,
  geo = FALSE,
  address = "address",
  y = NULL,
  by = NULL,
  ...,
  as_tibble = TRUE,
  call = caller_env()
)
}
\arguments{
\item{x}{A \code{sf} or \code{sfc} object or a data frame with lat/lon coordinates in a
single column or two separated columns.}

\item{crs}{Cordinate reference system to return, Default: 4326 for
\code{\link[=sf_to_df]{sf_to_df()}} and \code{NULL} for \code{\link[=df_to_sf]{df_to_sf()}}.}

\item{coords}{Coordinate columns for input data.frame or output sf object (if
geometry is 'centroid' or 'point') Default: c("lon", "lat").}

\item{geometry}{Type of geometry to include in data frame. options include
"drop", "wkt", "centroid", "point", Default: 'centroid'.}

\item{keep_all}{If \code{FALSE}, drop all columns other than those named in
coords, Default: \code{TRUE}.}

\item{from_crs}{For \code{\link[=df_to_sf]{df_to_sf()}}, coordinate reference system used by
coordinates or well known text in data frame.}

\item{into}{If coords is a single column name with both longitude and
latitude, \code{into} is used as the names of the new columns that coords is
separated into. Passed to \code{\link[tidyr:separate]{tidyr::separate()}}.}

\item{sep}{If coords is a single column name with both longitude and
latitude, \code{sep} is used as the separator between coordinate values. Passed
to \code{\link[tidyr:separate]{tidyr::separate()}}.}

\item{rev}{If \code{TRUE}, reverse \code{c("lat", "lon")} coords to \code{c("lon", "lat")}.
\code{\link[=check_coords]{check_coords()}} only.}

\item{remove_coords}{For \code{\link[=df_to_sf]{df_to_sf()}}, if \code{TRUE}, remove the coordinate columns
after converting a data frame to simple feature object; defaults to
\code{FALSE}.}

\item{geo}{If \code{TRUE}, use \code{\link[=address_to_sf]{address_to_sf()}} to geocode address column; defaults
to \code{FALSE}.}

\item{address}{Address column name passed to \code{\link[tidygeocoder:geocode]{tidygeocoder::geocode()}} or
\link[tidygeocoder:geo]{tidygeocoder::geo}}

\item{y}{A sf object passed as y argument to \code{\link[dplyr:mutate-joins]{dplyr::left_join()}}.}

\item{by}{A character vector of variables to join by passed to
\code{\link[dplyr:mutate-joins]{dplyr::left_join()}}.}

\item{...}{Other parameters passed onto methods.}

\item{call}{The execution environment of a currently
running function, e.g. \code{caller_env()}. The function will be
mentioned in error messages as the source of the error. See the
\code{call} argument of \code{\link[rlang:abort]{abort()}} for more information.}
}
\value{
\code{\link[=sf_to_df]{sf_to_df()}} returns a data frame with geometry dropped or converted
to wkt or coordinates for the centroid or point on surface; \code{\link[=df_to_sf]{df_to_sf()}}
returns a simple feature object with POINT geometry.
}
\description{
\code{\link[=sf_to_df]{sf_to_df()}} converts a simple feature object to a data.frame by dropping
geometry or, using \code{\link[=get_coords]{get_coords()}}, converting geometry to well known text, or
(if the geometry type is not POINT) getting coordinates for a centroid or
point on surface. If an sfc object is provided,the "drop" geometry option is
not supported.

\code{\link[=df_to_sf]{df_to_sf()}} converts a data.frame to a simple feature object using a
geometry column (named "geometry"), address column (name matching the address
parameter), well known text column (named "wkt"), or coords (must have
columns matching coords values). Related helper functions include
\code{\link[=check_coords]{check_coords()}} to suggest the appropriate coordinate column names based on
the column names in the provided data frame; \code{\link[=has_coords]{has_coords()}} to check if a
data frame has the specified coordinates.
}
\examples{
nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))

# Convert a sf object to a data frame
nc_df <- sf_to_df(nc)

# Convert a data frame to a sf object
df_to_sf(nc_df, coords = c("lon", "lat"), remove_coords = TRUE)

# If lon and lat values are present in a single column, use the into parameter
# to split the values back into separate columns
nc_df$xy <- paste(nc_df$lon, nc_df$lat, sep = ",")

df_to_sf(nc_df, coords = "xy", into = c("lon", "lat"))
}
\seealso{
\code{\link[sf:st_coordinates]{sf::st_coordinates()}}

\code{\link[ggspatial:df_spatial]{ggspatial::df_spatial()}}
\code{\link[sf:st_as_sf]{sf::st_as_sf()}}
}
