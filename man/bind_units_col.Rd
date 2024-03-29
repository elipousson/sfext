% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bind_units_col.R
\name{bind_units_col}
\alias{bind_units_col}
\title{Bind units column to data frame}
\usage{
bind_units_col(
  x,
  y,
  units = NULL,
  drop = FALSE,
  keep_all = TRUE,
  .id = NULL,
  call = caller_env()
)
}
\arguments{
\item{x}{Data frame or sf object.}

\item{y}{Vector of numeric or units values to bind to x.}

\item{units}{Units to use for y (if numeric) or convert to (if y is units
class); defaults to \code{NULL}.}

\item{drop}{If \code{TRUE}, apply the \link[units:drop_units]{units::drop_units} function to the column
with units class values and return numeric values instead; defaults to
\code{FALSE}.}

\item{keep_all}{If \code{FALSE}, keep all columns. If \code{FALSE}, return only the
named .id column.}

\item{.id}{Name to use for vector of units provided to "y" parameter, when
"y" is bound to the "x" data frame or tibble as a new column.}

\item{call}{The execution environment of a currently
running function, e.g. \code{caller_env()}. The function will be
mentioned in error messages as the source of the error. See the
\code{call} argument of \code{\link[rlang:abort]{abort()}} for more information.}
}
\description{
Utility function supporting \link{get_area}, \link{get_dist}, and \link{get_bearing}.
}
