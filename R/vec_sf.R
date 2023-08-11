#' Vectorized wrapper for `sf::st_union()`
#'
#' [vec_st_union()] adds vctrs recycling rules to [sf::st_union()] by assuming
#' that y must be length 1 or the same length as x and that if y is the same
#' length as x, the function should union each element in turn, e.g. first
#' feature of x unioned with first feature of y and so on.
#'
#' @inheritParams sf::st_union
#' @param cbind If `TRUE`, bind the attributes for y to x assuming y is the same
#'   length as x.
#' @keywords internal
#' @importFrom sf st_crs st_geometry st_union st_set_geometry st_drop_geometry
#' @importFrom vctrs vec_recycle_common vec_cbind
vec_st_union <- function(x, y, cbind = FALSE, ...) {
  vec_st_fn(x, y, .fn = sf::st_union, cbind = cbind, ...)
}

vec_st_distance <- function(x, y, cbind = FALSE, ...) {
  vec_st_fn(x, y, .fn = sf::st_distance, cbind = cbind, ...)
}

#' @noRd
vec_st_fn <- function(x, y, .fn, cbind = FALSE, ...) {
  crs <- sf::st_crs(x)
  y <- st_transform_ext(y, crs = crs)

  params <- vctrs::vec_recycle_common(
    x = st_geometry_ext(x),
    y = st_geometry_ext(y)
  )

  geometry <- as_sfc(
    lapply(
      seq_along(params[["x"]]),
      function(i) {
        .fn(
          params[["x"]][[i]],
          params[["y"]][[i]],
        )
      }
    ),
    crs = crs
  )

  if (!is_sf(x)) {
    return(geometry)
  }

  x <- sf::st_set_geometry(x, geometry)

  if (!cbind || !is_sf(y)) {
    return(x)
  }

  sf_list_cbind(list(x), list(sf::st_drop_geometry(y)), ...)
}

#' @noRd
st_geometry_ext <- function(x, ...) {
  if (is_sfc(x)) {
    return(x)
  }

  sf::st_geometry(x, ...)
}
