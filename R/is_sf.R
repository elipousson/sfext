#' What is the class or spatial attributes of this feature?
#'
#' @param x An `sf`, `sfc`, or `bbox` object.
#' @param y An sf object or a character or numeric object supported by
#'   [sf::st_crs] that can be compared to x. (used by [is_same_crs])
#' @param ext If `TRUE`, check if x is a `sf`, `sfc`, or `bbox` class object or
#'   not; defaults to `FALSE`. (used by [is_sf])
#' @param null.ok If `TRUE` and x is `NULL`, return `TRUE`; defaults to `FALSE`.
#' @details
#' - [is_sf]: is x a `sf` class object?
#' - [is_sfc]: is x is a `sfc` class object?
#' - [is_bbox]: is x is a `bbox` class object?
#' - [is_sf_list]: is x is a list of `sf` class objects (with or without names)?
#' - [is_raster]: is x a `Raster` class object?
#' - [is_sp]: is x a `Spatial` class object of any type?
#' - [is_same_crs]: do x and y have the same coordinate reference system?
#'
#' @export
#' @md
is_sf <- function(x, ext = FALSE, null.ok = FALSE) {
  classes <- "sf"

  if (ext) {
    classes <- c(classes, "sfc", "bbox")
  }

  is_class(x, classes = classes, null.ok = null.ok)
}

#' @name is_sfg
#' @rdname is_sf
#' @export
is_sfg <- function(x, null.ok = FALSE) {
  is_class(x, classes = "sfg", null.ok = null.ok)
}

#' @name is_sfc
#' @rdname is_sf
#' @export
is_sfc <- function(x, null.ok = FALSE) {
  is_class(x, classes = "sfc", null.ok = null.ok)
}

#' @name is_bbox
#' @rdname is_sf
#' @export
is_bbox <- function(x, null.ok = FALSE) {
  is_class(x, classes = "bbox", null.ok = null.ok)
}

#' @rdname is_sf
#' @name is_sf_list
#' @param named If `TRUE`, check if sf list is named; defaults `FALSE`.
#' @export
#' @importFrom rlang is_named
is_sf_list <- function(x, named = FALSE, ext = FALSE, null.ok = FALSE) {
  if (is.null(x) && null.ok) {
    return(TRUE)
  }

  if (is_sf(x, ext = TRUE, null.ok = FALSE)) {
    return(FALSE)
  }

  is_sf_list <-
    is.list(x) && all(
      vapply(
        x,
        function(x) {
          is_sf(x, ext = ext, null.ok = null.ok)
        },
        TRUE
      )
    )

  if (!named) {
    return(is_sf_list)
  }

  is_sf_list && rlang::is_named(x)
}

#' @name is_raster
#' @rdname is_sf
#' @export
is_raster <- function(x, null.ok = FALSE) {
  is_class(x, classes = "RasterLayer", null.ok = null.ok)
}

#' @name is_sp
#' @rdname is_sf
#' @export
is_sp <- function(x, null.ok = FALSE) {
  if (is.null(x) && null.ok) {
    return(TRUE)
  }

  any(grepl("Spatial", class(x)))
}

#' @name is_same_crs
#' @rdname  is_sf
#' @importFrom sf st_crs
#' @export
is_same_crs <- function(x, y) {
  sf::st_crs(x) == sf::st_crs(y)
}

#' @name is_sf_or_what
#' @rdname  is_sf
#' @param return type of object to return; "list" or `NULL` (default).
#' @param us If `TRUE`, check if x is a U.S. state or county name; defaults to
#'   `FALSE`.
#' @importFrom cli cli_abort
is_sf_or_what <- function(x = NULL, return = NULL, us = FALSE, null.ok = TRUE) {
  # Is x a sf object, a sf/sfc/bbox object, a character string, or a state or county name/id?

  is_null <- is.null(x)

  if (!null.ok && is_null) {
    cli::cli_abort("is_sf_or_what found a NULL object when null.ok is FALSE")
  }

  type <-
    list("null" = is_null)

  if (!is_null) {
    type <- c(
      type,
      list(
        "sf" = is_sf(x),
        "sf_ext" = is_sf(x, ext = TRUE),
        "sf_list" = is_sf_list(x, named = FALSE),
        "nm_sf_list" = is_sf_list(x, named = TRUE),
        "nm_list" = rlang::is_named(x) && is.list(x),
        "list" = is.list(x) && (is.character(x) | is.numeric(x)),
        "chr" = is.character(x),
        "num" = is.numeric(x),
        "len" = length(x)
      )
    )

    if ((type$chr | type$num) && us) {
      type <- c(
        type,
        list(
          "state" = is_state_geoid(x) | is_state_name(x),
          "county" = is_county_geoid(x) | is_county_name(x)
        )
      )
    }
  }

  if (!is.null(return) && return == "list") {
    return(type)
  }

  return(
    dplyr::case_when(
      type$null ~ "null",
      type$nm_sf_list ~ "nm_sf_list",
      type$sf_list ~ "sf_list",
      type$nm_list ~ "nm_list",
      type$list ~ "list",
      type$chr ~ "chr",
      type$num ~ "num"
    )
  )
}

#' @noRd
is_state_name <- function(x, null.ok = TRUE) {
  if (is.null(x) && null.ok) {
    return(FALSE)
  }

  x %in% c(us_states$name, us_states$abb)
}

#' @noRd
is_state_geoid <- function(x, null.ok = TRUE) {
  if (is.null(x) && null.ok) {
    return(FALSE)
  }

  x %in% c(as.integer(us_states$statefp), us_states$statefp)
}

#' @noRd
is_county_name <- function(x, null.ok = TRUE) {
  if (is.null(x) && null.ok) {
    return(FALSE)
  }

  x %in% c(us_counties$name, us_counties$name_short)
}

#' @noRd
is_county_geoid <- function(x, null.ok = TRUE) {
  if (is.null(x) && null.ok) {
    return(FALSE)
  }

  x %in% c(as.integer(us_counties$geoid), us_counties$geoid)
}
