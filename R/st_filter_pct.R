#' Filter by share of length or area of one geometry overlapping with a second
#' geometry
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param x A sf object to filter.
#' @param y A sf or sfc object to filter by.
#' @param pct Percent of length or area to use as a threshold value for filter.
#'   Numeric value of 1 or less.
#' @param ... Additional parameters. Not used currently.
#' @return A filtered version of the input sf object.
#' @rdname st_filter_pct
#' @export
#' @importFrom dplyr case_when
st_filter_pct <- function(x, y, pct = NULL, ...) {
  dplyr::case_when(
    is_line(x) | is_multiline(x) ~ st_filter_pct_length(x, y, ...),
    is_polygon(x) | is_multipolygon(x) ~ st_filter_pct_area(x, y, ...),
  )
}


#' @name st_filter_pct_area
#' @rdname st_filter_pct
#' @export
#' @importFrom cliExtras cli_abort_ifnot
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr filter left_join mutate select
st_filter_pct_area <- function(x, y, pct = NULL) {
  cliExtras::cli_abort_ifnot(
    "{.arg y} must be a POLYGON or MULTIPOLYGON object." = is_polygon(x) | is_multipolygon(x)
  )

  x <- get_area(x, .id = "init_area", drop = TRUE)

  x$trim_join_id <- c(1:nrow(x))

  x_trim <- st_trim(x, y)

  x_trim <-
    sf::st_drop_geometry(get_length(x_trim, .id = "trim_area", drop = TRUE))

  x <- dplyr::filter(dplyr::left_join(x, x_trim), !is.na(trim_area))

  x <- dplyr::mutate(
    x,
    pct_area = trim_area / init_area
  )

  x <-
    dplyr::select(
      x,
      -dplyr::all_of(c("trim_join_id", "trim_area", "init_area"))
    )

  if (!is.null(pct)) {
    cliExtras::cli_abort_ifnot(
      "{.arg pct} must be a {.cls numeric} value of 1 or less." = is.numeric(pct) & (pct <= 1)
    )
    x <- dplyr::filter(x, pct_area >= pct)
  }

  x
}

#' @name st_filter_pct_length
#' @rdname st_filter_pct
#' @export
#' @importFrom cliExtras cli_abort_ifnot
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr filter left_join mutate select
st_filter_pct_length <- function(x, y, pct = NULL) {
  cliExtras::cli_abort_ifnot(
    "{.arg y} must be a LINESTRING or MULTILINESTRING object." = is_line(x) | is_multiline(x)
  )

  x <- get_length(x, .id = "init_length", drop = TRUE)

  x$trim_join_id <- c(1:nrow(x))

  x_trim <- st_trim(x, y)

  x_trim <-
    sf::st_drop_geometry(get_length(x_trim, .id = "trim_length", drop = TRUE))

  x <- dplyr::filter(dplyr::left_join(x, x_trim), !is.na(trim_length))

  x <- dplyr::mutate(
    x,
    pct_length = trim_length / init_length
  )

  x <-
    dplyr::select(
      x,
      -c(trim_join_id, trim_length, init_length)
    )

  if (!is.null(pct)) {
    cliExtras::cli_abort_ifnot(
      "{.arg pct} must be a {.cls numeric} value of 1 or less." = is.numeric(pct) & (pct <= 1)
    )
    x <- dplyr::filter(x, pct_length >= pct)
  }

  x
}
