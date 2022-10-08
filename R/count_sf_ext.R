#' Count extended for working with sf objects
#'
#' An extended version of [dplyr::count()] that makes it easier to count the
#' occurrences of features from data that intersect with features from a second
#' sf object (set by y) or created by passing x or data to [st_make_grid_ext()].
#' Similar to [count_features()] and the two functions may be combined in the
#' future.
#'
#' @inheritDotParams st_make_grid_ext -.id
#' @inheritParams sf::st_join
#' @inheritParams dplyr::count
#' @param data Data to count in relationship to y
#' @param x Optional sf object passed to [st_make_grid_ext()]. Defaults to `NULL`.
#' @param y If `NULL` (default), y defaults to an `sf` object created by [st_make_grid_ext()] using x or data (if x is
#'   `NULL`) as the x parameter for [st_make_grid_ext()]. If not `NULL`, y must be an `sf` object that
#'   has a column with the same name as .id (defaults to "id").
#' @param replace_na If `TRUE`, replace NA values from count with 0. If `FALSE`,
#'   filter NA values from the returned object.
#' @param count_lims Optional numeric vector with minimum or both minimum and
#'   maximum count values. If provided, any values below the minimum are set to
#'   that minimum and any values above the maximum as set to the maximum. If
#'   only one value is provided, it is assumed to be a minimum limit.
#' @param geometry If `TRUE` (default) return a `sf` object. If `FALSE`, return
#'   a data frame.
#'
#' @examples
#' nc <- sf::read_sf(path = system.file("shape/nc.shp", package = "sf"))
#' data <- sf::st_sample(nc, size = 75)
#'
#' # Count data based on nc
#' count <- count_sf_ext(data = data, y = nc, .id = "FIPS")
#' plot(count[, "n"], pal = heat.colors(n = max(count$n), alpha = 0.5), reset = FALSE)
#' plot(data, add = TRUE, pch = 20)
#'
#' # Count data based grid created by passing nc to st_make_grid_ext
#' count_grid <- count_sf_ext(data = data, x = nc, .id = "FIPS")
#' plot(count_grid[, "n"], pal = heat.colors(n = max(count_grid$n), alpha = 0.5), reset = FALSE)
#' plot(data, add = TRUE, pch = 20)
#'
#' @importFrom dplyr count right_join filter mutate case_when select
#' @importFrom sf st_join st_as_sf
#' @importFrom tidyr replace_na
count_sf_ext <- function(data,
                         x = NULL,
                         y = NULL,
                         join = sf::st_intersects,
                         largest = TRUE,
                         wt = NULL,
                         sort = FALSE,
                         replace_na = FALSE,
                         keep_na = FALSE,
                         lims = NULL,
                         geometry = TRUE,
                         .id = "id",
                         name = NULL,
                         ...) {
  data <- as_sf(data)

  if (rlang::has_name(data, .id)) {
    data <- dplyr::select(data, -{{ .id }})
  }

  y <-
    y %||%
    suppressWarnings(
      st_make_grid_ext(
        x = x %||% data,
        ...,
        .id = .id
      )
    )

  check_sf(y)

  cli_abort_ifnot(
    "{.arg .id} must be a length 1 character vector.",
    condition = is.character(.id) && (length(.id) == 1)
  )

  cli_abort_ifnot(
    "{.arg y} must have a column with named {.val {(.id)}} to match the {.arg .id} parameter.",
    condition = rlang::has_name(y, .id)
  )

  sf_col <- get_sf_col(y)

  data <-
    sf::st_join(
      data,
      y,
      suffix = c("_", ""),
      join = join,
      largest = largest
    )

  name <- name %||% "n"

  data_count <-
    dplyr::count(
      x = sf::st_drop_geometry(data),
      .data[[.id]],
      wt = {{ wt }}, # wt is a data-masking variable
      sort = sort,
      name = name
    )

  data_count <-
    dplyr::right_join(
      x = data_count,
      y = y,
      by = {
        .id
      }
    )

  if (replace_na) {
    data_count <- tidyr::replace_na(data_count, rlang::set_names(list(0), name))
  } else if (!keep_na) {
    data_count <- dplyr::filter(data_count, !is.na(.data[[name]]))
  }

  if (!is.null(lims)) {
    stopifnot(
      is.numeric(lims)
    )

    min_count <- as.integer(min(lims))

    if (length(lims) > 1) {
      max_count <- as.integer(max(lims))
      cli::cli_inform(
        "Make sure caption or legend indicates that all values at or below
        {.val {min_count}} or at or above {.val {max_count}} are binned."
      )
    } else {
      cli::cli_inform(
        "Make sure caption or legend indicates that all values at or below
        {.val {min_count}} are binned."
      )
      max_count <- max(data_count[[name]])
    }

    data_count <-
      dplyr::mutate(
        data_count,
        "{name}" := dplyr::case_when(
          .data[[name]] == 0 ~ .data[[name]],
          .data[[name]] > max_count ~ max_count,
          .data[[name]] < min_count ~ min_count,
          TRUE ~ .data[[name]]
        )
      )
  }

  if (geometry) {
    return(sf::st_as_sf(data_count))
  }

  dplyr::select(data_count, -.data[[sf_col]])
}
