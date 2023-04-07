#' Count extended for working with sf objects
#'
#' An extended version of [dplyr::count()] that makes it easier to count the
#' occurrences of features from data that intersect with features from a second
#' sf object (set by y) or created by passing x or data to [st_make_grid_ext()].
#' Similar to [count_features()] and the two functions may be combined in the
#' future.
#'
#' @inheritParams st_make_grid_ext
#' @inheritParams sf::st_join
#' @inheritParams dplyr::count
#' @param data Data to count in relationship to y
#' @param x Optional sf object passed to [st_make_grid_ext()]. Defaults to
#'   `NULL`.
#' @param y If `NULL` (default), y defaults to an `sf` object created by
#'   [st_make_grid_ext()] using x or data (if x is `NULL`) as the x parameter
#'   for [st_make_grid_ext()]. If not `NULL`, y must be an `sf` object that has
#'   a column with the same name as .id (defaults to "id").
#' @param replace_na If `TRUE`, replace NA values from count with 0.
#' @param keep_na If `TRUE`, filter NA values from count. Ignored if replace_na
#'   is `TRUE`.
#' @param lims Optional numeric vector with minimum or both minimum and
#'   maximum count values. If provided, any values below the minimum are set to
#'   that minimum and any values above the maximum as set to the maximum. If
#'   only one value is provided, it is assumed to be a minimum limit.
#' @param geometry If `TRUE` (default) return a `sf` object. If `FALSE`, return
#'   a data frame.
#' @inheritDotParams st_make_grid_ext
#'
#' @examples
#' nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
#' data <- sf::st_sample(nc, size = 75)
#'
#' # Count data based on nc
#' count <- count_sf_ext(data = data, y = nc, .id = "FIPS")
#' plot(count[, "n"], reset = FALSE)
#' plot(data, col = "gray60", pch = 3, add = TRUE)
#'
#' # Count data based grid created by passing nc to st_make_grid_ext
#' count_grid <- count_sf_ext(data = data, x = nc, .id = "FIPS")
#' plot(count_grid[, "n"], reset = FALSE)
#' plot(data, col = "gray60", pch = 3, add = TRUE)
#'
#' @returns A sf object or a tibble (if `geometry = FALSE`) with a column
#'   counting occurrences of features from data.
#' @export
#' @importFrom dplyr count right_join filter mutate case_when select
#' @importFrom sf st_join st_as_sf
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
    data <- dplyr::select(data, -dplyr::all_of(.id))
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
    check_installed("tidyr")
    data_count <- tidyr::replace_na(data_count, rlang::set_names(list(0), name))
  } else if (!keep_na) {
    data_count <- dplyr::filter(data_count, !is.na(.data[[name]]))
  }

  if (!is.null(lims)) {
    stopifnot(
      is.numeric(lims)
    )

    min_count <- as.integer(min(lims))
    n_lims <- length(lims)

    if (n_lims > 1) {
      max_count <- as.integer(max(lims))
      cli::cli_inform(
        "Make sure caption or legend indicates that all values at or below
        {.val {min_count}} or at or above {.val {max_count}} are binned."
      )
    }

    if (n_lims == 1) {
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
