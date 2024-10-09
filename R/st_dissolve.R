#' Safely use spdep::poly2nb and spdep::n.comp.nb to get an index of neighboring
#' features
#'
#' [spdep::poly2nb()] errors if no neighboring features exist. This wrapper
#' function returns a vector of the same length as x with the integer 0 for any
#' input object that typically errors. Adapted from code by Josiah Parry
#' inspired by work from CGMossa.
#'
#' @source <https://github.com/r-spatial/sf/issues/2422#issue-2459271220>
#' @inheritParams x A sf or sfc object
#' @param quiet If `TRUE` (default), suppress warnings from [spdep::poly2nb()].
#' @inheritParams spdep::poly2nb
#' @inheritDotParams spdep::poly2nb
#' @noRd
poly_2_ncomp_id <- function(x,
                            snap = NULL,
                            queen = TRUE,
                            quiet = TRUE,
                            ...) {
  rlang::check_installed("spdep")
  fn <- invisible
  if (quiet) {
    fn <- suppressWarnings
  }

  rlang::try_fetch(
    fn({
      nb <- spdep::poly2nb(x, snap = snap, queen = queen, ...)
      attr(nb, "ncomp")[["comp.id"]]
    }),
    error = \(cnd) {
      rep_len(0, length(x))
    }
  )
}

#' Dissolve geometry preserving existing or supplied grouping variables
#'
#' [st_dissolve()] dissolves sf and sfc objects. If the input object is an sf
#' object, any existing grouping variables or added grouping variables passed to
#' the `.by` parameter are included in the output sf data frame.
#'
#' @param x A sf or sfc object. If sf object, a grouped data frame is allowed.
#' @inheritParams dplyr::mutate
#' @param .keep Method for handling attributes for input data. By default
#'   ("nest"), input data is converted to a nested list column with the name
#'   from `.data_key`. Any other values
#' @param do_union Use [sf::st_union()] to dissolve geometry if `TRUE`
#'   (default). Use [sf::st_combine()] if `FALSE`.
#' @param .data_key Passed to `.data` argument of [tidyr::nest()]
#' @param .dissolve_key Used as the column name for the dissolve grouping
#'   variable. x can't have any existing columns with this name.
#' @inheritParams check_sf
#' @inheritDotParams spdep::poly2nb
#' @export
st_dissolve <- function(x,
                        ...,
                        .by = NULL,
                        .keep = "nest",
                        do_union = TRUE,
                        .data_key = "data",
                        .dissolve_key = "group.comp.id",
                        call = caller_env) {
  .check_sfish(x, allow_class = c("sfc"), call = call)
  check_string(.dissolve_key, call = call)
  check_string(.data_key, call = call)
  check_string(.keep, allow_null = TRUE, call = call)
  check_logical(do_union, call = call)

  stopifnot(
    !rlang::has_name(x, .dissolve_key)
  )

  x_is_sfc <- FALSE

  if (inherits(x, "sfc")) {
    x_is_sfc <- TRUE
    x <- sf::st_set_geometry(sf::st_as_sf(x), "geometry")
  }


  x_group_vars <- NULL

  # Handle tidyselect style .by arguments
  by <- rlang::enquo(.by)
  if (!rlang::quo_is_null(by)) {
    x <- dplyr::group_by(x, dplyr::across(!!by))
  }

  if (dplyr::is_grouped_df(x)) {
    x_group_vars <- dplyr::group_vars(x)
  }

  sf_column_nm <- attr(x, "sf_column")

  # Create dissolve grouping variable with poly_2_nb_id
  x <- x |>
    dplyr::mutate(
      "{.dissolve_key}" := paste0(
        dplyr::cur_group_id(), ".",
        poly_2_ncomp_id(.data[[sf_column_nm]], ...)
      )
    )

  # Drop grouping after mutate
  if (dplyr::is_grouped_df(x)) {
    x <- dplyr::ungroup(x)
  }

  # Use st_combine or st_union (if `do_union = TRUE`)
  sf_summarise_fn <- sf::st_combine
  if (do_union) {
    sf_summarise_fn <- sf::st_union
  }

  x_dissolve <- x |>
    dplyr::summarise(
      # Keep unique values for grouping variables (if supplied)
      dplyr::across(
        tidyselect::any_of(x_group_vars),
        unique
      ),
      # Combine geometry with sf summary function
      dplyr::across(
        tidyselect::all_of(sf_column_nm),
        sf_summarise_fn
      ),
      .by = tidyselect::all_of(.dissolve_key)
    )

  if (x_is_sfc) {
    return(sf::st_geometry(x_dissolve))
  }

  if (!is.null(.keep) && .keep != "nest") {
    return(x_dissolve)
  }

  check_installed("tidyr", call = call)

  # Move original columns into a nested list column
  x_orig <- x |>
    tidyr::nest(
      .by = tidyselect::all_of(.dissolve_key),
      .key = .data_key
    )

  # Join nested list column to dissolved sf object and move geometry to end
  x_dissolve |>
    dplyr::left_join(
      x_orig,
      by = .dissolve_key
    ) |>
    dplyr::relocate(
      dplyr::all_of(sf_column_nm),
      .after = tidyselect::everything()
    )
}
