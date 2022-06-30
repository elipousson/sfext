.onLoad <- function(lib, pkg) {
  run_on_load()

  utils::data(
    list = c(
      "dist_units",
      "dist_unit_options", "area_unit_options",
      "standard_scales"
    ),
    package = pkg,
    envir = parent.env(environment())
  )
}

#' @noRd
underscore <- function(x) {
  gsub(" ", "_", x)
}

#' Does the data frame has a column with the same name?
#'
#' @name has_same_name_col
#' @noRd
#' @importFrom dplyr rename select
has_same_name_col <- function(x, col = NULL, prefix = "orig", ask = FALSE, quiet = FALSE, drop = TRUE) {
  if (has_name(x, col) && !drop) {
    new_col <- paste0(prefix, "_", col)

    if (ask && !quiet) {
      if (!cli_yeah("The provided data includes an existing column named '{col}'.
                   Do you want to proceed and rename this column to {new_col}?")) {
        cli_abort("Please rename your column to use this function.")
      }
    }

    if (!quiet) {
      cli_inform(
       c("v" = "The existing column '{col}' to '{new_col}' to avoid overwriting any existing values.")
      )
    }

    x <-
      dplyr::rename(
        x,
        "{new_col}" := col
      )
  } else if (has_name(x, col) && drop) {
    x <-
      dplyr::select(
        x,
        -dplyr::all_of(col)
      )
  }

  x
}
