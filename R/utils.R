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

utils::globalVariables(
  c( # Variables for format_md_crash_data
    "filename", "image_description", "image_height", "image_width", "latitude", "latitude_ref",
    "longitude", "longitude_ref", "name", "orientation", "source_file"
  )
)

#' Eval and parse data
#'
#' @noRd
use_eval_parse <- function(data, package = NULL) {
  data <- paste0(collapse = "::", c(package, data))
  eval(parse(text = data))
}

#' Group data by selected column if present
#'
#' @param data Data frame or simple feature object
#' @param col Column name/value
#' @noRd
#' @importFrom dplyr group_by
group_by_col <- function(data, col = NULL) {
  if (is.null(col) || is.null(data)) {
    return(data)
  }

  if ((has_length(col, 1)) && has_name(data, col)) {
    return(dplyr::group_by(data, .data[[col]]))
  }
}

#' Does the data frame has a column with the same name?
#'
#' @name has_same_name_col
#' @noRd
#' @importFrom dplyr select all_of rename
has_same_name_col <- function(x, col = NULL, prefix = "orig", ask = FALSE, quiet = FALSE, drop = TRUE) {
  if (!has_name(x, col)) {
    return(x)
  }

  if (drop) {
    return(dplyr::select(x, -dplyr::all_of(col)))
  }

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

  dplyr::rename(x, "{new_col}" := col)
}
