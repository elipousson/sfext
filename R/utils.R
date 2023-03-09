# @staticimports pkg:stringstatic
#  str_detect str_extract
#  str_replace str_remove regex

# @staticimports pkg:isstatic
#  is_any is_all has_same_len is_any_in is_url is_esri_url is_gsheet_url
#  is_gist_url is_gmap_url is_unit is_rds_fileext is_rda_fileext
#  is_rdata_fileext is_excel_fileext is_csv_fileext is_geojson_fileext is_units
#  has_fileext int_to_alpha as_numbered_labels str_add_fileext
#  str_remove_fileext str_extract_fileext

.onLoad <- function(lib, pkg) {
  utils::data(
    list = c(
      "dist_units",
      "dist_unit_options", "area_unit_options",
      "standard_scales", "paper_sizes"
    ),
    package = pkg,
    envir = parent.env(environment())
  )
}

utils::globalVariables(
  c(
    "filename", "image_description", "image_height", "image_width", "latitude",
    "latitude_ref", "longitude", "longitude_ref", "name", "orientation",
    "source_file", "asp", "block_height", "block_width", "col_width", "gutter",
    "height", "row_height", "width", "img_cardinal_dir", "img_direction",
    "path", "x_nudge", "y_nudge", "trim_area", "init_area", "pct_area",
    "trim_length", "init_length", "trim_join_id", "pct_length"
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
#' @importFrom cliExtras cli_yesno
has_same_name_col <- function(x, col = NULL, prefix = "orig", ask = FALSE, quiet = FALSE, drop = TRUE) {
  if (!has_name(x, col)) {
    return(x)
  }

  if (drop) {
    return(dplyr::select(x, -dplyr::all_of(col)))
  }

  new_col <- paste0(prefix, "_", col)

  if (ask && !quiet) {
    if (!cli_yesno("The provided data includes an existing column named '{col}'.
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
