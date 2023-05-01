# @staticimports pkg:stringstatic
#  str_detect str_extract
#  str_replace str_remove regex

# @staticimports pkg:isstatic
#  is_any is_all has_same_len is_any_in is_url is_esri_url is_gsheet_url
#  is_gist_url is_gmap_url is_unit is_rds_fileext is_rda_fileext
#  is_rdata_fileext is_excel_fileext is_csv_fileext is_geojson_fileext is_units
#  has_fileext int_to_alpha as_numbered_labels str_add_fileext
#  str_remove_fileext str_extract_fileext has_min_length

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

#' Set names using vctrs::vec_as_names
#'
#' @noRd
#' @importFrom vctrs vec_as_names
set_names_repair <- function(data = NULL,
                             nm = NULL,
                             .name_repair = "check_unique",
                             repair_arg = ".name_repair",
                             quiet = FALSE,
                             call = caller_env()) {
  check_character(nm, allow_null = TRUE, call = call)

  nm <- nm %||% names(data)

  if (is_null(nm)) {
    return(data)
  }

  if (!is_null(.name_repair)) {
    nm <- vctrs::vec_as_names(
      nm,
      repair = .name_repair,
      quiet = quiet,
      repair_arg = repair_arg
    )
  }

  set_names(data, nm = nm)
}

#' Set snakecase-ish names (simple stand-in for janitor::clean_names)
#'
#' @noRd
set_snakecaseish_names <- function(x, .name_repair = NULL, ..., call = caller_env()) {
  set_names_repair(x, snakecaseish(names(x)), .name_repair = .name_repair, call = call)
}

#'
#' @noRd
underscore <- function(x) {
  gsub("[[:blank:]]", "_", x, perl = TRUE)
}

#' Make snakecase-ish names (simple stand-in for janitor::make_clean_names)
#'
#' @noRd
snakecaseish <- function(x,
                         replace_blank = "_",
                         remove_punct = TRUE,
                         lower = TRUE) {
  if (!is.character(replace_blank)) {
    x <- gsub("[[:blank:]]", replace_blank, x, perl = TRUE)
  }

  if (remove_punct) {
    x <- gsub("[[:punct:]]", "", x, perl = TRUE)
  }

  if (lower) {
    x <- tolower(x)
  }

  x
}

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
group_by_col <- function(data, col = NULL, allow_null = TRUE, call = caller_env()) {
  check_required(data, call = call)

  if (allow_null && is_null(col)) {
    return(data)
  }

  check_name(col, call = call)
  check_data_frame(data, call = call)

  if ((has_length(col, 1)) && has_name(data, col)) {
    return(dplyr::group_by(data, .data[[col]]))
  }
}

#' @noRd
sf_to_sfc_list <- function(data) {
  lapply(
    do.call(
      "mapply",
      c(
        FUN = list,
        data,
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )
    ),
    function(x) {
      sf::st_sfc(x$geometry, crs = sf::st_crs(data))
    }
  )
}


#' @keywords internal
#' @importFrom rlang zap current_env
#' @importFrom vctrs vec_rbind
list_rbind <- function(x, names_to = zap(), ptype = NULL) {
  vctrs::vec_rbind(
    !!!x,
    .names_to = names_to,
    .ptype = ptype,
    .error_call = current_env()
  )
}

#' @keywords internal
#' @importFrom sf st_as_sf
list_rbind_as_sf <- function(x, ...) {
  sf::st_as_sf(list_rbind(x, ...))
}

#' @keywords internal
map_sf <- function(x, .f, ...) {
  list_rbind_as_sf(map(x, .f, ...))
}

#' @keywords internal
#' @importFrom rlang zap current_env
#' @importFrom vctrs vec_cbind
list_cbind <- function(x,
                       name_repair = c("unique", "universal", "check_unique"),
                       size = NULL) {
  vctrs::vec_cbind(
    !!!x,
    .name_repair = name_repair,
    .size = size,
    .error_call = current_env()
  )
}

#' Does the data frame has a column with the same name?
#'
#' @name has_same_name_col
#' @noRd
#' @importFrom dplyr select all_of rename
#' @importFrom cliExtras cli_yesno
has_same_name_col <- function(x,
                              col = NULL,
                              prefix = "orig",
                              ask = FALSE,
                              quiet = FALSE,
                              drop = TRUE,
                              call = caller_env()) {
  if (!has_name(x, col)) {
    return(x)
  }

  check_name(col, call = call)

  cli_quiet(quiet)

  if (drop) {
    x[[col]] <- NULL
    return(x)
  }

  new_col <- paste0(prefix, "_", col)

  if (ask && !quiet) {
    if (!cli_yesno("The provided data includes an existing column named '{col}'.
                   Do you want to proceed and rename this column to {new_col}?")) {
      cli_abort("Please rename your column to use this function.", call = call)
    }
  }

  cli::cli_alert_success(
    "The existing column '{col}' to '{new_col}' to avoid overwriting any existing values."
  )

  dplyr::rename(x, "{new_col}" := col)
}
