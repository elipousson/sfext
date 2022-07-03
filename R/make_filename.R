
#' Make file name and path with optional label, prefix, or postfix
#'
#' A helper function to create consistent file names for plots created with data
#' files exported with [write_sf_ext()].
#'
#' @param name Name to make file name converted to snake case with
#'   [janitor::make_clean_names()], e.g. "Residential zoning map" becomes
#'   "residential_zoning_map"
#' @param label Label to combine with name converted to snake case with
#'   [janitor::make_clean_names()]. The label is designed to identify the area
#'   or other shared characteristics across multiple data files, maps, or plots.
#' @param filetype File type or extension.
#' @param filename File name; if file name is `NULL`, name and file type are
#'   both required.
#' @param path Path to file or data directory.
#' @param prefix File name prefix. "date" adds a date prefix, "time" adds a
#'   date/time prefix; defaults to `NULL`.
#' @param postfix File name postfix; defaults to `NULL`.
#' @param cache If TRUE, path is set to the package cache directory using
#'   [get_data_dir()]; defaults to `FALSE`.
#' @inheritParams str_pad_digits
#' @family read_write
#' @export
make_filename <- function(name = NULL,
                          label = NULL,
                          filetype = NULL,
                          filename = NULL,
                          path = NULL,
                          prefix = NULL,
                          postfix = NULL,
                          cache = FALSE,
                          pad = NULL,
                          width = NULL) {
  stopifnot(
    is.character(name) || is.character(filename)
  )

  if (cache) {
    path <- get_data_dir(path = path)
  } else if (!is.null(path)) {
    path <- create_data_dir(path, create = TRUE)
  }

  # If file name is provided, remove file type
  if (!is.null(filename)) {
    if (is.null(filetype)) {
      filetype <- str_extract_filetype(filename)
    }

    filename <- str_remove_filetype(filename, filetype)
  }

  # If file name is not provided, file name is based on label, name, pad and width
  if (is.null(filename)) {
    filename <-
      str_fix(
        prefix = label,
        string = name,
        clean_names = TRUE,
        pad = pad,
        width = width
      )
  }

  # Apply prefix and postfix to the filename
  filename <-
    str_fix(
      prefix = prefix,
      string = filename,
      postfix = postfix,
      clean_names = TRUE,
      pad = NULL
    )

  filename <-
    str_add_filetype(
      filename,
      filetype
    )

  if (is.null(path)) {
    return(filename)
  }

  file.path(path, filename)
}
