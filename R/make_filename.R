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
#' @inheritParams str_pad_digits
#' @param postfix File name postfix; defaults to `NULL`.
#' @param cache If `TRUE`, path is set to the package cache directory using
#'   [get_data_dir()]; defaults to `FALSE`.
#' @inheritParams get_data_dir
#' @family read_write
#' @export
make_filename <- function(name = NULL,
                          label = NULL,
                          filetype = NULL,
                          filename = NULL,
                          path = NULL,
                          prefix = NULL,
                          postfix = NULL,
                          pad = NULL,
                          width = NULL,
                          cache = FALSE,
                          pkg = "sfext",
                          create = TRUE) {
  cli_abort_ifnot(
    "{.arg name} or {.arg filename} must be provided.",
    condition = is.character(name) | is.character(filename)
  )

  # If filename is provided, remove file type (if filename includes the filetype)
  if (!is.null(filename)) {
    filetype <- filetype %||% str_extract_filetype(filename)
    filename <- str_remove_filetype(filename)
  }

  # If file name is not provided, filename is based on label, name, pad and width
  if (!is.null(name)) {
    cli_warn_ifnot(
      "The provided {.arg filename} can't be used
      if {.arg name} is also provided.",
      condition = is.null(filename)
    )

    filename <-
      str_fix(
        prefix = label,
        string = name,
        clean_names = TRUE,
        pad = pad,
        width = width,
        col_names = FALSE
      )
  }

  # Apply prefix and postfix to the filename
  filename <-
    str_fix(
      prefix = prefix,
      string = filename,
      postfix = postfix,
      clean_names = TRUE,
      pad = NULL,
      col_names = FALSE
    )

  # Append filetype
  filename <-
    str_add_filetype(
      filename,
      filetype
    )

  path <-
    get_data_dir(
      path = path,
      cache = cache,
      create = create,
      pkg = pkg,
      null.ok = TRUE
    )

  if (is.null(path)) {
    return(filename)
  }

  file.path(path, filename)
}
