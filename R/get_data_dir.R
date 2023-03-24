#' Check if data directory exists and create a new directory if needed
#'
#' `r lifecycle::badge('superseded')`
#' Get the path for a package-specific cache directory with
#' [rappdirs::user_cache_dir()], check for the existence of a data directory,
#' optionally create a new directory at the provided path location.
#'
#' @param path Path to directory for use as data directory.
#' @param cache If `TRUE`, and path is `NULL` set path to
#'   [rappdirs::user_cache_dir] (using value of pkg as appname). If path is not
#'   `NULL`, the path is returned even if cache is `TRUE`.
#' @param create If `FALSE` and path does not exist, return path with a warning.
#'   If `TRUE` and [rlang::is_interactive] is `TRUE`, ask user if directory
#'   should be created. If the session not interactive and create is `TRUE`, a
#'   new directory will be created.
#' @param pkg Package name; defaults to "sfext"
#' @param allow_null If `TRUE`, path is `NULL`, cache is `FALSE`, return the `NULL`
#'   path value; defaults to `TRUE`.
#' @export
get_data_dir <- function(path = NULL,
                         cache = FALSE,
                         create = TRUE,
                         pkg = "sfext",
                         allow_null = TRUE) {
  lifecycle::signal_stage("superseded", "get_data_dir()", "filenamr::get_data_dir()")

  if (cache) {
    rlang::check_installed("rappdirs")
    path <- path %||% rappdirs::user_cache_dir(pkg)
  }

  if (!is.null(path) && dir.exists(path)) {
    return(path)
  } else if (is.null(path)) {
    if (allow_null) {
      return(invisible(path))
    }

    cli_abort("{.arg path} can't be {.val NULL} when {.code allow_null = FALSE}")
  }

  if (!create) {
    cli_warn("The provided {.arg path} {.file {path}} does not exist.")
    return(path)
  }

  if (is_interactive()) {
    create <-
      cli_yesno(
        c(
          "x" = "The directory {.file {path}} does not exist.",
          ">" = "Do you want to create a directory at this location?"
        )
      )
  }

  if (create) {
    dir.create(path)
    cli_inform(c("v" = "New directory created at {.file {path}}"))
  }
}


#' @name list_data_files
#' @param fileext If pattern is NULL, fileext is used to set the pattern and
#'   filter listed files to those matching the file extension.
#' @inheritParams base::list.files
#' @param ... Additional parameters passed to [list.files()]
#' @rdname get_data_dir
#' @export
list_data_files <- function(path = NULL,
                            pkg = "sfext",
                            cache = FALSE,
                            fileext = NULL,
                            pattern = NULL,
                            full.names = TRUE,
                            ignore.case = TRUE,
                            ...) {
  path <-
    filenamr::get_data_dir(
      path = path,
      cache = cache,
      pkg = pkg,
      allow_null = FALSE,
      create = FALSE
    )

  if (!is.null(fileext)) {
    pattern <- pattern %||% paste0(fileext, "$")
  }

  list.files(
    path,
    pattern = pattern,
    full.names = full.names,
    ignore.case = ignore.case,
    ...
  )
}


#' Get file types from a path
#'
#' @param path A valid directory or file path.
#' @param filetype If not `NULL`, function returns file type as is.
#' @param n Max number of unique file types to return. Returns warning and n
#'   most common file types if path has more than n unique file types.
#' @noRd
get_path_filetype <- function(path, filetype = NULL, n = 1) {
  lifecycle::signal_stage("superseded", "get_path_filetype()", "filenamr::get_path_fileext()")

  if (!is.null(filetype)) {
    return(filetype)
  }

  if (dir.exists(path)) {
    file_list <- list.files(path)
  } else if (file.exists(path)) {
    file_list <- path
  } else {
    cli_abort(
      c("A valid file or directory {.arg path} must be provided.",
        "i" = "The provided {.arg path} {.file {path}} does not exist."
      )
    )
  }

  filetype <- str_extract_fileext(file_list)

  if (length(unique(filetype)) <= n) {
    return(unique(filetype))
  }

  # https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
  filetype <- names(sort(table(filetype), decreasing = TRUE)[1:n])

  cli_warn(
    c("The directory {.file {path}} has more than {n} unique filetypes.",
      "i" = "Using {n} most frequent filetype{?s}: {.val {filetype}}"
    )
  )

  filetype
}

#' Get list of files at a path (using a single file type at a time)
#'
#' @noRd
get_path_files <- function(path, filetype = NULL, full.names = TRUE) {
  if (is.data.frame(path) && rlang::has_name(path, "path")) {
    path <- path[["path"]]
  }

  if (all(dir.exists(path))) {
    return(
      list.files(
        path = path,
        pattern = glue("\\.{get_path_filetype(path, filetype)}$"),
        full.names = full.names
      )
    )
  } else if (all(file.exists(path))) {
    return(path)
  }

  cli_abort(
    c("A valid file or directory {.arg path} must be provided.",
      "i" = "The provided {.arg path} {.file {path}} does not exist."
    )
  )
}
