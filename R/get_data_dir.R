
#' Check data directory and create if needed
#'
#' A utility function that wraps [rappdirs::user_cache_dir].
#'
#' @param path Path to directory to use as data directory.
#' @param package Package name; defaults to "sfext"
#' @export
get_data_dir <- function(path = NULL, package = "sfext") {
  if (is.null(path)) {
    is_pkg_installed("rappdirs")

    path <- rappdirs::user_cache_dir(package)
  }

  create_data_dir(path, create = TRUE)

  path
}

#' @rdname get_data_dir
#' @name create_data_dir
#' @param create For [create_data_dir], if `TRUE` and directory does not exist
#'   at the path, prompt user to create directory. Defaults to `TRUE`.
#' @export
create_data_dir <- function(path = NULL, create = TRUE) {
  if (!dir.exists(path) && create) {
    if (cli_yeah(
      "The directory {.file {path}} does not exist.
    Do you want to create a directory at this location?"
    )) {
      dir.create(path = path)
      cli_inform(c("v" = "New directory created at {.file {path}}"))
    } else {
      cli_abort("Please provide a different path for this file.")
    }
  }
}
