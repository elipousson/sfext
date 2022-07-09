#' Check if data directory exists and create a new directory if needed
#'
#' A utility function that wraps [rappdirs::user_cache_dir].
#'
#' @param path Path to directory to use as data directory.
#' @param cache If `TRUE`, replace a NULL path with [rappdirs::user_cache_dir]
#'   (using value of package as appname).
#' @param create If `FALSE` and path does not exist, return path with a warning.
#'   If `TRUE` and interactive, ask user if directory should be created. If not
#'   interactive and create is `TRUE`, a new directory will be created.
#' @param package Package name; defaults to "sfext"
#' @export
get_data_dir <- function(path = NULL, cache = FALSE, create = TRUE, package = "sfext", null.ok = TRUE) {
  if (cache) {
    is_pkg_installed("rappdirs")

    path <- rappdirs::user_cache_dir(package)
  }

  if (is.null(path) && null.ok) {
    return(path)
  }

  if (dir.exists(path)) {
    return(path)
  }

  if (!create) {
    cli_warn(
      "The provided {.arg path} {.file {path}} does not exist."
    )
    return(path)
  }

  if (is_interactive()) {
    create <-
      cli_yeah(
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
