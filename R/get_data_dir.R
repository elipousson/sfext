#' Check if data directory exists and create a new directory if needed
#'
#' Get the path for a package-specific cache directory with
#' [rappdirs::user_cache_dir], check for the existence of a data directory,
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
#' @param null.ok If `TRUE`, path is `NULL`, cache is `FALSE`, return the `NULL`
#'   path value; defaults to `TRUE`.
#' @export
get_data_dir <- function(path = NULL,
                         cache = FALSE,
                         create = TRUE,
                         pkg = "sfext",
                         null.ok = TRUE) {
  if (cache) {
    is_pkg_installed("rappdirs")

    path <- path %||% rappdirs::user_cache_dir(pkg)
  }

  if (!is.null(path) && dir.exists(path)) {
    return(path)
  }

  if (is.null(path) && null.ok) {
    return(path)
  } else if (is.null(path)) {
    cli_abort("{.arg path} can't be {.val NULL} when {.code null.ok = FALSE}")
  }

  if (!create) {
    cli_warn("The provided {.arg path} {.file {path}} does not exist.")
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
