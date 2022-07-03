#' List data files from package
#'
#' @noRd
#' @importFrom utils data
ls_pkg_data <- function(pkg, envir = .GlobalEnv) {
  utils::data(package = pkg, envir = envir)$results[, "Item"]
}

#' List data files in package extdata directory
#'
#' @noRd
ls_pkg_extdata <- function(pkg) {
  list.files(system.file("extdata", package = pkg))
}

#' List data files in package cache directory
#'
#' @noRd
ls_pkg_cache <- function(pkg) {
  list.files(get_data_dir(path = NULL, package = pkg))
}

#' Is this package installed?
#'
#' @param pkg Name of a package.
#' @param repo GitHub repository to use for the package.
#' @importFrom rlang check_installed
#' @noRd
is_pkg_installed <- function(pkg, repo = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (!is.null(repo)) {
      pkg <- repo
    }

    check_installed(pkg = pkg)
  }
}
