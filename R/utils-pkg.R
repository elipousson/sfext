#' List data files from package
#'
#' @noRd
#' @importFrom utils data
ls_pkg_data <- function(pkg, envir = .GlobalEnv, call = caller_env()) {
  results <- utils::data(package = pkg, envir = envir)$results

  if (is_null(results) | is_empty(results)) {
    return(invisible(NULL))
  }

  results[, "Item"]
}

#' @noRd
is_pkg_data <- function(x, pkg, call = caller_env()) {
  ls <- ls_pkg_data(pkg, call = call)

  if (is.null(ls)) {
    return(FALSE)
  }

  x %in% ls
}

#' List data files in package extdata directory
#'
#' @noRd
ls_pkg_extdata <- function(pkg, call = caller_env()) {
  path <- system.file("extdata", package = pkg)

  if (is.null(path)) {
    return(invisible(path))
    # cli_abort("{.file extdata} folder can't be found for the provided package
    # {.val pkg}.")
  }

  list.files(path)
}

#' @noRd
is_pkg_extdata <- function(x, pkg, call = caller_env()) {
  ls <- ls_pkg_extdata(pkg, call = call)

  if (is.null(ls)) {
    return(FALSE)
  }

  x %in% ls
}

#' List data files in package cache directory
#'
#' @noRd
ls_pkg_cachedata <- function(pkg, call = caller_env()) {
  path <- filenamr::get_data_dir(cache = TRUE, create = FALSE, pkg = pkg)

  if (is.null(path)) {
    return(invisible(path))
    # cli_abort("{.file cache} folder can't be found for the provided package {.val pkg}.")
  }

  list.files(path)
}


#' @noRd
is_pkg_cachedata <- function(x, pkg, call = caller_env()) {
  ls <- ls_pkg_cachedata(pkg, call = call)

  if (is.null(ls)) {
    return(FALSE)
  }

  x %in% ls
}

#' Is this package installed?
#'
#' @param pkg Name of a package.
#' @param repo GitHub repository to use for the package.
#' @noRd
#' @importFrom rlang check_installed
check_dev_installed <- function(pkg = NULL, repo = NULL) {
  if (!is.null(pkg) && !rlang::is_installed(pkg = pkg)) {
    rlang::check_installed(pkg = repo %||% pkg)
  }
}
