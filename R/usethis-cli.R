#'
#' @noRd
cli_paths <- function(path,
                      ...,
                      call = caller_env(),
                      .envir = parent.frame()) {
  len_path <- length(path)

  cli_inform(
    c("v" = paste0(..., " {len_path} file{?s}:")),
    call = call,
    .envir = .envir
  )

  path <- paste0("{.file ", path, "}")
  cli::cli_bullets(rlang::set_names(path, rep("*", len_path)))
}
