#' @noRd
str_add_filetype <- function(string, filetype = NULL) {
  if (grepl(pattern = "\\.[a-zA-Z0-9]+$", x = string)) {
    return(string)
  }

  paste0(string, ".", filetype)
}

#' @noRd
str_remove_filetype <- function(string, filetype = NULL) {
  if (!is.null(filetype)) {
    filetype <- str_extract_filetype(string)
  }

  sub(
    pattern = paste0("\\.", filetype, "$"),
    replacement = "",
    x = string
  )
}

#' @noRd
str_extract_filetype <- function(string) {
  tolower(stringr::str_extract(string, "(?<=\\.)[:alnum:]+$(?!\\.)"))
}

#' Replace spaces with underscores
#'
#' Not exported by overedge
#'
#' @noRd
underscore <- function(x) {
  gsub(" ", "_", x)
}
