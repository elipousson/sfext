#' Modify strings to help make consistent file names
#'
#' Prefix and postfix can include more than one value that are added in the same
#' order provided. For [str_fix], the string must be a single character string.
#'
#' Functions include:
#'
#' - [str_fix]: Add a label, prefix, and postfix to string
#' - [str_prefix]: Add a prefix or a postfix to a string
#' - [str_add_filetype]: Add file type to string
#' - [str_remove_filetype]: Remove file type from string
#' - [str_extract_filetype]: Extract file type from string
#' - [str_pad_digits]: Pad a string with digits
#' - [str_extract_digits]: Extract digits from a string
#'
#' @name str_misc
NULL

#' @param prefix Character string or character vector to add to string parameter
#'   as a prefix.
#' @param string A single string that the attach prefix or postfix is added to.
#' @param postfix Character string or character vector to add to string
#'   parameter as a postfix.
#' @param sep Separator character passed as the collapse parameter of [paste()].
#' @param clean_names If `TRUE`, prefix, postfix, and string are all converted
#'   to snake case with [janitor::make_clean_names()].
#' @inheritParams str_pad_digits
#' @name str_fix
#' @rdname str_misc
#' @export
#' @importFrom janitor make_clean_names
str_fix <- function(prefix = NULL, string = NULL, postfix = NULL, sep = "_", clean_names = TRUE, pad = NULL, width = NULL) {
  stopifnot(
    is.character(prefix) || is.null(prefix),
    is.character(string) || is.null(string),
    is.character(postfix) || is.null(postfix)
  )

  string <- str_pad_digits(string, pad = pad, width = width)

  if (clean_names) {
    string <- janitor::make_clean_names(string)
  }

  string <- str_prefix(string, prefix = prefix, sep = sep)

  string <- str_prefix(string, prefix = postfix, sep = sep, post = TRUE)

  string <- gsub("_{2}", "_", string)

  return(string)
}

#' @name str_prefix
#' @rdname str_misc
#' @param post If `TRUE`, use the prefix string as a postfix; defaults to
#'   `FALSE`.
#' @param date.format,time.format Date or time format Only used by [str_prefix] if
#'   prefix is "date" or "time" and not currently accessible when using
#'   [str_fix] or [make_filename].
#' @export
#' @importFrom janitor make_clean_names
str_prefix <- function(string = NULL,
                       prefix = NULL,
                       sep = "_",
                       clean_names = TRUE,
                       post = FALSE,
                       date.format = "%F",
                       time.format = "%Y-%m-%d_%I-%M-%S_%p") {
  if (is.null(prefix)) {
    return(string)
  }

  if (clean_names) {
    prefix <- janitor::make_clean_names(prefix)
  }

  if (prefix %in% c("date", "time")) {
    prefix <- switch(prefix,
      "date" = format(Sys.Date(), date.format),
      "time" = format(Sys.time(), time.format)
    )
  }

  if (!post) {
    return(paste(c(prefix, string), collapse = sep))
  }

  paste(c(string, prefix), collapse = sep)
}


#' @param pad Single padding character added to digits in string; defaults to
#'   "0"
#' @inheritParams stringr::str_pad
#' @name str_pad_digits
#' @rdname str_misc
#' @export
#' @importFrom stringr str_pad str_replace
str_pad_digits <- function(string, pad = "0", side = "left", width = NULL) {
  if (is.null(pad)) {
    return(string)
  }

  digit_string <-
    str_extract_digits(string)

  if (is.null(width)) {
    width <-
      max(nchar(digit_string))
  }

  digit_string <-
    stringr::str_pad(
      string = digit_string,
      pad = pad,
      width = width
    )

  stringr::str_replace(
    string = string,
    pattern = "[:digit:]+",
    replacement = digit_string
  )
}

#' @name str_extract_digits
#' @rdname str_misc
#' @export
str_extract_digits <- function(string) {
  regmatches(string, regexpr("[0-9]+", string, perl = TRUE))
}

#' @name str_add_filetype
#' @rdname str_misc
#' @param filetype File type string
#' @export
str_add_filetype <- function(string, filetype = NULL) {
  if (grepl(pattern = "\\.[a-zA-Z0-9]+$", x = string)) {
    return(string)
  }

  paste0(string, ".", filetype)
}

#' @name str_remove_filetype
#' @rdname str_misc
#' @export
str_remove_filetype <- function(string, filetype = NULL) {
  if (!is.null(filetype)) {
    filetype <- str_extract_filetype(string)
  }

  sub(paste0("\\.", filetype, "$"), "", string)
}

#' @name str_extract_filetype
#' @rdname str_misc
#' @export
str_extract_filetype <- function(string) {
  tolower(regmatches(string, regexpr("(?<=\\.)[a-zA-Z0-9]+$(?!\\.)", string, perl = TRUE)))
}


#' @noRd
str_add_filetype <- function(string, filetype = NULL) {
  if (grepl(pattern = "\\.[a-zA-Z0-9]+$", x = string)) {
    return(string)
  }

  paste0(string, ".", filetype)
}

#' Replace spaces with underscores
#'
#' Not exported by overedge
#'
#' @noRd
underscore <- function(x) {
  gsub(" ", "_", x)
}
