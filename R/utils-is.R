#' Is the class of this object any of the specified classes?
#'
#' @noRd
is_class <- function(x, classes = NULL, allow_null = FALSE) {
  if (is.null(x) && allow_null) {
    return(TRUE)
  }

  inherits(x, classes)
}
