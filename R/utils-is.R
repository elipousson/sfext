#' Is the class of this object any of the specified classes?
#'
#' @noRd
is_class <- function(x, classes = NULL, null.ok = FALSE) {
  if (is.null(x) && null.ok) {
    return(TRUE)
  }

  inherits(x, classes)
}
