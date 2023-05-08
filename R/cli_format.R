#' cli_format style for sf objects
#'
#' @param x A sf object to format with the cli_format.sf method.
#' @param style Not in use for the 'sf' class method.
#' @param ... Not in use for the 'sf' class method.
#' @seealso [cliExtras::register_cli_format()]
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   library(cli)
#'   library(sf)
#'   cliExtras::register_cli_format("sf", cli_format.sf)
#'   nc <- read_sf(system.file("shape/nc.shp", package = "sf"))
#'   cli_text("`nc` is a {.val {nc}}")
#' }
#' }
#' @export
#' @importFrom rlang check_installed
#' @importFrom cli cli_format pluralize
#' @importFrom sf st_geometry_type st_crs
cli_format.sf <- function(x,
                          style = NULL,
                          ...) {
  txt <- "a simple feature collection with"
  txt <- c(txt, cli::pluralize("{nrow(x)} feature{?s},"))
  txt <- c(txt, cli::pluralize("{ncol(x) - 1} field{?s},"))

  geom_type <- sf::st_geometry_type(x, by_geometry = FALSE)
  geom_type <- unique(as.character(geom_type))

  txt <- c(txt, "and", paste0("`", geom_type, "`"), "geometry")

  crs <- sf::st_crs(x)$srid
  if (!is.na(crs)) {
    txt <- c(txt, "in", paste0("`", crs, "`"))
  }

  paste0(txt, collapse = " ")
}
