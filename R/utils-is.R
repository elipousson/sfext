#' Is the class of this object any of the specified classes?
#'
#' @noRd
is_class <- function(x, classes = NULL, null.ok = FALSE) {
  if (is.null(x) && null.ok) {
    return(TRUE)
  }

  any(classes %in% class(x))
}

#' Is this a GeoJSON file path or url?
#'
#' @noRd
is_geojson_path <- function(x) {
  grepl("\\.geojson$", x)
}

#' Is this a CSV file path or url?
#'
#' @noRd
is_csv_path <- function(x) {
  grepl("\\.csv$", x)
}

#' Is this a Excel file path or url?
#'
#' @noRd
is_excel_path <- function(x) {
  grepl("\\.xls$|\\.xlsx$", x)
}

#' Is this a RDS, RDA, or RData file path or url?
#'
#' @noRd
is_rdata_path <- function(x) {
  is_rda_path(x) | is_rds_path(x) | grepl("\\.RData$", x, ignore.case = TRUE)
}

#' Is this a RDS file path or url?
#'
#' @noRd
is_rds_path <- function(x) {
  grepl("\\.rds$", x, ignore.case = TRUE)
}

#' Is this a RDA file path or url?
#'
#' @noRd
is_rda_path <- function(x) {
  grepl("\\.rda$", x, ignore.case = TRUE)
}

#' Is this a URL?
#'
#' @noRd
is_url <- function(x) {
  grepl(
    "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+",
    x
  )
}

#' Is this an ArcGIS MapServer or FeatureServer URL?
#'
#' @noRd
is_esri_url <- function(x) {
  grepl("/MapServer|/FeatureServer", x)
}

#' Is this a Google Sheets URL?
#'
#' @noRd
is_gsheet_url <- function(x) {
  grepl("^https://docs.google.com/spreadsheets/", x)
}

#' Is this a GitHub gist URL?
#'
#' @noRd
is_gist_url <- function(x) {
  grepl("^https://gist.github.com/", x)
}

#' Is this a Google Maps URL?
#'
#' @noRd
is_gmap_url <- function(x) {
  grepl("^https://www.google.com/maps/", x)
}

#' Is this a unit class object?
#'
#' @noRd
is_unit <- function(x, null.ok = FALSE) {
  is_class(x, classes = "unit", null.ok)
}
