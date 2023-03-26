default_exif_tags <-
  c(
    "Title",
    "ImageDescription",
    "Keywords",
    "Headline",
    "Byline",
    "Caption",
    "FileName",
    "CreateDate",
    "DateTimeOriginal",
    "OffsetTimeOriginal",
    "ImageWidth",
    "ImageHeight",
    "Orientation",
    "SourceFile",
    "FileSize",
    "FileType",
    "*GPS*"
  )

exif_xwalk <-
  list(
    "description" = "image_description",
    "lon" = "longitude",
    "lat" = "latitude",
    "lon_ref" = "longitude_ref",
    "lat_ref" = "latitude_ref",
    "path" = "source_file",
    "img_width" = "image_width",
    "img_height" = "image_height",
    "exif_orientation" = "orientation"
  )

#' Read EXIF metadata to create a simple feature object or write
#' EXIF metadata to image files
#'
#' @description
#' `read_sf_exif()` read EXIF data from folder of files and, geometry is `TRUE`
#' and coordinate metadata is available, convert the data to a sf object. This
#' function also assigns a cardinal direction based on the direction metadata
#' and recodes the orientation metadata.
#'
#' For `write_exif()` the parameters are used to multiple tags with the same
#' values:
#'
#' - title: Title, IPTC:Headline, IPTC:ObjectName, XMP-dc:Title
#' - description: ImageDescription, XMP-dc:Description, and
#' IPTC:Caption-Abstract
#' - keywords: Keywords, IPTC:Keywords, XMP-dc:Subject
#'
#' @inheritParams filenamr::read_exif
#' @param sort Column name for variable to sort by passed to [sort_features()].
#'   Currently supports "lon", "lat", or "filename". Defaults to `NULL`.
#' @param tags Optional list of EXIF tags to read from files. Must include GPS
#'   tags to create an `sf` object.
#' @param fileext,filetype File extension or file type. filetype is used if
#'   fileext is `NULL`.
#' @param bbox Bounding box to filter by.
#' @param geometry If `TRUE` (default), return a simple feature object. If
#'   `FALSE`, return a data.frame.
#' @family read_write
#' @example examples/read_sf_exif.R
#' @export
#' @importFrom cli cli_abort cli_warn
#' @importFrom purrr map_dfr
#' @importFrom dplyr rename_with rename mutate case_when
#' @importFrom janitor clean_names
#' @importFrom rlang has_name
#' @importFrom sf st_crs
read_sf_exif <- function(path = NULL,
                         fileext = NULL,
                         filetype = NULL,
                         bbox = NULL,
                         sort = NULL,
                         tags = NULL,
                         geometry = TRUE,
                         ...) {
  geo_tags <- c("GPSLatitude", "GPSLongitude")

  if (geometry && !all(geo_tags %in% tags)) {
    cli_bullets(
      c(
        "!" = "{.arg tags} must be include {.val {c('GPSLatitude', 'GPSLongitude')}}
        to create a {.cls sf} object from EXIF metadata.",
        "*" = "Adding required tags to {.arg tags}."
      )
    )

    tags <- c(tags, geo_tags)
  }

  data <-
    filenamr::read_exif(
      path,
      fileext = fileext %||% filetype,
      tags = tags
    )

  if (geometry) {
    data <- df_to_sf(data, from_crs = 4326, crs = bbox)
  }

  if (!is.null(sort)) {
    data <- sort_features(data, sort = sort)
  }

  st_filter_ext(data, bbox)
}

#' Write EXIF data for photos on spatial join with a sf object or list of sf objects
#'
#' Extends [sfext::read_sf_exif()] and [filenamr::write_exif()]
#'
#' @name write_exif_from
#' @aliases write_exif_keywords
#' @inheritParams filenamr::write_exif
#' @inheritParams read_sf_exif
#' @param fileext,filetype File extension or file type. filetype is used if
#'   fileext is `NULL`.
#' @param from A sf object or list of sf objects where each object has a column
#'   with a name matching the .id parameter. The attribute value in this column
#'   are used to assign the tag parameter to the file at the provided path based
#'   on the spatial relationship set by join. For example, from may be boundary
#'   data used to assign keywords based on photo locations.
#' @param .id Column name in from with the values to use for tag values.
#' @param tag EXIF tag to update, supported options include "keywords", "title",
#'   or "description".
#' @param join geometry predicate function; defaults to `NULL`, set to
#'   [sf::st_intersects] if from contains only POLYGON or MULTIPOLYGON objects
#'   or [sf::st_nearest_feature] if from contains other types.
#' @export
#' @importFrom rlang has_name
#' @importFrom dplyr pull select all_of summarize group_by
#' @importFrom purrr map_dfr
#' @importFrom sf st_drop_geometry st_join
#' @importFrom cli cli_bullets
write_exif_from <- function(path,
                            fileext = NULL,
                            filetype = NULL,
                            from,
                            .id = "name",
                            tag = "keywords",
                            join = NULL,
                            overwrite = TRUE) {
  tag <- match.arg(tolower(tag), c("keywords", "title", "description"))

  if (!is_sf(path)) {
    data <- read_sf_exif(
      path = path,
      fileext = fileext,
      filetype = filetype
    )
    path <- get_path_files(path, filetype)
  } else if (is.data.frame(path)) {
    data <- path

    stopifnot(
      rlang::has_name(data, "path")
    )

    path <- dplyr::pull(data, path)
  }

  existing_vals <- data[[tag]]

  data <- has_same_name_col(data, col = .id)

  from <- as_sf_list(from, nm = NULL, crs = data)

  join <- set_join_by_geom_type(from, join = join)

  data <-
    purrr::map_dfr(
      from,
      ~ sf::st_drop_geometry(
        sf::st_join(
          data,
          dplyr::select(.x, dplyr::all_of(.id)),
          join = join
        )
      )
    )

  append_vals <-
    dplyr::summarize(
      dplyr::group_by(data, path),
      vals = unique(list(.data[[.id]]))
    )[["vals"]]

  replacement_vals <- append_vals

  if (!is.null(existing_vals)) {
    replacement_vals <-
      map2(
        existing_vals,
        append_vals,
        ~ unique(append(.x, .y))
      )
  }

  cliExtras::cli_list_files(
    path = path,
    text = c("v" = "Updated EXIF tag {.val {tag}} for {length(path)} file{?s}:")
  )
  suppressMessages(
    walk2_write_exif(path, replacement_vals, tag)
  )
}

#' Pass file path and replacement tag values to write_exif based on selected tag
#'
#' @noRd
#' @importFrom purrr walk2
walk2_write_exif <- function(path, replacement_vals, tag = "keywords") {
  if (tag == "keywords") {
    purrr::walk2(
      path,
      replacement_vals,
      ~ write_exif(
        path = .x, keywords = .y,
        overwrite = TRUE, append_keywords = FALSE
      )
    )
  } else if (tag == "title") {
    purrr::walk2(
      path,
      replacement_vals,
      ~ write_exif(
        path = .x, title = .y,
        overwrite = TRUE
      )
    )
  } else if (tag == "description") {
    purrr::walk2(
      path,
      replacement_vals,
      ~ write_exif(
        path = .x, description = .y,
        overwrite = TRUE
      )
    )
  }
}
