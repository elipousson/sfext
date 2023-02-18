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
#' @param path A path to folder or file.
#' @param bbox Optional bounding box to crop returned file (excluding images
#'   with location data outside the bounding box). If bbox is provided the
#'   returned data will match the crs of the bbox.
#' @param filetype The file extension or file type; defaults to `NULL`.
#' @param sort Column name for variable to sort by passed to [sort_features()].
#'   Currently supports "lon", "lat", or "filename". Defaults to `NULL`.
#' @param tags Optional list of EXIF tags to read from files. Must include GPS
#'   tags to create an `sf` object.
#' @param geometry If `TRUE` (defualt), return a simple feature object. If
#'   `FALSE`, return a data.frame.
#' @param ... Additional EXIF tags to pass to [exiftoolr::exif_read]
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
                         filetype = NULL,
                         bbox = NULL,
                         sort = NULL,
                         tags = NULL,
                         geometry = TRUE,
                         ...) {
  rlang::check_installed("exiftoolr")

  # FIXME: This is a partial list of filetypes that support GPS EXIF metadata
  # filetype <- match.arg(filetype, c("jpg", "jpeg", "png", "tiff", "pdf"))

  if (is.null(tags)) {
    # FIXME: The default fields likely vary by file type and could be set based
    # on that NOTE: Are there other tags that should be included by default?
    tags <- default_exif_tags
  } else if (!all(c("GPSLatitude", "GPSLongitude") %in% tags)) {
    cli_warn(
      c("{.arg tags} must be include {.val {c('GPSLatitude', 'GPSLongitude')}}
        to create a {.cls sf} object from EXIF metadata.",
        "i" = "The provided {.arg tags} are {.val {tags}}."
      )
    )
  }

  path <- get_path_files(path, filetype)

  # FIXME: Figure out how to append path to the end of the table not the
  # beginning
  data <-
    suppressMessages(
      exiftoolr::exif_read(
        path,
        tags = tags
      )
    )

  data <-
    # Rename variables
    dplyr::rename_with(
      janitor::clean_names(data),
      ~ sub("^gps_", "", .x)
    )

  xwalk <- exif_xwalk[rlang::has_name(data, exif_xwalk)]

  data <-
    # Rename variables
    dplyr::rename_with(
      data,
      ~ names(xwalk)[which(xwalk == .x)],
      .cols = as.character(xwalk)
    )

  data <- fmt_exif_orientation(data)

  data <- fmt_exif_direction(data)

  if (!geometry) {
    return(data)
  }

  data <- df_to_sf(data, from_crs = 4326, crs = bbox)

  if (!is.null(sort)) {
    data <- sort_features(data, sort = sort)
  }

  st_filter_ext(data, bbox)
}

#' @noRd
fmt_exif_orientation <- function(data) {
  has_orientation_names <-
    rlang::has_name(data, c("exif_orientation", "img_width", "img_width"))

  if (!all(has_orientation_names)) {
    return(data)
  }

  dplyr::mutate(
    data,
    exif_orientation =
      dplyr::case_when(
        exif_orientation == 1 ~ "Horizontal (normal)",
        exif_orientation == 2 ~ "Mirror horizontal",
        exif_orientation == 3 ~ "Rotate 180",
        exif_orientation == 4 ~ "Mirror vertical",
        exif_orientation == 5 ~ "Mirror horizontal and rotate 270 CW",
        exif_orientation == 6 ~ "Rotate 90 CW",
        exif_orientation == 7 ~ "Mirror horizontal and rotate 90 CW",
        exif_orientation == 8 ~ "Rotate 270 CW"
      ),
    orientation =
      dplyr::case_when(
        (img_width / img_height) > 1 ~ "landscape",
        (img_width / img_height) < 1 ~ "portrait",
        (img_width / img_height) == 1 ~ "square"
      ),
    .after = "exif_orientation"
  )
}

#' Format img_direction as cardinal directions (degrees and wind directions)
#'
#' @noRd
fmt_exif_direction <- function(data, .after = "img_direction") {
  if (!all(rlang::has_name(data, c("img_direction")))) {
    return(data)
  }

  # See https://en.wikipedia.org/wiki/Points_of_the_compass#8-wind_compass_rose
  cardinal_degrees <-
    c(
      "N" = 0, "NE" = 45,
      "E" = 90, "SE" = 135,
      "S" = 180, "SW" = 225,
      "W" = 270, "NW" = 315, "N" = 360
    )

  dplyr::mutate(
    data,
    img_cardinal_dir = cardinal_degrees[
      findInterval(img_direction, cardinal_degrees - 22.5)
    ],
    img_cardinal_wind = names(img_cardinal_dir),
    .after = .after
  )
}

#' @name write_exif
#' @rdname read_sf_exif
#' @param title Title to add to file metadata with exiftoolr, Default: `NULL`.
#' @param author Author to add to file metadata with exiftoolr, Default: `NULL`.
#' @param date Date to add to file metadata with exiftoolr (not currently
#'   working), Default: `NULL`.
#' @param keywords Keyword(s) added to file metadata with with exiftoolr,
#'   Default: `NULL`.
#' @param description description to add to ImageDescription EXIF tag, XMP-dc
#'   Description tag, and IPTC Caption-Abstract tag.
#' @param args Alternate arguments passed to [exiftoolr::exif_call()]. If args
#'   is not `NULL`, title, author, date, and keywords are ignored; defaults to
#'   `NULL`.
#' @param overwrite If `TRUE`, overwrite any existing EXIF metadata present in the
#'   provided fields; defaults to `TRUE`
#' @param append_keywords If `TRUE`, append keywords to existing keywords. If
#'   `FALSE` (default), replace existing keywords with the provided values.
#' @export
#' @importFrom cliExtras cli_paths
write_exif <- function(path,
                       filetype = NULL,
                       title = NULL,
                       author = NULL,
                       date = NULL,
                       keywords = NULL,
                       description = NULL,
                       args = NULL,
                       overwrite = TRUE,
                       append_keywords = FALSE) {
  rlang::check_installed("exiftoolr")

  # FIXME: I want to implement a method that allows adding, replacing, or modifying exif
  if (is.null(args)) {
    if (!is.null(title)) {
      args <- c(args, glue("-Title={title}"))
      args <- c(args, glue("-IPTC:Headline={title}"))
      args <- c(args, glue("-IPTC:ObjectName={title}"))
      args <- c(args, glue("-XMP-dc:Title={title}"))
    }

    if (!is.null(author)) {
      args <- c(args, glue("-Author={author}"))
    }

    if (!is.null(description)) {
      args <- c(args, glue("-ImageDescription={description}"))
      args <- c(args, glue("-XMP-dc:Description={description}"))
      args <- c(args, glue("-IPTC:Caption-Abstract={description}"))
    }

    if (!is.null(date)) {
      # FIXME: exiftoolr::exif_call() does not support the "now" value supported
      # by exif If CreateDate is set to now automatically, why bother revising
      # with exiftoolr anyway? TODO: Add support for subjects (partially
      # complete with keywords)
      # https://stackoverflow.com/questions/28588696/python-exiftool-combining-subject-and-keyword-tags#28609886
      date <- "now"
      if ("png" %in% filetype) {
        args <- c(args, glue("-CreationTime={date}"))
      } else {
        args <- c(args, c("-CreateDate={date}", "-ModifyDate={date}"))
      }
    }

    if (!is.null(keywords)) {
      op <- "+="

      if (overwrite && !append_keywords) {
        op <- "="
      }

      args <- c(args, paste0("-Keywords", op, keywords))
      args <- c(args, paste0("-IPTC:Keywords", op, keywords))
      args <- c(args, paste0("-XMP-dc:Subject", op, keywords))
    }

    if (overwrite) {
      args <- c(args, "-overwrite_original")
    }
  }

  if (!is.null(args)) {
    path <- get_path_files(path)

    suppressMessages(
      suppressWarnings(
        exiftoolr::exif_call(
          args = args,
          path = path,
          quiet = TRUE
        )
      )
    )

    cliExtras::cli_paths(path, "Updated EXIF metadata for")
  }
}

#' @name write_exif_from
#' @aliases write_exif_keywords
#' @rdname read_sf_exif
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
#' @importFrom purrr map_dfr map2
#' @importFrom sf st_drop_geometry st_join
#' @importFrom cli cli_bullets
write_exif_from <- function(path,
                            filetype = NULL,
                            from,
                            .id = "name",
                            tag = "keywords",
                            join = NULL,
                            overwrite = TRUE) {
  tag <- match.arg(tolower(tag), c("keywords", "title", "description"))

  if (!is_sf(path)) {
    data <- read_sf_exif(path = path, filetype = filetype)
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

  if (!is.null(existing_vals)) {
    replacement_vals <-
      purrr::map2(
        existing_vals,
        append_vals,
        ~ unique(append(.x, .y))
      )
  } else {
    replacement_vals <- append_vals
  }

  len_path <- length(path)

  cli_inform(
    c("v" = "Updated EXIF tag {.val {tag}} for {len_path} file{?s}:")
  )

  path_msg <- paste0("{.file ", path, "}")
  cli::cli_bullets(rlang::set_names(path_msg, rep("*", len_path)))

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
