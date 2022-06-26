
#' Read location data from images to a simple feature object using EXIF or write EXIF metadata
#'
#' Read EXIF data from folder of images.
#'
#' @param path A path to folder of one or more files with EXIF location
#'   metadata.
#' @param bbox Optional bounding box to crop returned file (excluding images
#'   with location data outside the bounding box). If bbox is provided the
#'   returned data will match the crs of the bbox.
#' @param filetype The file extension or file type; defaults to `NULL`.
#' @param sort Column name for variable to sort by. Currently supports "lon"
#'   (default), "lat", or "filename"
#' @param tags Optional list of EXIF tags to read from files. Must include GPS
#'   tags to create an sf object.
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
                         sort = "lon",
                         tags = NULL,
                         ...) {
  is_pkg_installed("exiftoolr")

  if (!dir.exists(path)) {
    cli::cli_abort("A valid path is required and the path provided does not exist.")
  }

  # FIXME: This is a partial list of filetypes that support GPS EXIF metadata
  # filetype <- match.arg(filetype, c("jpg", "jpeg", "png", "tiff", "pdf"))

  if (is.null(tags)) {
    # FIXME: The default fields likely vary by file type and could be set based on that
    # NOTE: Are there other tags that should be included by default?
    tags <-
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
        "*GPS*",
        ...
      )
  } else if (!any(grepl("GPS", tags))) {
    cli::cli_warn("The tags must include GPS values to create a simple feature object based on the file EXIF data.")
  }

  file_list <- get_path_file_list(path, filetype)

  # FIXME: Figure out how to append path to the end of the table not the beginning
  data <-
    suppressMessages(
      purrr::map_dfr(
        file_list,
        ~ exiftoolr::exif_read(
          .x,
          tags = tags
        )
      )
    )

  data <-
    # Rename variables
    dplyr::rename_with(
      janitor::clean_names(data, "snake"),
      ~ sub("^gps_", "", .x)
    )

  if (rlang::has_name(data, "image_description")) {
    data <-
      dplyr::rename(
        data,
        description = image_description
      )
  }

  if (all(rlang::has_name(data, c("latitude", "longitude")))) {
    data <-
      dplyr::rename(
        data,
        lon = longitude,
        lat = latitude
      )
  }

  if (all(rlang::has_name(data, c("longitude_ref", "latitude_ref", "img_direction", "img_direction_ref", "source_file")))) {
    data <-
      dplyr::rename(
        data,
        lon_ref = longitude_ref,
        lat_ref = latitude_ref,
        path = source_file
      )
  }

  if (all(rlang::has_name(data, c("orientation", "image_width", "image_height")))) {
    data <-
      dplyr::rename(
        data,
        img_width = image_width,
        img_height = image_height,
        exif_orientation = orientation
      )

    data <-
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
          )
      )
  }

  data <- sort_features(data = data, sort = sort)

  exif_crs <- 4326

  data <- df_to_sf(data, crs = exif_crs)

  if (is.null(bbox)) {
    return(data)
  }

  get_location_data(
    location = bbox,
    data = data,
    from_crs = exif_crs,
    crs = sf::st_crs(bbox)
  )
}

#' Get filetype from the path (using most frequent type if multiple are at the path)
#'
#' @noRd
#' @importFrom fs dir_ls
#' @importFrom stringr str_extract
#' @importFrom cli cli_warn
get_path_filetype <- function(path, filetype = NULL) {
  if (!is.null(filetype)) {
    return(filetype)
  }
  dir_files <- fs::dir_ls(path)
  filetype <- unique(stringr::str_extract(dir_files, "(?<=\\.).+$"))

  if (length(filetype) > 1) {
    # https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
    filetype <- names(sort(table(filetype), decreasing = TRUE)[1])
    cli::cli_warn("The path {.file {path}} includes multiple filetypes. Using most frequent filetype: {.val {filetype}}")
  }

  return(filetype)
}

#' Get list of files at
#' @noRd
#' @importFrom fs dir_ls
get_path_file_list <- function(path, filetype = NULL) {
  filetype <- get_path_filetype(path, filetype)
  fs::dir_ls(path = path, glob = paste0("*.", filetype))
}

#' @name write_exif
#' @rdname read_sf_exif
#' @section Writing EXIF metadata
#' @param title Title to add to file metadata with exiftoolr, Default: `NULL`.
#' @param author Author to add to file metadata with exiftoolr, Default: `NULL`.
#' @param date Date to add to file metadata with exiftoolr (not currently
#'   working), Default: `NULL`.
#' @param keywords Keyword(s) added to file metadata with with exiftoolr,
#'   Default: `NULL`.
#' @param args Alternate arguments passed to [exiftoolr::exif_call()]. If args
#'   is not `NULL`, title, author, date, and keywords are ignored; defaults to
#'   `NULL`.
#' @param overwrite If TRUE, overwrite any existing EXIF metadata present in the
#'   provided fields; defaults to TRUE
#' @export
#' @importFrom glue glue
write_exif <- function(path = NULL,
                       filetype = NULL,
                       title = NULL,
                       author = NULL,
                       date = NULL,
                       keywords = NULL,
                       args = NULL,
                       overwrite = TRUE) {
  is_pkg_installed("exiftoolr")

  # FIXME: I want to implement a method that allows adding, replacing, or modifying exif
  if (is.null(args)) {
    if (!is.null(title)) {
      args <- c(args, "-Title=Untitled")
    } else {
      args <- c(args, glue::glue("-Title={title}"))
    }

    if (!is.null(author)) {
      args <- c(args, glue::glue("-Author={author}"))
    }

    if (!is.null(date)) {
      # FIXME: exiftoolr::exif_call() does not support the "now" value supported by exif
      # If CreateDate is set to now automatically, why bother revising with exiftoolr anyway?
      # TODO: Add support for subjects (partially complete with keywords) https://stackoverflow.com/questions/28588696/python-exiftool-combining-subject-and-keyword-tags#28609886
      date <- "now"
      if ("png" %in% filetype) {
        args <- c(args, glue::glue("-CreationTime={date}"))
      } else {
        args <- c(args, c("-CreateDate={date}", "-ModifyDate={date}"))
      }
    }

    if (!is.null(keywords)) {
      args <- c(args, paste0("-Keywords+=", keywords))
    }

    if (overwrite) {
      args <- c(args, "-overwrite_original")
    }
  }

  if (!is.null(args)) {
    if (length(path) == 1) {
      suppressMessages(
        suppressWarnings(
          exiftoolr::exif_call(
            args = args,
            path = path,
            quiet = TRUE
          )
        )
      )

      cli::cli_alert_success("EXIF metadata updated for {.file {path}}")
    } else {
      suppressMessages(
        suppressWarnings(
          purrr::walk(
            path,
            ~ exiftoolr::exif_call(
              args = args,
              path = .x,
              quiet = TRUE
            )
          )
        )
      )
    }
  }
}


#' @name write_exif_keywords
#' @rdname read_sf_exif
#' @param key_list List of sf objects with features with keywords, e.g. boundaries
#' @param key_col Column name in key_list with the values to use for keywords.
#' @param join geometry predicate function; defaults to `NULL`, set to
#'   [sf::st_intersects] if key_list contains only POLYGON or MULTIPOLYGON objects
#'   or [sf::st_nearest_feature] if key_list contains other types.
#' @export
#' @importFrom fs dir_ls
#' @importFrom purrr map_dfr walk2
#' @importFrom sf st_drop_geometry st_join
#' @importFrom dplyr select all_of group_by summarize
write_exif_keywords <- function(path,
                                filetype = NULL,
                                key_list,
                                key_col = "name",
                                keywords = NULL,
                                join = NULL,
                                overwrite = TRUE) {
  data <- read_sf_exif(path = path, filetype = filetype)
  data <- has_same_name_col(data, col = key_col)

  file_list <- get_path_file_list(path, filetype)
  key_list <- as_sf_list(key_list, nm = NULL, crs = data)

  join <-
    set_join_by_geom_type(key_list, join = join)

  data <-
    purrr::map_dfr(
      key_list,
      ~ sf::st_drop_geometry(
        sf::st_join(
          data,
          dplyr::select(.x, dplyr::all_of(key_col)),
          join = join
        )
      ),
      .id = path
    )

  data <-
    dplyr::group_by(
      data,
      path
    )

  data <-
    dplyr::summarize(
      data,
      keywords = list(unique(keywords, .data[[key_col]]))
    )

  purrr::walk2(
    data$path,
    data$keywords,
    ~ write_exif(path = .x, keywords = .y, overwrite = overwrite)
  )
}
