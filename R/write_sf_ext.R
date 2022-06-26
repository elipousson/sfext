#' Write or cache a simple feature object to a file
#'
#' The write_sf_ext and write_sf_cache helper functions wrap the [sf::write_sf]
#' function to provide some additional options including consistent file naming
#' with [make_filename()] and features including:
#'
#' - If the data is not an sf object, optionally save as an RDS file.
#' - If filetype is "csv" or the filename ends in ".csv" the file is
#' automatically converted to a dataframe using [df_to_sf()]; if file type is
#' "gsheet" the file is converted and turned into a new Google Sheet document
#' (if a Google account is authorized with the {googlesheets4} package using the
#' [write_sf_gsheet()] function.).
#' - If cache is `TRUE` use write_sf_cache to cache file after writing a copy to
#' the path provided.
#' - If data is a named sf list, pass the name of each sf object in the list to
#' the name parameter and keep all other parameters consistent to write a file
#' for each object in the list.
#'
#' @param data `sf` object to write.
#' @param filename File name to use. If filename is provided and the data is an
#'   `sf` object make sure to include the file type, e.g. "data.gpkg" or
#'   "data.csv". Objects that are not simple features are written to RDS with
#'   `readr::write_rds()`.
#' @param data_dir cache data directory, defaults to
#'   [rappdirs::user_cache_dir()] when data_dir is `NULL`. (only used
#'   for write_sf_cache; default is used when `cache = TRUE` for write_sf_ext)
#' @param overwrite Logical. Default `FALSE`. If `TRUE`, overwrite any existing
#'   cached files that use the same file name.
#' @param filetype File type to write and cache, Default: `NULL` for
#'   `write_sf_ext()`
#' @param cache If `TRUE`, write `sf` object to file in cache directory;
#'   defaults to `FALSE`.
#' @inheritParams make_filename
#' @inheritParams write_sf_cache
#' @seealso
#'  [sf::st_write()]
#' @export
#' @md
#' @importFrom purrr discard walk
#' @importFrom sf write_sf
#' @importFrom glue glue
write_sf_ext <- function(data,
                         name = NULL,
                         label = NULL,
                         prefix = NULL,
                         postfix = NULL,
                         filename = NULL,
                         filetype = NULL,
                         path = NULL,
                         cache = FALSE,
                         overwrite = FALSE) {
  if (is_sf_list(data, named = TRUE)) {
    purrr::map(
      data,
      ~ write_sf_ext(
        data = .x,
        name = names(.x),
        label = label,
        prefix = prefix,
        postfix = postfix,
        filetype = filetype,
        path = path,
        cache = cache,
        overwrite = overwrite
      )
    )
  } else {

    # If data is sf object, write or cache it
    filename <-
      make_filename(
        name = name,
        label = label,
        filetype = filetype,
        filename = filename,
        path = NULL,
        prefix = prefix,
        postfix = postfix
      )

    if (is.null(filetype)) {
      filetype <- str_extract_filetype(filenname)
    }

    if (is.null(path)) {
      path <- filename
    } else {
      path <- file.path(path, filename)
    }

    if (!is.null(filetype) && ("geojson" %in% filetype)) {
      data <-
        st_transform_ext(
          data,
          crs = 4326
        )
    }

    write_sf_types(
      data = data,
      filename = filename,
      filetype = filetype,
      path = path,
      overwrite = overwrite
    )

    if (cache) {
      write_sf_cache(
        data = data,
        filename = filename,
        overwrite = overwrite
      )
    }
  }
}

#' @rdname write_sf_ext
#' @name write_sf_cache
#' @export
#' @importFrom sf write_sf
#' @importFrom readr write_rds
write_sf_cache <- function(data,
                           name = NULL,
                           label = NULL,
                           prefix = NULL,
                           postfix = NULL,
                           filename = NULL,
                           filetype = NULL,
                           data_dir = NULL,
                           overwrite = FALSE) {
  data_dir <- get_data_dir(path = data_dir)

  filename <-
    make_filename(
      name = name,
      label = label,
      filetype = filetype,
      filename = filename,
      prefix = prefix,
      postfix = postfix,
      path = NULL
    )

  path <- file.path(data_dir, filename)

  write_sf_types(
    data = data,
    filename = filename,
    path = path,
    filetype = filetype,
    overwrite = overwrite
  )
}

#' @rdname write_sf_ext
#' @name write_sf_gist
#' @inheritParams gistr::gist_create
#' @param token A personal access token on GitHub with permission to create
#'   gists; defaults to Sys.getenv("GITHUB_PAT")
#' @export
write_sf_gist <- function(data,
                          name = NULL,
                          label = NULL,
                          prefix = NULL,
                          postfix = NULL,
                          filename = NULL,
                          filetype = "geojson",
                          description = NULL,
                          public = TRUE,
                          browse = FALSE,
                          token = get_access_token(type = "GITHUB_PAT")) {
  is_pkg_installed("gistr")

  filename <-
    make_filename(
      name = name,
      label = label,
      filetype = filetype,
      filename = filename,
      prefix = prefix,
      postfix = postfix,
      path = NULL
    )

  path <- tempdir()

  if ("geojson" %in% filetype) {
    data <-
      st_transform_ext(
        data,
        crs = 4326
      )
  }

  suppressMessages(
    write_sf_types(
      data = data,
      filename = filename,
      filetype = filetype,
      path = path,
      overwrite = TRUE
    )
  )

  gistr::gist_auth(app = token)

  if (is.null(description)) {
    description <-
      paste("A", filetype, "format spatial data file.")
  }

  cli::cli_alert_success("Creating gist for {.file filename}")

  suppressWarnings(
    gistr::gist_create(
      files = file.path(path, filename),
      filename = filename,
      description = description,
      public = public,
      browse = browse
    )
  )
}

#' @rdname write_sf_ext
#' @name write_sf_gsheet
#' @param ask If `TRUE`, the user is prompted to make revisions to the created
#'   Google Sheet. When user responds to the prompt, the date is read back into
#'   the environment using [read_sf_gsheet] and joined to the provided data with
#'   the column name provided to key. Defaults to `FALSE`.
#' @param key If ask is `TRUE`, a key is required to join the sheet data to the
#'   provided data.
#' @inheritParams googlesheets4::sheet_write
#' @export
#' @importFrom stringr str_remove
write_sf_gsheet <- function(data,
                            name = NULL,
                            label = NULL,
                            prefix = NULL,
                            postfix = NULL,
                            filename = NULL,
                            sheet = 1,
                            ask = FALSE,
                            key = NULL) {
  is_pkg_installed("googlesheets4")

  if (!is.null(filename)) {
    filename <-
      str_remove_filetype(filename, filetype = "gsheet")
  }

  filename <-
    make_filename(
      name = name,
      label = label,
      filetype = NULL,
      filename = filename,
      prefix = prefix,
      postfix = postfix,
      path = NULL
    )

  # FIXME: Using the path as the name may cause issues
  ss <-
    googlesheets4::gs4_create(
      name = filename
    )

  data <- sf_to_df(data)

  googlesheets4::write_sheet(
    data = data,
    ss = ss,
    sheet = sheet
  )

  if (!ask) {
    return(data)
  }

  join_sf_gsheet(data, ss = ss, sheet = sheet, key = key)
}

#' Write sf to a file of selected type
#'
#' @noRd
#' @importFrom readr write_csv write_rds
#' @importFrom sf write_sf
#' @importFrom stringr str_detect
#' @importFrom cli cli_alert_success
write_sf_types <- function(data,
                           filename = NULL,
                           path = NULL,
                           filetype = NULL,
                           overwrite = TRUE) {

  check_file_overwrite(filename = filename, path = path, overwrite = overwrite)

  if (is_sf(data)) {
    cli::cli_alert_success("Writing {.file {path}}")

    if (grepl(".csv$", path) |
      (!is.null(filetype) && (filetype == "csv"))) {
      readr::write_csv(
        x = sf_to_df(data),
        file = path
      )
    } else if (!is.null(filetype) &&
      (filetype == "gsheet")) {
      write_sf_gsheet(data = data, filename = filename)
    } else if (!is.null(filename) &&
      !stringr::str_detect(path, paste0(filename, "$"))) {
      sf::write_sf(
        obj = data,
        dsn = file.path(path, filename)
      )
    } else {
      sf::write_sf(
        obj = data,
        dsn = path
      )
    }
  } else if (
    cli_yeah(
      "This data is not a simple feature object.
      Do you want to save the file as an RDS file?"
    )
  ) {

    # Remove file extension from path
    path <- str_remove_filetype(path, filetype)

    cli::cli_alert_success("Writing {.file {path}}")

    readr::write_rds(
      x = data,
      file = path
    )
  }
}

#' Check before overwriting file
#'
#' @noRd
#' @importFrom cli cli_abort cli_alert_success
#' @importFrom stringr str_detect
check_file_overwrite <- function(filename = NULL,
                           path = NULL,
                           overwrite = TRUE) {
  if (!is.null(filename) && (filename %in% list.files(path))) {
    if (!overwrite) {
      overwrite <-
        cli_yeah(
          "A file with the same name exists in {.file {path}}
        Do you want to overwrite {.val {filename}}?"
        )
    }

    if (!overwrite) {
      cli::cli_abort("{.file {filename}} was not overwritten.")
    }

    cli::cli_alert_success("Removing {.val {filename}} from {.file {path}}")
    if (!stringr::str_detect(path, paste0(filename, "$"))) {
      file.remove(file.path(path, filename))
    } else {
      file.remove(path)
    }
  }
}

