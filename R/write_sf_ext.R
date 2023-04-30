#' Write or cache a simple feature object to a file
#'
#' The write_sf_ext and write_sf_cache helper functions wrap the [sf::write_sf()]
#' function to provide some additional options including consistent file naming
#' with [make_filename()] and features including:
#'
#' - If fileext is "csv", "xlsx", or "gsheet" the file is converted to a
#' dataframe using [df_to_sf()]
#' - If the data is not an `sf` object and none of these filenames are provided,
#' the user is prompted to save the file as an rda file with [readr::write_rds()].
#' - If cache is `TRUE` use [write_sf_cache()] to cache file after writing a copy to
#' the path provided.
#' - If data is a named sf list, pass the name of each sf object in the list to
#' the name parameter and keep all other parameters consistent to write a file
#' for each object in the list. No ... parameters are passed if data is an sf
#' list.
#'
#' @param data A `sf` object, data frame, or other object to write.
#' @param filename,fileext,filetype File name and/or file extension to write.
#'   filetype is superseded in favor of fileext. Both are optional if path
#'   includes filename and type, e.g. "~/Documents/data.geojson". fileext can be
#'   provided as part of the filename, e.g. "data.geojson". If a filename
#'   includes a file extensions and a separate fileext is also provided, the
#'   separate fileext parameter is used. Supported file extensions include
#'   "csv", "xlsx", "gsheet" (writes a Google Sheet), "rda", or any fileext
#'   supported by the available drivers (use [sf::st_drivers()] to list
#'   drivers).
#' @param data_dir cache data directory, defaults to [rappdirs::user_cache_dir()]
#'   when data_dir is `NULL`. (only used for [write_sf_cache()]; default is used
#'   when `cache = TRUE` for [write_sf_ext()])
#' @param overwrite Logical. Default `FALSE`. If `TRUE`, overwrite any existing
#'   cached files that use the same file name.
#' @param cache If `TRUE`, write `sf` object to file in cache directory;
#'   defaults to `FALSE`.
#' @inheritParams filenamr::make_filename
#' @inheritParams write_sf_cache
#' @param ... If data is an sf object and the fileext is "csv" or "xlsx", the
#'   ... parameters are passed to [sf_to_df()] or to [sf::write_sf()] otherwise. If
#'   fileext is "rda" ... parameters are passed to [readr::write_rds()].
#' @seealso
#'  [sf::st_write()]
#' @export
#' @importFrom sf write_sf
write_sf_ext <- function(data,
                         name = NULL,
                         label = NULL,
                         prefix = NULL,
                         postfix = NULL,
                         filename = NULL,
                         fileext = NULL,
                         filetype = NULL,
                         description = NULL,
                         path = NULL,
                         cache = FALSE,
                         pkg = "sfext",
                         overwrite = FALSE,
                         onefile = FALSE,
                         ...) {
  fileext <- fileext %||% filetype
  if (is_sf_list(data) && is_named(data)) {
    write_sf_list(
      data,
      label = label,
      prefix = prefix,
      postfix = postfix,
      fileext = fileext,
      path = path,
      cache = cache,
      overwrite = overwrite,
      onefile = onefile,
      ...
    )

    return(invisible(NULL))
  }

  if (has_fileext(path) && is_null(name) && is_null(filename)) {
    filename <- basename(path)
    path <- dirname(path)
    if (path == ".") {
      path <- NULL
    }
  }

  # If data is sf object, write or cache it
  filename <-
    filenamr::make_filename(
      name = name,
      label = label,
      fileext = fileext,
      filename = filename,
      path = NULL,
      prefix = prefix,
      postfix = postfix
    )

  write_sf_types(
    data = data,
    filename = filename,
    fileext = fileext,
    path = path,
    overwrite = overwrite,
    ...
  )

  if (cache) {
    write_sf_cache(
      data = data,
      filename = filename,
      overwrite = overwrite,
      pkg = pkg,
      ...
    )
  }
}

#' @rdname write_sf_ext
#' @name write_sf_list
#' @param onefile If `TRUE` and the fileext if "gpkg" (directly or from
#'   filename), save a sf list as a multilayer GeoPackage file where names for
#'   list items are used as layer names.
#' @export
write_sf_list <- function(data,
                          name = NULL,
                          label = NULL,
                          prefix = NULL,
                          postfix = NULL,
                          filename = NULL,
                          fileext = NULL,
                          filetype = NULL,
                          path = NULL,
                          overwrite = FALSE,
                          onefile = FALSE,
                          cache = FALSE,
                          ...) {
  fileext <- fileext %||% filetype
  if (!onefile) {
    walk(
      seq_along(data),
      ~ write_sf_ext(
        data = data[[.x]],
        name = names(data)[[.x]],
        label = label,
        prefix = prefix,
        postfix = postfix,
        fileext = fileext,
        path = path,
        cache = cache,
        overwrite = overwrite
      )
    )

    return(invisible(NULL))
  }

  filename <-
    filenamr::make_filename(
      name = name,
      label = label,
      prefix = prefix,
      postfix = postfix,
      fileext = fileext,
      filename = filename,
      path = path,
      cache = cache
    )

  multilayer_fileext <- c("gpkg", "gdb")

  if (!has_fileext(filename, multilayer_fileext)) {
    cli_abort(
      "{.arg fileext} must be {.or {multilayer_fileext}}
      when {.code onefile = TRUE}"
    )
  }

  walk(
    seq_along(data),
    function(i) {
      layer_options <- NULL
      y <- names(data)[[i]]
      if (has_fileext(filename, "gdb")) {
        layer_options <- glue("LAYER_ALIAS={y}")
        y <- NULL
      }

      sf::write_sf(
        data[[i]],
        dsn = filename,
        layer = y,
        layer_options = layer_options
      )
    }
  )
}

#' @rdname write_sf_ext
#' @name write_sf_cache
#' @param pkg The name of the package cache directory to use for
#'   [write_sf_cache] or [write_sf_ext] if `cache = TRUE`.
#' @export
#' @importFrom sf write_sf
write_sf_cache <- function(data,
                           name = NULL,
                           label = NULL,
                           prefix = NULL,
                           postfix = NULL,
                           filename = NULL,
                           fileext = NULL,
                           filetype = NULL,
                           data_dir = NULL,
                           pkg = "sfext",
                           overwrite = FALSE,
                           create = TRUE,
                           ...) {
  fileext <- fileext %||% filetype

  filename <-
    filenamr::make_filename(
      name = name,
      label = label,
      fileext = fileext,
      filename = filename,
      prefix = prefix,
      postfix = postfix,
      path = NULL
    )

  path <-
    filenamr::get_data_dir(
      path = data_dir,
      cache = TRUE,
      create = create,
      pkg = pkg,
      allow_null = FALSE
    )

  write_sf_types(
    data = data,
    filename = filename,
    path = path,
    overwrite = overwrite,
    ...
  )
}

#' @rdname write_sf_ext
#' @name write_sf_gist
#' @inheritParams gistr::gist_create
#' @param token A personal access token on GitHub with permission to create
#'   gists; defaults to Sys.getenv("GITHUB_PAT")
#' @export
#' @importFrom filenamr make_filename
write_sf_gist <- function(data,
                          name = NULL,
                          label = NULL,
                          prefix = NULL,
                          postfix = NULL,
                          filename = NULL,
                          fileext = "geojson",
                          filetype = NULL,
                          description = NULL,
                          public = TRUE,
                          browse = FALSE,
                          token = Sys.getenv("GITHUB_PAT")) {
  fileext <- fileext %||% filetype
  check_installed("gistr")

  filename <-
    filenamr::make_filename(
      name = name,
      label = label,
      fileext = fileext,
      filename = filename,
      prefix = prefix,
      postfix = postfix,
      path = NULL
    )

  write_sf_types(
    data = data,
    filename = filename,
    fileext = fileext,
    path = tempdir(),
    overwrite = TRUE
  )

  gistr::gist_auth(app = token)

  description <-
    description %||% glue("A {fileext} format spatial data file.")

  cli_alert_success("Creating gist for {.file filename}")

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
write_sf_gsheet <- function(data,
                            name = NULL,
                            label = NULL,
                            prefix = NULL,
                            postfix = NULL,
                            filename = NULL,
                            sheet = 1,
                            ask = FALSE,
                            key = NULL,
                            ...) {
  check_installed("googlesheets4")

  if (!is_null(filename)) {
    filename <- str_remove_fileext(filename, fileext = "gsheet")
  }

  filename <-
    filenamr::make_filename(
      name = name,
      label = label,
      fileext = NULL,
      filename = filename,
      prefix = prefix,
      postfix = postfix,
      path = NULL
    )

  ss <- googlesheets4::gs4_create(name = filename)

  data <- suppressMessages(sf_to_df(data, ...))

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
#' @importFrom sf write_sf
#' @importFrom cliExtras cli_yesno
#' @importFrom filenamr check_file_overwrite
write_sf_types <- function(data,
                           filename = NULL,
                           path = NULL,
                           fileext = NULL,
                           filetype = NULL,
                           description = NULL,
                           overwrite = TRUE,
                           append = FALSE,
                           layer_options = NULL,
                           ...) {
  fileext <- fileext %||% filetype
  # Get working directory if path is NULL
  # if (is_null(path)) {
  #   path <- getwd()
  #   cli_inform("Setting path to current working directory: {.file {path}}")
  # }

  # Set filename from path if ends with a fileext
  if (has_fileext(path)) {
    if (!is_null(filename)) {
      # FIXME: Is this just an internal error or can this be triggered by a
      # user?
      cli_abort("A {.arg filename} *or* {.arg path} with a filename must be
      provided. Both can't be provided.")
    }

    filename <- basename(path)
    path <- dirname(path)
  }

  # Get fileext from filename if fileext is NULL
  fileext <- fileext %||% str_extract_fileext(filename)
  # Add fileext to filename if it doesn't have a filename at the end
  filename <- str_add_fileext(filename, fileext)

  # Remove filename from path
  # FIXME: assumes that the user has not provided both a filename
  # and a path ending in a filename - add a check to confirm
  # Check if overwrite is needed and possible
  filenamr::check_file_overwrite(
    filename = filename,
    path = path,
    overwrite = overwrite
  )

  if (!is_null(path)) {
    # Put path and filename back together
    path <- file.path(path, filename)
  } else {
    path <- filename
  }

  if (is_sf(data)) {
    type <-
      dplyr::case_when(
        is_csv_fileext(filename) ~ "sf_csv",
        is_excel_fileext(filename) ~ "sf_excel",
        any(fileext %in% "gsheet") ~ "sf_gsheet",
        any(fileext %in% "svg") ~ "sf_svg",
        !any(fileext %in% c("rda", "rds", "rdata")) ~ "sf_spatial",
        TRUE ~ "rda"
      )

    if ("geojson" %in% fileext) {
      data <- st_transform_ext(data, 4326)
    }

    if (has_fileext(filename, "gpkg") & !is_null(description)) {
      layer_options <- glue("DESCRIPTION={description}")
    }
  } else {
    type <-
      dplyr::case_when(
        is_csv_fileext(filename) && is.data.frame(data) ~ "df_csv",
        is_excel_fileext(filename) && is.data.frame(data) ~ "df_excel",
        TRUE ~ "rda"
      )
  }

  if (type == "rda") {
    if (!(fileext %in% c("rda", "rds", "RData"))) {
      ask <-
        is_interactive() &&
          cli_yesno(
            c("{.arg data} is not a simple feature object.",
              ">" = "Do you want to save {.arg data} as a RDA file?"
            )
          )

      if (!ask) {
        return(invisible())
      }
    }

    path <- str_remove_fileext(path, fileext)
    path <- str_add_fileext(path, "rda")
  }

  # Check if readr is installed
  if (type %in% c("sf_csv", "df_csv", "rda")) {
    check_installed("readr")
  } else if (type %in% c("sf_excel", "df_excel")) {
    check_installed("openxlsx")
    path <- str_add_fileext(path, "xlsx")
  }

  # Drop geometry for sf to csv export
  if (type %in% c("sf_csv", "sf_excel")) {
    data <- suppressMessages(sf_to_df(data, ...))
  }

  cli_inform(c("v" = "Writing {.file {path}}"))

  switch(type,
    "sf_csv" = readr::write_csv(x = data, file = path),
    "sf_excel" = openxlsx::write.xlsx(data, file = path),
    "sf_gsheet" = write_sf_gsheet(data = data, filename = filename, ...),
    "sf_spatial" = sf::write_sf(obj = data, dsn = path, layer_options = layer_options, ...),
    "sf_svg" = write_sf_svg(data = data, filename = path, ...),
    "df_csv" = readr::write_csv(x = data, file = path),
    "df_excel" = openxlsx::write.xlsx(data, file = path),
    "rda" = readr::write_rds(x = data, file = path, ...)
  )
}

#' Write an sf object to an svg file
#'
#' [write_sf_svg()] uses [plot()] and [svg()] to create a simple plot of an sf
#' object geometry. This function is convenient for working with designers or
#' other collaborators interested in using spatial data outside of R or a
#' desktop GIS application.
#'
#' @name write_sf_svg
#' @inheritParams grDevices::svg
#' @inheritDotParams grDevices::svg
#' @export
#' @importFrom grDevices svg dev.off
#' @importFrom sf st_geometry
write_sf_svg <- function(data,
                         filename = NULL,
                         path = NULL,
                         ...,
                         width = 10,
                         height = 10) {
  check_required(data)

  if (is_null(filename) && has_fileext(path)) {
    filename <- basename(path)
    path <- dirname(path)
  }

  if (!is_null(path)) {
    filename <- file.path(path, filename)
  }

  cli_abort_ifnot(
    "{.arg filename} or {.arg path} must include a {.val svg} file extension.",
    condition = has_fileext(filename, "svg")
  )

  grDevices::svg(
    filename = filename,
    width = width,
    height = height,
    antialias = "none",
    ...
  )

  # Code of the plot
  plot(sf::st_geometry(data))

  grDevices::dev.off()

  invisible(data)
}
