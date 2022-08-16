#' Write or cache a simple feature object to a file
#'
#' The write_sf_ext and write_sf_cache helper functions wrap the [sf::write_sf()]
#' function to provide some additional options including consistent file naming
#' with [make_filename()] and features including:
#'
#' - If filetype is "csv", "xlsx", or "gsheet" the file is converted to a
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
#' @param filename,filetype File name and/or write. Both are optional if path
#'   includes filename and type, e.g. "~/Documents/data.geojson". Filetype can
#'   be provided as part of the filename, e.g. "data.geojson". If a filename
#'   includes a filetype and a separate filetype is also provided, the separate
#'   filetype parameter is used. Supported filetypes includes "csv", "xlsx",
#'   "gsheet" (writes a Google Sheet), "rda", or any filetype supported by the
#'   available drivers (use [sf::st_drivers()] to list drivers).
#' @param data_dir cache data directory, defaults to [rappdirs::user_cache_dir()]
#'   when data_dir is `NULL`. (only used for [write_sf_cache()]; default is used
#'   when `cache = TRUE` for [write_sf_ext()])
#' @param overwrite Logical. Default `FALSE`. If `TRUE`, overwrite any existing
#'   cached files that use the same file name.
#' @param cache If `TRUE`, write `sf` object to file in cache directory;
#'   defaults to `FALSE`.
#' @inheritParams make_filename
#' @inheritParams write_sf_cache
#' @param ... If data is an sf object and the filetype is "csv" or "xlsx", the
#'   ... parameters are passed to [sf_to_df()] or to [sf::write_sf()] otherwise. If
#'   filetype is "rda" ... parameters are passed to [readr::write_rds()].
#' @seealso
#'  [sf::st_write()]
#' @export
#' @md
#' @importFrom purrr discard walk
#' @importFrom sf write_sf
write_sf_ext <- function(data,
                         name = NULL,
                         label = NULL,
                         prefix = NULL,
                         postfix = NULL,
                         filename = NULL,
                         filetype = NULL,
                         path = NULL,
                         cache = FALSE,
                         pkg = "sfext",
                         overwrite = FALSE,
                         ...) {
  if (is_sf_list(data, named = TRUE)) {
    purrr::walk(
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

    invisible(return(NULL))
  }

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

  write_sf_types(
    data = data,
    filename = filename,
    filetype = filetype,
    path = path,
    overwrite = overwrite,
    ...
  )

  if (!cache) {
    invisible(return(NULL))
  }

  write_sf_cache(
    data = data,
    filename = filename,
    overwrite = overwrite,
    pkg = pkg,
    ...
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
                           filetype = NULL,
                           data_dir = NULL,
                           pkg = "sfext",
                           overwrite = FALSE,
                           create = TRUE,
                           ...) {
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

  path <-
    get_data_dir(
      path = data_dir,
      cache = TRUE,
      create = create,
      pkg = pkg,
      null.ok = FALSE
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
                          token = Sys.getenv("GITHUB_PAT")) {
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

  write_sf_types(
    data = data,
    filename = filename,
    filetype = filetype,
    path = tempdir(),
    overwrite = TRUE
  )

  gistr::gist_auth(app = token)

  description <-
    description %||% glue("A {filetype} format spatial data file.")

  cli_inform(c("v" = "Creating gist for {.file filename}"))

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
                            key = NULL,
                            ...) {
  is_pkg_installed("googlesheets4")

  if (!is.null(filename)) {
    filename <- str_remove_filetype(filename, filetype = "gsheet")
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
#' @importFrom stringr str_remove
write_sf_types <- function(data,
                           filename = NULL,
                           path = NULL,
                           filetype = NULL,
                           overwrite = TRUE,
                           append = FALSE,
                           ...) {
  # Get working directory if path is NULL
  if (is.null(path)) {
    path <- getwd()
    cli_inform("Setting path to current working directory: {.file {path}}")
  }

  # Set filename from path if ends with a filetype
  if (has_filetype(path)) {
    if (!is.null(filename)) {
      cli_abort("A {.arg filename} *or* {.arg path} with a filename must be
      provided. Both can't be provided.")
    }

    filename <- basename(path)
  }

  # Get filetype from filename if filetype is NULL
  filetype <- filetype %||% str_extract_filetype(filename)
  # Add filetype to filename if it doesn't have a filename at the end
  filename <- str_add_filetype(filename, filetype)

  # Remove filename from path
  # FIXME: assumes that the user has not provided both a filename
  # and a path ending in a filename - add a check to confirm
  folder_path <- stringr::str_remove(path, glue("{filename}$"))

  # Check if overwrite is needed and possible
  check_file_overwrite(
    filename = filename,
    path = folder_path,
    overwrite = overwrite
  )

  # Put path and filename back together
  path <- file.path(folder_path, filename)

  if (is_sf(data)) {
    type <-
      dplyr::case_when(
        is_csv_path(filename) ~ "sf_csv",
        is_excel_path(filename) ~ "sf_excel",
        any(filetype %in% "gsheet") ~ "sf_gsheet",
        !any(filetype %in% c("rda", "rds", "rdata")) ~ "sf_spatial",
        TRUE ~ "rda"
      )

    if ("geojson" %in% filetype) {
      data <- st_transform_ext(data, 4326)
    }
  } else {
    type <-
      dplyr::case_when(
        is_csv_path(filename) && is.data.frame(data) ~ "df_csv",
        is_excel_path(filename) && is.data.frame(data) ~ "df_excel",
        TRUE ~ "rda"
      )
  }

  if (type == "rda") {
    if (!(filetype %in% c("rda", "rds", "RData"))) {
      ask <-
        is_interactive() &&
          cli_yeah(
            c("{.arg data} is not a simple feature object.",
              ">" = "Do you want to save {.arg data} as a RDA file?"
            )
          )

      if (!ask) {
        invisible(return(NULL))
      }
    }

    path <- str_remove_filetype(path, filetype)
    path <- str_add_filetype(path, "rda")
  }

  # Check if readr is installed
  if (type %in% c("sf_csv", "df_csv", "rda")) {
    is_pkg_installed("readr")
  } else if (type %in% c("sf_excel", "df_excel")) {
    is_pkg_installed("openxlsx")
    path <- str_add_filetype(path, "xlsx")
  }

  # Drop geometry for sf to csv export
  if (type %in% c("sf_csv", "sf_excel")) {
    data <- suppressMessages(sf_to_df(data, ...))
  }

  cli_inform(c("v" = "Writing {.file {path}}"))

  switch(type,
    "sf_csv" = readr::write_csv(x = data, file = path),
    "sf_excel" = openxlsx::write.xlsx(data, fiile = path),
    "sf_gsheet" = write_sf_gsheet(data = data, filename = filename, ...),
    "sf_spatial" = sf::write_sf(obj = data, dsn = path, ...),
    "df_csv" = readr::write_csv(x = data, file = path),
    "df_excel" = openxlsx::write.xlsx(data, fiile = path),
    "rda" = readr::write_rds(x = data, file = path, ...)
  )
}

#' Check before overwriting file
#'
#' @noRd
#' @importFrom stringr str_detect
check_file_overwrite <- function(filename = NULL,
                                 path = NULL,
                                 overwrite = TRUE) {
  filename <- filename %||% basename(path)

  if (!has_filetype(filename)) {
    abort()
  }

  if (filename %in% list.files(path)) {
    if (!overwrite && is_interactive()) {
      overwrite <-
        cli_yeah(
          c(
            "i" = "A file with the same name exists in {.file {path}}",
            ">" = "Do you want to overwrite {.val {filename}}?"
          )
        )
    }

    cli_abort_ifnot(
      c(
        "!" = "{.file {filename}} can't be saved.",
        "i" = "A file with the same name already exists and
        {.arg overwrite = FALSE}."
      ),
      condition = overwrite
    )

    cli_inform(
      c("v" = "Removing {.val {filename}} from {.file {path}}")
    )

    if (!stringr::str_detect(path, paste0(filename, "$"))) {
      file.remove(file.path(path, filename))
    } else {
      file.remove(path)
    }

    invisible(return(NULL))
  }
}
