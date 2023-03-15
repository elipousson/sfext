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
#' @md
#' @importFrom purrr walk walk2
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
  if (is_sf_list(data, named = TRUE)) {
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

    return(invisible())
  }

  if (has_fileext(path) && is.null(name) && is.null(filename)) {
    filename <- basename(path)
    path <- dirname(path)
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
    purrr::walk(
      data,
      ~ write_sf_ext(
        data = .x,
        name = names(.x),
        label = label,
        prefix = prefix,
        postfix = postfix,
        fileext = fileext,
        path = path,
        cache = cache,
        overwrite = overwrite
      )
    )

    return(invisible())
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
    cli::cli_abort(
      "{.arg filext} must be {.or {multilayer_fileext}}
      when {.code onefile = TRUE}"
    )
  }

  purrr::walk2(
    data,
    names(data),
    function(x, y) {
      layer_options <- NULL
      if (has_fileext(filename, "gdb")) {
        layer_options <- glue("LAYER_ALIAS={y}")
        y <- NULL
      }

      sf::write_sf(
        x,
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
  rlang::check_installed("gistr")

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
  rlang::check_installed("googlesheets4")

  if (!is.null(filename)) {
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
  # if (is.null(path)) {
  #   path <- getwd()
  #   cli_inform("Setting path to current working directory: {.file {path}}")
  # }

  # Set filename from path if ends with a fileext
  if (has_fileext(path)) {
    if (!is.null(filename)) {
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

  if (!is.null(path)) {
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

    if (has_fileext(filename, "gpkg") & !is.null(description)) {
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
    rlang::check_installed("readr")
  } else if (type %in% c("sf_excel", "df_excel")) {
    rlang::check_installed("openxlsx")
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
#' [write_sf_svg()] uses [ggplot2::geom_sf()] and [ggplot2::theme_void()] to
#' create a simple plot of an sf object and then save the plot as an svg file
#' using [ggplot2::ggsave()]. This function is convenient for working with
#' designers or other collaborators interested in using spatial data outside of
#' R or a desktop GIS application.
#'
#' @name write_sf_svg
#' @inheritParams ggplot2::geom_sf
#' @inheritDotParams ggplot2::geom_sf
#' @inheritParams ggplot2::ggsave
#' @export
#' @importFrom rlang check_installed check_required
write_sf_svg <- function(data,
                         filename = NULL,
                         path = NULL,
                         mapping = NULL,
                         ...,
                         scale = 1,
                         width = NA,
                         height = NA,
                         units = c("in", "cm", "mm", "px"),
                         dpi = 300) {
  rlang::check_installed("ggplot2")
  rlang::check_required(data)

  if (is.null(filename) && has_fileext(path)) {
    filename <- basename(path)
    path <- dirname(path)
  }

  cli_abort_ifnot(
    "{.arg filename} or {.arg path} must include a {.val svg} file extension.",
    condition = (str_extract_fileext(filename) == "svg")
  )

  if (is.null(mapping)) {
    mapping <- ggplot2::aes()
  }

  plot <- ggplot2::ggplot(data = data) +
    ggplot2::geom_sf(mapping = mapping, ...) +
    ggplot2::theme_void()

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    device = "svg",
    path = path,
    scale = scale,
    width = width,
    height = height,
    units = units,
    dpi = dpi
  )
}



#' Check if a file exists and remove file or error
#'
#' @noRd
#' @importFrom rlang caller_env is_interactive
check_file_overwrite <- function(filename = NULL,
                                 path = NULL,
                                 overwrite = TRUE,
                                 ask = TRUE,
                                 call = caller_env()) {
  filename <- filename %||% basename(path)
  filepath <- filename

  if (!is.null(path)) {
    if (has_fileext(path) && is.null(filename)) {
      filepath <- path
      path <- dirname(path)
    } else {
      filepath <- file.path(path, filename)
    }
  }

  cli_abort_ifnot(
    "{.arg filename} or {.arg path} must include a valid file type.",
    condition = has_fileext(filepath),
    call = call
  )

  if (file.exists(filepath)) {
    if (!overwrite && ask && rlang::is_interactive()) {
      overwrite <-
        cli_yesno(
          c(
            "i" = "A file with the same name exists in {.path {path}}",
            ">" = "Do you want to overwrite {.val {filename}}?"
          )
        )
    }

    cli_abort_ifnot(
      c(
        "!" = "{.file {filename}} can't be saved.",
        "i" = "A file with the same name already exists.
        Set {.code overwrite = TRUE} to remove."
      ),
      condition = overwrite,
      call = call
    )

    cli_inform(
      c("v" = "Removing {.path {filepath}}")
    )

    file.remove(filepath)
  }

  invisible(NULL)
}
