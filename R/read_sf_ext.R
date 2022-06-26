#' Read spatial data in a bounding box to a simple feature object
#'
#' An extended version of [sf::read_sf()] that support reading spatial data
#' based on a file path, URL, or the data name and associated package.
#' Optionally provide a bounding box to filter data (not supported for all data
#' types). If a file path or url is provided for a GeoJSON file, the
#' [read_sf_geojson] function (using the suggested
#' [{geojsonsf}](https://github.com/SymbolixAU/geojsonsf) package) is used.
#'
#' @details Reading data from a url:
#'
#' [read_sf_url] supports multiple types of urls:
#'
#'   - A MapServer or FeatureServer URL
#'   - A URL for a GitHub gist with a single spatial data file (first file used if gist contains multiple)
#'   - A URL for a spatial data file or a CSV file
#'   - A Google Sheets URL
#'   - A public Google Maps URL
#'
#' @details Reading data from a package:
#'
#' [read_sf_pkg] looks for three types of package data:
#'
#'   - Data loaded with the package
#'   - External data in the `extdata` system files folder.
#'   - Cached data in the cache directory returned by [rappdirs::user_cache_dir]
#'
#' @details Additional ... parameters:
#'
#' [read_sf_pkg] and [read_sf_download] both pass additional parameters
#' to [read_sf_path] which supports query, name_col, name, and table. name and
#' name_col are ignored if a query parameter is provided. If table is not
#' provided, a expected layer name is created based on the file path.
#'
#' [read_sf_url] pass the where, name_col, and name for any ArcGIS FeatureServer or
#' MapServer url (passed to [get_esri_data]) or sheet if the url is for a Google
#' Sheet (passed to [googlesheets4::read_sheet]), or a query or wkt filter
#' parameter if the url is some other type (passed to [sf::read_sf]).
#'
#' [read_sf_ext] is a flexible function that only has bbox as a named parameter
#' and all other parameters in ... are passed to one of the other overedge
#' read_sf functions.
#'
#' @param bbox A bounding box object; defaults to `NULL`. If `"bbox"` is provided,
#'   only returns features intersecting the bounding box.
#' @param path A file path.
#' @param url A url for a spatial data file, a GitHub gist, or a ArcGIS
#'   FeatureServer or MapServer to access with [get_esri_data()]
#' @param data Character; name of dataset; used by [read_sf_pkg()] only.
#' @param package Character; package name; used by [read_sf_pkg()] only.
#' @param filetype File type supported for dsn parameter of [sf::read_sf()];
#'   Default: 'gpkg'; used by [read_sf_pkg()] only and required only if the data
#'   is in the package cache directory or extdata system files.
#' @param coords Character vector with coordinate values. Coordinates must be
#'   latlon data (with coordinate reference system 4326).
#' @param geo If `TRUE`, use [address_to_sf] to geocode address column; defaults
#'   to `FALSE`.
#' @inheritParams address_to_sf
#' @inheritParams df_to_sf
#' @param ... Additional parameters passed to multiple functions; see details.
#' @name read_sf_ext
#' @family read_write
#' @export
#' @importFrom rlang list2 exec
#' @importFrom dplyr case_when
#' @importFrom cli cli_abort
read_sf_ext <- function(..., bbox = NULL) {
  params <- rlang::list2(...)

  read_sf_fn <-
    dplyr::case_when(
      !is.null(params$package) ~ "pkg",
      !is.null(params$url) ~ "url",
      !is.null(params$path) && is.null(params$filename) ~ "path",
      TRUE ~ "missing"
    )

  if (read_sf_fn == "missing") {
    cli::cli_abort("The parameters provided did not match any overedge read_sf function.
                   Are you missing a {.arg package}, {.arg url}, or {.arg path} argument?")
  }

  read_sf_fn <-
    switch(read_sf_fn,
      "path" = read_sf_path,
      "pkg" = read_sf_pkg,
      "url" = read_sf_url
    )

  args <-
    modify_fn_fmls(
      params = params,
      fn = read_sf_fn,
      missing = TRUE
    )

  rlang::exec(read_sf_fn, !!!args)
}

#' @name read_sf_pkg
#' @rdname read_sf_ext
#' @export
#' @md
#' @importFrom utils data
#' @importFrom cli cli_abort
#' @importFrom dplyr case_when
read_sf_pkg <- function(data, bbox = NULL, package = NULL, filetype = "gpkg", ...) {
  stopifnot(
    "The `package` argument is missing." = !is.null(package)
  )

  is_pkg_installed(package)

  # Read package data
  if (data %in% ls_pkg_data(package)) {
    return(use_eval_parse(data = data, package = package))
  }

  # FIXME: This triggers an alert with lintr but works fine
  filename <- str_add_filetype(data, filetype = filetype)

  path <-
    dplyr::case_when(
      # If data is in extdata folder
      filename %in% ls_pkg_extdata(package) ~ system.file("extdata", filename, package = package),
      # If data is in the cache directory
      filename %in% ls_pkg_cache(package) ~ file.path(get_data_dir(package = package), filename)
    )

  read_sf_path(path = path, bbox = bbox, ...)
}

#' @name read_sf_path
#' @rdname read_sf_ext
#' @export
#' @importFrom sf read_sf
read_sf_path <- function(path, bbox = NULL, ...) {
  stopifnot(
    "No file exists at `path` provided." = fs::file_exists(path)
  )

  filetype <- str_extract_filetype(path)

  if (filetype %in% c("csv", "xlsx", "xls", "geojson")) {
    data <-
      switch(filetype,
        "csv" = read_sf_csv(path = path, bbox = bbox, ...),
        "xlsx" = read_sf_excel(path = path, bbox = bbox, ...),
        "xls" = read_sf_excel(path = path, bbox = bbox, ...),
        "geojson" = read_sf_geojson(geojson = path, bbox = bbox, ...)
      )
  }

  read_sf_query(path = path, bbox = bbox, ...)
}


#' @noRd
read_sf_query <- function(path, bbox = NULL, query = NA, table = NULL, name = NULL, name_col = NULL, wkt_filter = character(0), ...) {
  if (!is.null(name) && !is.null(name_col)) {
    if (is.null(table)) {
      table <-
        stringr::str_extract(
          basename(path),
          "[:graph:]+(?=\\.)"
        )
    }

    query <-
      glue::glue("select * from {table} where {name_col} = '{name}'")
  }

  if (!is.null(bbox)) {
    # Convert bbox to well known text
    wkt_filter <- sf_bbox_to_wkt(bbox = bbox)
  }

  # Read external, cached, or data at path with wkt_filter
  sf::read_sf(
    dsn = path,
    wkt_filter = wkt_filter,
    query = query,
    ...
  )
}

#' @name read_sf_excel
#' @rdname read_sf_ext
#' @inheritParams readxl::read_excel
#' @export
#' @importFrom rlang list2
#' @importFrom purrr map
read_sf_excel <- function(path, sheet = NULL, bbox = NULL, coords = c("lon", "lat"), geo = FALSE, address = "address", from_crs = 4326, ...) {
  is_pkg_installed("readxl")
  # Convert XLS or XLSX file with coordinates to sf

  if (!is.null(sheet) && length(sheet) > 1) {
    params <- rlang::list2(...)

    data <- purrr::map(
      sheet,
      ~ read_sf_excel(
        path = path,
        sheet = .x,
        bbox = bbox,
        coords = coords,
        col_types = params$col_types
      )
    )

    return(data)
  }

  data <- readxl::read_excel(path = path, sheet = sheet, ...)

  data <- df_to_sf(data, coords = coords, geo = geo, address = address, from_crs = from_crs)

  bbox_filter(data, bbox = bbox)
}

#' @name read_sf_csv
#' @rdname read_sf_ext
#' @inheritParams readr::read_csv
#' @export
read_sf_csv <- function(path, url = NULL, bbox = NULL, coords = c("lon", "lat"), geo = FALSE, address = "address", show_col_types = FALSE, from_crs = 4326, ...) {
  if (rlang::is_missing(path) && !is.null(url)) {
    path <- url
  }

  data <- readr::read_csv(file = path, show_col_types = show_col_types, ...)

  data <- df_to_sf(data, coords = coords, geo = geo, address = address, crs = NULL, from_crs = from_crs)

  bbox_filter(data, bbox = bbox)
}

#' @name read_sf_url
#' @rdname read_sf_ext
#' @param zm_drop If `TRUE`, drop Z and/or M dimensions using [sf::st_zm]
#' @export
#' @importFrom rlang list2
#' @importFrom stringr str_detect
#' @importFrom sf read_sf st_zm
#' @importFrom dplyr case_when
read_sf_url <- function(url, bbox = NULL, coords = NULL, zm_drop = TRUE, ...) {
  params <- rlang::list2(...)

  stopifnot(
    is_url(url)
  )

  url_type <-
    dplyr::case_when(
      is_esri_url(url) ~ "esri",
      is_gsheet_url(url) ~ "gsheet",
      is_gist_url(url) ~ "gist",
      is_gmap_url(url) ~ "gmap",
      stringr::str_detect(url, "\\.csv$") ~ "csv",
      stringr::str_detect(url, "\\.geojson$") ~ "geojson",
      !is.null(params$filename) ~ "download",
      TRUE ~ "other"
    )

  if (url_type != "other") {
    data <-
      switch(url_type,
        "esri" = get_esri_data(
          url = url,
          location = bbox,
          name_col = params$name_col,
          name = params$name,
          where = params$where
        ),
        "gsheet" = read_sf_gsheet(
          url = url,
          bbox = bbox,
          coords = coords,
          sheet = params$sheet
        ),
        "gist" = read_sf_gist(
          url = url,
          bbox = bbox
        ),
        "gmap" = read_sf_gmap(
          url = url,
          bbox = bbox
        ),
        "csv" = read_sf_csv(
          url = url,
          bbox = bbox,
          coords = coords
        ),
        "geojson" = read_sf_geojson(
          url = url,
          bbox = bbox
        ),
        "download" = read_sf_download(
          url = url,
          filename = params$filename,
          bbox = bbox,
          path = params$path
        )
      )

    return(data)
  }

  if (is.null(params$query)) {
    params$query <- NA
  }

  if (is.null(params$wkt_filter)) {
    params$wkt_filter <- character(0)
  }

  data <- sf::read_sf(
    dsn = url,
    query = params$query,
    wkt_filter = params$wkt_filter
  )

  if (zm_drop) {
    data <- sf::st_zm(data)
  }

  bbox_filter(data, bbox = bbox)
}


#' @name read_sf_geojson
#' @rdname read_sf_ext
#' @inheritParams geojsonsf::geojson_sf
#' @export
#' @importFrom rlang is_missing
read_sf_geojson <- function(url,
                            path = NULL,
                            geojson = NULL,
                            bbox = NULL,
                            ...) {
  is_pkg_installed("geojsonsf")

  if (!is.null(path)) {
    url <- path
  } else if (!is.null(geojson)) {
    url <- geojson
  }

  stopifnot(
    "geojson `url` or `path` is missing." = !rlang::is_missing(url)
  )

  data <-
    suppressWarnings(
      geojsonsf::geojson_sf(geojson = url, ...)
    )

  bbox_filter(data, bbox = bbox)
}

#' @name read_sf_gist
#' @rdname read_sf_ext
#' @inheritParams gistr::gist
#' @export
read_sf_gist <- function(url,
                         id = NULL,
                         bbox = NULL,
                         ...) {
  is_pkg_installed("gistr")

  if (!rlang::is_missing(url) && is.null(id)) {
    id <- url
  }

  gist_data <-
    gistr::gist(
      id = id
    )

  stopifnot(
    !is.null(gist_data$files)
  )

  if (length(gist_data$files) > 1) {
    cli::cli_alert_danger("This gist has {length(gist_data$files)} files but only the first file is read.")
  }

  url <- gist_data$files[[1]]$raw_url

  read_sf_url(url = url, bbox = bbox, ...)
}


#' @name read_sf_gmap
#' @rdname read_sf_ext
#' @export
#' @importFrom sf st_layers
#' @importFrom purrr map_dfr
#' @importFrom cli cli_progress_along
read_sf_gmap <- function(url,
                         bbox = NULL,
                         zm_drop = TRUE) {
  url <- make_gmap_url(url)

  layers <-
    sf::st_layers(
      dsn = url
    )

  data <-
    purrr::map_dfr(
      cli::cli_progress_along(
        layers[["name"]],
        "Downloading map layers"
      ),
      function(x) {
        sf::read_sf(
          dsn = url,
          layer = layers[["name"]][x]
        )
      }
    )

  is_pkg_installed("naniar")

  data <-
    naniar::replace_with_na(data, replace = list("Description" = ""))


  if (zm_drop) {
    data <- sf::st_zm(data)
  }

  bbox_filter(data, bbox = bbox)
}

#' Get map ID from url
#' @noRd
get_gmap_id <- function(url) {
  stringr::str_extract(
    url,
    "(?<=mid=)[:graph:]+(?=&)"
  )
}

#' Make a Google Maps KML format URL
#' @noRd
make_gmap_url <- function(url = NULL, mid = NULL, format = "kml") {
  stopifnot(
    is_gmap_url(url)
  )

  if (!is.null(url)) {
    mid <- get_gmap_id(url)
  }

  if (format == "kml") {
    glue::glue("https://www.google.com/maps/d/u/0/kml?forcekml=1&mid={mid}")
  }
}


#' @name read_sf_download
#' @rdname read_sf_ext
#' @param unzip If `TRUE`, url must be a zip file that is downloaded, unzipped
#'   into a temporary directory (created with [tempdir()]), and then read to a file using the specified
#'   file type.
#' @inheritParams utils::download.file
#' @inheritParams get_data_dir
#' @inheritParams make_filename
#' @export
#' @importFrom sf st_crs
#' @importFrom utils download.file unzip
read_sf_download <-
  function(url,
           filename,
           bbox = NULL,
           path = NULL,
           filetype = "geojson",
           prefix = "date",
           method = "auto",
           unzip = FALSE,
           ...) {
    # TODO: Update read_sf_download to support unzipping downloads and reading files from folder
    path <- get_data_dir(path = path)

    destfile <-
      make_filename(
        prefix = prefix,
        filename = filename,
        path = path,
        filetype = filetype
      )

    utils::download.file(
      url = url,
      destfile = destfile,
      method = method
    )

    if (unzip) {
      zipdest <-
        make_filename(
          prefix = prefix,
          filename = filename,
          path = tempdir(),
          filetype = filetype
        )

      utils::unzip(
        zipfile = destfile,
        exdir = tempdir(),
        overwrite = TRUE
      )

      destfile <- zipdest
    }


    read_sf_path(path = destfile, bbox = bbox, ...)
  }

#' @name read_sf_gsheet
#' @rdname read_sf_ext
#' @inheritParams googlesheets4::read_sheet
#' @param ask If `TRUE`, ask for the name of the Google Sheet to read if ss is
#'   not provided to [read_sf_gsheet].
#' @export
#' @importFrom rlang is_missing
read_sf_gsheet <- function(url, sheet = NULL, ss = NULL, bbox = NULL, coords = c("lon", "lat"), ask = FALSE, geo = FALSE, address = "address", from_crs = 4326, ...) {
  # Convert Google Sheet with coordinates to sf
  is_pkg_installed("googlesheets4")

  if (is.null(ss)) {
    if (!rlang::is_missing(url)) {
      ss <- url
    } else if (ask) {
      ss <-
        googlesheets4::gs4_find(cli_ask("What is the name of the Google Sheet to return?"))
    }
  }

  data <- googlesheets4::read_sheet(ss = ss, sheet = sheet, ...)

  data <- df_to_sf(data, coords = coords, geo = geo, address = address, from_crs = from_crs)

  bbox_filter(data, bbox = bbox)
}

#' Join data from a Google Sheet to a simple feature object
#'
#' @noRd
#' @importFrom dplyr left_join
#' @importFrom sf st_drop_geometry
join_sf_gsheet <- function(data, ss = NULL, sheet = 1, key = NULL, suffix = c("", "_gsheet")) {
  if (cli_yeah("Are you ready to sync from Google Sheets back to an sf object?")) {
    sheet_data <-
      sf::st_drop_geometry(
        read_sf_gsheet(
          ss = ss,
          sheet = sheet,
          ask = TRUE
        )
      )

    if (!is.null(key)) {
      data <-
        dplyr::left_join(
          sheet_data,
          data,
          by = key,
          suffix = suffix
        )
    }
  }

  return(data)
}
