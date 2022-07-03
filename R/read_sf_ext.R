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
#' MapServer url (passed to [read_sf_esri]) or sheet if the url is for a Google
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
#' @param url A url for a spatial data file, tabular data with coordinates, or a
#'   ArcGIS FeatureServer or MapServer to access with [esri2sf::esri2sf()]
#' @param data Name of a package dataset; used by [read_sf_pkg()] only.
#' @param package Package name; used by [read_sf_pkg()] only.
#' @param filetype File type supported by [sf::read_sf()]; Default: 'gpkg'; used
#'   by [read_sf_pkg()] only and required only if the data is in the package
#'   cache directory or extdata system files.
#' @param coords Character vector with coordinate values. Coordinates must use
#'   the same crs as the `from_crs` parameter.
#' @param geo If `TRUE`, use [address_to_sf] to geocode address column; defaults
#'   to `FALSE`.
#' @inheritParams address_to_sf
#' @inheritParams df_to_sf
#' @param ... Additional parameters passed to multiple functions; see details.
#' @name read_sf_ext
#' @family read_write
#' @export
#' @importFrom dplyr case_when
read_sf_ext <- function(..., bbox = NULL) {
  params <- list2(...)

  read_sf_fn <-
    dplyr::case_when(
      !is.null(params$package) ~ "pkg",
      !is.null(params$url) ~ "url",
      !is.null(params$path) ~ "path",
      !is.null(params$dsn) ~ "sf",
      TRUE ~ "missing"
    )

  cli_abort_ifnot(
    c("The parameters provided can't be matched to a function.",
      "i" = "You must provide a {.arg url}, {.arg path}, {.arg package}, or {.arg dsn} argument."
    ),
    condition = (read_sf_fn != "missing")
  )

  read_sf_fn <-
    switch(read_sf_fn,
      "path" = read_sf_path,
      "pkg" = read_sf_pkg,
      "url" = read_sf_url,
      "sf" = read_sf_query
    )

  args <-
    modify_fn_fmls(
      params = params,
      fn = read_sf_fn,
      missing = TRUE
    )

  exec(read_sf_fn, !!!args)
}


#' Modify function parameters
#'
#' @noRd
#' @importFrom purrr discard
#' @importFrom utils modifyList
modify_fn_fmls <- function(params, fn, keep_missing = FALSE, keep.null = FALSE, ...) {
  fmls <- fn_fmls(fn)

  if (!keep_missing) {
    fmls <- purrr::discard(fmls, is_missing)
  }

  params <- c(list2(...), params)

  utils::modifyList(
    fmls,
    params,
    keep.null = keep.null
  )
}


#' @name read_sf_pkg
#' @rdname read_sf_ext
#' @export
#' @importFrom dplyr case_when
read_sf_pkg <- function(data, bbox = NULL, package = NULL, filetype = "gpkg", ...) {
  check_null(package)

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
#' @importFrom fs file_exists
read_sf_path <- function(path, bbox = NULL, ...) {
  cli_abort_ifnot(
    "Can't find {.path {path}}.",
    condition = fs::file_exists(path)
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

    return(data)
  }

  read_sf_query(path = path, bbox = bbox, ...)
}

#' @name read_sf_query
#' @rdname read_sf_ext
#' @inheritParams sf::read_sf
#' @export
#' @importFrom stringr str_extract
#' @importFrom sf read_sf st_zm
read_sf_query <- function(path,
                          dsn = NULL,
                          bbox = NULL,
                          query = NA,
                          table = NULL,
                          name = NULL,
                          name_col = NULL,
                          wkt_filter = character(0),
                          zm_drop = FALSE,
                          ...) {
  if (!is_missing(path)) {
    dsn <- path
  }

  if (!is.null(name) && !is.null(name_col)) {
    if (is.null(table)) {
      table <-
        stringr::str_extract(
          basename(path),
          "[:graph:]+(?=\\.)"
        )
    }

    query <-
      glue("select * from {table} where {name_col} = '{name}'")
  }

  if (!is.null(bbox)) {
    # Convert bbox to well known text
    wkt_filter <- sf_bbox_to_wkt(bbox = bbox)
  }

  if (is.null(query)) {
    query <- NA
  }

  if (is.null(wkt_filter)) {
    wkt_filter <- character(0)
  }

  # Read external, cached, or data at path with wkt_filter
  data <-
    sf::read_sf(
      dsn = dsn,
      wkt_filter = wkt_filter,
      query = query
    )

  if (!zm_drop) {
    return(data)
  }

  sf::st_zm(data)
}

#' @name read_sf_excel
#' @rdname read_sf_ext
#' @inheritParams readxl::read_excel
#' @export

#' @importFrom purrr map
read_sf_excel <- function(path,
                          sheet = NULL,
                          bbox = NULL,
                          coords = c("lon", "lat"),
                          from_crs = 4326,
                          geo = FALSE,
                          address = "address",
                          ...) {
  is_pkg_installed("readxl")
  # Convert XLS or XLSX file with coordinates to sf

  if (!is.null(sheet) && length(sheet) > 1) {
    params <- list2(...)

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

  st_filter_ext(data, bbox, crop = TRUE)
}

#' @name read_sf_csv
#' @rdname read_sf_ext
#' @inheritParams readr::read_csv
#' @export
read_sf_csv <- function(path,
                        url = NULL,
                        bbox = NULL,
                        coords = c("lon", "lat"),
                        from_crs = 4326,
                        geo = FALSE,
                        address = "address",
                        show_col_types = FALSE,
                        ...) {
  if (is_missing(path) && !is.null(url)) {
    path <- url
  }

  is_pkg_installed("readr")

  data <- readr::read_csv(file = path, show_col_types = show_col_types, ...)

  data <- df_to_sf(data, coords = coords, geo = geo, address = address, crs = NULL, from_crs = from_crs)

  st_filter_ext(data, bbox, crop = TRUE)
}

#' @name read_sf_url
#' @rdname read_sf_ext
#' @param zm_drop If `TRUE`, drop Z and/or M dimensions using [sf::st_zm]
#' @export
#' @importFrom stringr str_detect
#' @importFrom sf read_sf st_zm
#' @importFrom dplyr case_when
read_sf_url <- function(url,
                        bbox = NULL,
                        coords = NULL,
                        zm_drop = TRUE,
                        ...) {
  params <- list2(...)

  cli_abort_ifnot(
    c("{.arg url} must be a valid url."),
    condition = is_url(url)
  )

  url_type <-
    dplyr::case_when(
      is_csv_path(url) ~ "csv",
      !is.null(params$filename) ~ "download",
      is_esri_url(url) ~ "esri",
      is_geojson_path(url) ~ "geojson",
      is_gist_url(url) ~ "gist",
      is_gmap_url(url) ~ "gmap",
      is_gsheet_url(url) ~ "gsheet",
      TRUE ~ "other"
    )

  switch(url_type,
    "csv" = read_sf_csv(
      url = url,
      bbox = bbox,
      coords = coords
    ),
    "download" = read_sf_download(
      url = url,
      filename = params$filename,
      bbox = bbox,
      path = params$path
    ),
    "esri" = read_sf_esri(
      url = url,
      location = bbox,
      where = params$where,
      name = params$name,
      name_col = params$name_col
    ),
    "geojson" = read_sf_geojson(
      url = url,
      bbox = bbox
    ),
    "gist" = read_sf_gist(
      url = url,
      bbox = bbox
    ),
    "gmap" = read_sf_gmap(
      url = url,
      bbox = bbox,
      zm_drop = zm_drop
    ),
    "gsheet" = read_sf_gsheet(
      url = url,
      bbox = bbox,
      coords = coords,
      sheet = params$sheet
    ),
    "other" = read_sf_query(
      dsn = url,
      bbox = bbox,
      query = params$query,
      wkt_filter = params$wkt_filter,
      table = params$table,
      name = params$name,
      name_col = params$name_col,
      zm_drop = zm_drop
    )
  )
}

#' @name read_sf_esri
#' @rdname read_sf_ext
#' @export
read_sf_esri <- function(url,
                         bbox = NULL,
                         where = NULL,
                         name = NULL,
                         name_col = NULL,
                         coords = c("lon", "lat"),
                         from_crs = 4326,
                         ...) {
  is_pkg_installed(pkg = "esri2sf", repo = "yonghah/esri2sf")

  meta <- esri2sf::esrimeta(url)

  if (!is.null(where)) {
    where <- paste0("(", where, ")")
  }

  if (!is.null(name) && !is.null(name_col)) {
    where <- c(where, glue("({name_col} = '{name}')"))
  }

  if (!is.null(bbox) && !is.null(coords) && (meta$type == "Table")) {
    where <-
      c(where, sf_bbox_to_lonlat_query(bbox = bbox, coords = coords))
  }

  if (!is.null(where)) {
    where <- paste(where[!is.na(where)], collapse = " AND ")
  }

  if (meta$type != "Table") {
    # Get FeatureServer with geometry
    return(
      esri2sf::esri2sf(
        url = url,
        where = where,
        bbox = bbox,
        crs = NULL,
        progress = TRUE,
        ...
      )
    )
  }

  # Get Table (no geometry) by filtering coordinate columns with bbox
  data <-
    esri2sf::esri2df(
      url = url,
      where = where,
      progress = TRUE,
      ...
    )

  if (is.null(coords)) {
    return(data)
  }

  df_to_sf(data, from_crs = from_crs, coords = coords)
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

  cli_abort_ifnot(
    "A {.arg url}, {.arg path}, or {.arg geojson} parameter must be provided.",
    condition = !is_missing(url)
  )

  data <-
    suppressWarnings(
      geojsonsf::geojson_sf(geojson = url, ...)
    )

  st_filter_ext(data, bbox, crop = TRUE)
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

  if (!is_missing(url) && is.null(id)) {
    id <- url
  }

  gist_data <-
    gistr::gist(
      id = id
    )

  check_null(gist_data$files)

  if (length(gist_data$files) > 1) {
    cli_warn("This gist has {length(gist_data$files)} files but only the first file is read.")
  }

  read_sf_url(
    url = gist_data$files[[1]]$raw_url,
    bbox = bbox,
    ...
  )
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

  st_filter_ext(data, bbox, crop = TRUE)
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
  cli_abort_ifnot(
    "{.arg url} must be a valid Google Maps url.",
    condition = is_gmap_url(url)
  )

  if (!is.null(url)) {
    mid <- get_gmap_id(url)
  }

  if (format == "kml") {
    glue("https://www.google.com/maps/d/u/0/kml?forcekml=1&mid={mid}")
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
read_sf_gsheet <- function(url,
                           sheet = NULL,
                           ss = NULL,
                           bbox = NULL,
                           ask = FALSE,
                           coords = c("lon", "lat"),
                           from_crs = 4326,
                           geo = FALSE,
                           address = "address",
                           ...) {
  # Convert Google Sheet with coordinates to sf
  is_pkg_installed("googlesheets4")

  if (is.null(ss)) {
    if (!is_missing(url)) {
      ss <- url
    } else if (ask) {
      ss <-
        googlesheets4::gs4_find(cli_ask("What is the name of the Google Sheet to return?"))
    }
  }

  data <- googlesheets4::read_sheet(ss = ss, sheet = sheet, ...)

  data <- df_to_sf(data, coords = coords, geo = geo, address = address, from_crs = from_crs)

  st_filter_ext(data, bbox, crop = TRUE)
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

  data
}
