#' Read spatial data in a bounding box to a simple feature object from multiple
#' sources
#'
#' An extended version of [sf::read_sf()] that support reading spatial data
#' based on a file path, URL, or the data name and associated package. A RDS,
#' RDA, or RData file Optionally provide a bounding box to filter data (data is
#' filtered before download or reading into memory where possible).
#'
#' @details Reading data from a url:
#'
#' [read_sf_url()] supports multiple types of urls:
#'
#'   - A MapServer or FeatureServer URL
#'   - A URL for a GitHub gist with a single spatial data file (first file used
#'   if gist contains multiple)
#'   - A URL for a spatial data file, CSV file, Excel file, or RDS file (RDA and
#'   RData files supported by [read_sf_path()])
#'   - A Google Sheets URL
#'   - A public Google Maps URL
#'
#' @section Reading data from a package:
#'
#' [read_sf_pkg()] looks for three types of package data:
#'
#'   - Data loaded with the package
#'   - External data in the `extdata` system files folder.
#'   - Cached data in the cache directory returned by
#'   [rappdirs::user_cache_dir()]
#'
#' @section Additional ... parameters:
#'
#'   [read_sf_ext()] is a flexible function where ... are passed to one of the
#'   other read functions depending on the provided parameters. If you are using
#'   more than one parameter, all parameters *must* be named.
#'
#'   [read_sf_pkg()] and [read_sf_download()] both pass additional parameters to
#'   [read_sf_path()] which supports query, name_col, name, and table. name and
#'   name_col are ignored if a query parameter is provided. If table is not
#'   provided, a expected layer name is created based on the file path.
#'
#'   [read_sf_url()] pass the where, name_col, and name for any ArcGIS
#'   FeatureServer or MapServer url (passed to [read_sf_esri()]) or sheet if the
#'   url is for a Google Sheet (passed to [googlesheets4::read_sheet()]), or a
#'   query or wkt filter parameter if the url is some other type (passed to
#'   [sf::read_sf()]).
#'
#' @param bbox A bounding box object; defaults to `NULL`. If `"bbox"` is
#'   provided, only returns features intersecting the bounding box.
#' @param path A file path.
#' @param url A url for a spatial data file, tabular data with coordinates, or a
#'   ArcGIS FeatureServer or MapServer to access with [esri2sf::esri2sf()]
#' @param data Name of a package dataset; used by [read_sf_pkg()] only.
#' @param package,pkg Package name; used by [read_sf_pkg()] only. pkg is used if
#'   package is `NULL`.
#' @param filetype File type supported by [sf::read_sf()]; Default: 'gpkg'; used
#'   by [read_sf_pkg()] only and required only if the data is in the package
#'   cache directory or extdata system files.
#' @param coords Character vector with coordinate values. Coordinates must use
#'   the same crs as the `from_crs` parameter.
#' @param geo If `TRUE`, use [address_to_sf()] to geocode address column;
#'   defaults to `FALSE`.
#' @param combine_layers,combine_sheets If `FALSE` (default), return a list with
#'   a sf object for each layer or sheet as a separate item. If `TRUE`, use
#'   [purrr::map_dfr()] to combine layers or sheets into a single sf object
#'   using the layer or sheet name as an additional column.
#' @inheritParams utils::download.file
#' @inheritParams address_to_sf
#' @inheritParams df_to_sf
#' @param ... Additional parameters passed to multiple functions; see details.
#' @name read_sf_ext
#' @family read_write
#' @export
#' @importFrom rlang list2 is_named is_null exec
#' @importFrom dplyr case_when
read_sf_ext <- function(...) {
  params <- list2(...)

  if (!is_named(params[1])) {
    names(params)[1] <-
      dplyr::case_when(
        is_url(params[[1]]) ~ "url",
        has_fileext(params[[1]]) ~ "path",
        has_name(params, "package") ~ "data",
        TRUE ~ "dsn"
      )
  }

  type <-
    dplyr::case_when(
      !is_null(params[["url"]]) ~ "url",
      !is_null(params[["path"]]) ~ "path",
      !is_null(params[["package"]]) ~ "pkg",
      !is_null(params[["dsn"]]) ~ "sf",
      TRUE ~ "sf"
    )

  read_sf_fn <-
    switch(type,
      "path" = read_sf_path,
      "pkg" = read_sf_pkg,
      "url" = read_sf_url,
      "sf" = read_sf_query
    )

  # FIXME: read_sf_ext has an issue with passing parameters that it shouldn't
  # Adding a path = NULL parameter to read_sf_pkg may fix one of the issues
  # temporarily but modify_fn_fmls needs an overhaul
  args <-
    modify_fn_fmls(
      params = params,
      fn = read_sf_fn,
      keep_missing = TRUE
    )

  exec(read_sf_fn, !!!args)
}


#' Modify function parameters
#'
#' @keywords internal
#' @noRd
#' @importFrom utils modifyList
#' @importFrom rlang fn_fmls list2
modify_fn_fmls <- function(params,
                           fn,
                           keep_missing = FALSE,
                           keep.null = FALSE,
                           ...) {
  fmls <- fn_fmls(fn)

  if (is_false(keep_missing)) {
    fmls <- discard(fmls, is_missing)
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
read_sf_pkg <- function(data,
                        bbox = NULL,
                        package = NULL,
                        pkg = NULL,
                        fileext = "gpkg",
                        filetype = NULL,
                        ...) {
  fileext <- fileext %||% filetype
  package <- package %||% pkg
  check_string(package, allow_empty = FALSE)
  check_installed(package)
  check_string(
    data,
    allow_empty = FALSE,
    message = cli::cli_text(
      "{.arg data} must be a string with the name or filename of data from the
      package {.pkg {package}}."
    ),
    use_cli_format = TRUE
  )

  # Read package data
  if (is_pkg_data(data, package)) {
    # FIXME: Check equivalency to readr::read_builtin
    return(use_eval_parse(data = data, package = package))
  }

  filename <- str_add_fileext(data, fileext = fileext)

  path <-
    dplyr::case_when(
      # If data is in extdata folder
      is_pkg_extdata(filename, package) ~ system.file(
        "extdata",
        filename,
        package = package
      ),
      # If data is in the cache directory
      is_pkg_cachedata(filename, package) ~ file.path(
        filenamr::get_data_dir(cache = TRUE, create = FALSE, pkg = package),
        filename
      )
    )

  read_sf_path(path = path, bbox = bbox, ...)
}

#' @name read_sf_path
#' @rdname read_sf_ext
#' @export
#' @importFrom sf read_sf
read_sf_path <- function(path, bbox = NULL, ...) {
  cli_abort_ifnot(
    "{.arg path} can't be found at {.path {path}}.",
    condition = file.exists(path)
  )

  type <-
    dplyr::case_when(
      is_csv_fileext(path) ~ "csv",
      is_excel_fileext(path) ~ "excel",
      is_rdata_fileext(path) ~ "rdata",
      TRUE ~ "query"
    )

  switch(type,
    "csv" = read_sf_csv(path = path, bbox = bbox, ...),
    "excel" = read_sf_excel(path = path, bbox = bbox, ...),
    "rdata" = read_sf_rdata(path = path, bbox = bbox, ...),
    "query" = read_sf_query(path = path, bbox = bbox, ...)
  )
}

#' @name read_sf_rdata
#' @rdname read_sf_ext
#' @inheritParams readr::read_rds
#' @export
#' @importFrom cliExtras cli_warning_ifnot
read_sf_rdata <- function(path,
                          file = NULL,
                          refhook = NULL,
                          bbox = NULL,
                          .name_repair = "check_unique",
                          ...) {
  file <- file %||% path

  type <-
    dplyr::case_when(
      is_rds_fileext(file) ~ "rds",
      is_rdata_fileext(file) ~ "rdata"
    )

  if (type == "rds") {
    check_installed("readr")
    data <- readr::read_rds(file, refhook = refhook)
  } else {
    file_name <- load(file)
    data <- get(file_name)
  }

  data <- set_names_repair(data, .name_repair = .name_repair)

  if (is_sf(data)) {
    return(st_filter_ext(data, bbox))
  }

  cliExtras::cli_warning_ifnot(
    "{.arg file} {.file {file}} is not a {.cls sf} object
      and can't be filtered by {.arg bbox}.",
    condition = is_null(bbox)
  )

  data
}

#' @name read_sf_query
#' @rdname read_sf_ext
#' @inheritParams sf::read_sf
#' @param name,name_col Name value and name column to use in generated a query
#'   for sources read with [read_sf_query()] or [read_sf_esri()].
#' @param table table can usually be inferred from basename of the data source.
#'   table is used to generate a custom query if both name and name_col are
#'   provided. Use `sf::st_layers(dsn = dsn)[["name"]]` to see a list of
#'   available table names.
#' @param .name_repair Passed to repair parameter of [vctrs::vec_as_names()]
#' @export
#' @importFrom sf st_layers read_sf st_zm
#' @importFrom rlang is_lambda as_function is_function set_names
read_sf_query <- function(path,
                          dsn = NULL,
                          bbox = NULL,
                          query = NULL,
                          table = NULL,
                          name = NULL,
                          name_col = NULL,
                          wkt_filter = NULL,
                          zm_drop = FALSE,
                          .name_repair = "check_unique",
                          ...) {
  dsn <- dsn %||% path

  query <-
    make_sf_query(
      dsn = dsn,
      table = table,
      name = name,
      name_col = name_col,
      query = query
    )

  wkt_filter <-
    make_sf_wkt_filter(
      wkt_filter = wkt_filter,
      bbox = bbox
    )

  # Read external, cached, or data at path with wkt_filter
  data <-
    sf::read_sf(
      dsn = dsn,
      wkt_filter = wkt_filter,
      query = query
    )

  data <-
    set_names_repair(data, .name_repair = .name_repair)

  if (is_false(zm_drop)) {
    return(data)
  }

  sf::st_zm(data)
}

#' @name read_sf_excel
#' @rdname read_sf_ext
#' @inheritParams readxl::read_excel
#' @export
#' @importFrom purrr map_dfr
read_sf_excel <- function(path,
                          sheet = NULL,
                          combine_sheets = FALSE,
                          bbox = NULL,
                          coords = c("lon", "lat"),
                          from_crs = 4326,
                          geo = FALSE,
                          address = "address",
                          .name_repair = "check_unique",
                          ...) {
  check_installed("readxl")

  # Convert XLS or XLSX file with coordinates to sf
  sheet <- sheet %||% readxl::excel_sheets(path)

  if (has_min_length(sheet, 2)) {
    params <- list2(...)

    map_fn <- map

    if (is_true(combine_sheets)) {
      map_fn <- purrr::map_dfr
    }

    return(
      map_fn(
        sheet,
        ~ read_sf_excel(
          path = path,
          sheet = .x,
          bbox = bbox,
          coords = coords,
          geo = geo,
          address = address,
          col_types = params[["col_types"]],
          .name_repair = .name_repair
        )
      )
    )
  }

  data_df <-
    readxl::read_excel(
      path = path,
      sheet = sheet,
      .name_repair = .name_repair,
      ...
    )

  if (is_null(coords) && is_false(geo)) {
    return(data_df)
  }

  data <-
    df_to_sf(
      data_df,
      coords = coords,
      from_crs = from_crs,
      geo = geo,
      address = address
    )

  if (!is_sf(data)) {
    return(data)
  }

  st_filter_ext(data, bbox)
}

#' @name read_sf_csv
#' @rdname read_sf_ext
#' @inheritParams readr::read_csv
#' @param wkt Name of column with well-known text for geometry. Used by
#'   [read_sf_csv()].
#' @export
read_sf_csv <- function(path,
                        url = NULL,
                        bbox = NULL,
                        coords = c("lon", "lat"),
                        from_crs = 4326,
                        geo = FALSE,
                        address = "address",
                        wkt = NULL,
                        .name_repair = "check_unique",
                        show_col_types = FALSE,
                        ...) {
  if (is_missing(path) && !is_null(url)) {
    path <- url
  }

  if (is_true(show_col_types)) {
    check_installed("readr")
    data <-
      readr::read_csv(
        file = path,
        show_col_types = show_col_types,
        name_repair = .name_repair,
        ...
      )
  } else {
    options <- make_sf_options(options, coords, wkt)

    data <-
      sf::read_sf(
        path,
        options = options,
        crs = from_crs %||% NA_character_,
        ...
      )

    data <-
      set_names_repair(
        data,
        .name_repair = .name_repair
      )
  }

  if (is_sf(data)) {
    return(st_filter_ext(data, bbox))
  }

  if (is_null(coords) && is_false(geo)) {
    return(data)
  }

  data <-
    df_to_sf(
      data,
      coords = coords,
      from_crs = from_crs,
      geo = geo,
      address = address
    )

  if (!is_sf(data)) {
    return(data)
  }

  st_filter_ext(data, bbox)
}

#' @name read_sf_url
#' @rdname read_sf_ext
#' @param zm_drop If `TRUE`, drop Z and/or M dimensions using [sf::st_zm]
#' @export
#' @importFrom sf read_sf st_zm
#' @importFrom dplyr case_when
read_sf_url <- function(url,
                        bbox = NULL,
                        coords = c("lon", "lat"),
                        ...) {
  params <- list2(...)

  cli_abort_ifnot(
    c("{.arg url} must be a valid url."),
    condition = is_url(url)
  )

  url_type <-
    dplyr::case_when(
      is_csv_fileext(url) ~ "csv",
      is_excel_fileext(url) ~ "excel",
      is_esri_url(url) ~ "esri",
      is_gist_url(url) ~ "gist",
      is_gmap_url(url) ~ "gmap",
      is_gsheet_url(url) ~ "gsheet",
      is_rds_fileext(url) ~ "rds",
      !is_null(params[["filename"]]) ~ "download",
      TRUE ~ "other"
    )

  # FIXME: If the defaults for the underlying functions ever change, these would
  # also need to be updated. Refactor to make sure that isn't an issue in the
  # future. Additional default setting for non-repeated parameters occurs in
  # function calls below
  from_crs <- params[["from_crs"]] %||% 4326
  zm_drop <- params[["zm_drop"]] %||% TRUE
  geo <- params[["geo"]] %||% FALSE
  address <- params[["address"]] %||% "address"
  wkt <- params[["wkt"]]
  .name_repair <- params[[".name_repair"]] %||% "check_unique"

  switch(url_type,
    "csv" = read_sf_csv(
      url = url,
      bbox = bbox,
      coords = coords,
      from_crs = from_crs,
      geo = geo,
      wkt = wkt,
      address = address,
      .name_repair = .name_repair
    ),
    "excel" = read_sf_excel(
      url = url,
      bbox = bbox,
      sheet = params[["sheet"]],
      combine_sheets = params[["combine_sheets"]] %||% FALSE,
      coords = coords,
      from_crs = from_crs,
      geo = geo,
      address = address,
      .name_repair = .name_repair
    ),
    "download" = read_sf_download(
      url = url,
      filename = params[["filename"]],
      bbox = bbox,
      path = params[["path"]],
      filetype = params[["filetype"]] %||% "geojson",
      prefix = params[["prefix"]] %||% "date",
      method = params[["method"]] %||% "auto",
      unzip = params[["unzip"]] %||% FALSE,
      .name_repair = .name_repair
    ),
    "esri" = read_sf_esri(
      url = url,
      bbox = bbox,
      where = params[["where"]],
      name = params[["name"]],
      name_col = params[["name_col"]],
      coords = coords,
      from_crs = from_crs,
      .name_repair = .name_repair
    ),
    "gist" = read_sf_gist(
      url = url,
      bbox = bbox,
      nth = params[["nth"]] %||% 1,
      .name_repair = .name_repair
    ),
    "gmap" = read_sf_gmap(
      url = url,
      bbox = bbox,
      layer = params[["layer"]],
      combine_layers = params[["combine_layers"]] %||% FALSE,
      zm_drop = zm_drop,
      .name_repair = .name_repair
    ),
    "gsheet" = read_sf_gsheet(
      url = url,
      sheet = params[["sheet"]],
      bbox = bbox,
      coords = coords,
      from_crs = from_crs,
      geo = geo,
      address = address,
      .name_repair = .name_repair
    ),
    "rds" = read_sf_rdata(
      file = url,
      bbox = bbox,
      refhook = params[["refhook"]],
      .name_repair = .name_repair
    ),
    "other" = read_sf_query(
      dsn = url,
      bbox = bbox,
      query = params[["query"]],
      wkt_filter = params[["wkt_filter"]],
      table = params[["table"]],
      name = params[["name"]],
      name_col = params[["name_col"]],
      zm_drop = zm_drop,
      .name_repair = .name_repair
    )
  )
}

#' @name read_sf_esri
#' @rdname read_sf_ext
#' @inheritParams esri2sf::esri2sf
#' @export
read_sf_esri <- function(url,
                         bbox = NULL,
                         where = NULL,
                         name = NULL,
                         name_col = NULL,
                         coords = c("lon", "lat"),
                         from_crs = 4326,
                         .name_repair = "check_unique",
                         ...) {
  check_dev_installed(pkg = "esri2sf", repo = "elipousson/esri2sf")

  meta <- esri2sf::esrimeta(url)

  is_feature_layer <-
    !any(c(is_null(meta[["geometryType"]]), (meta[["geometryType"]] == "")))

  if (is_feature_layer) {
    coords <- NULL
  }

  where <- make_where_query(where, name, name_col, bbox, coords)

  if (is_feature_layer) {
    # Get FeatureServer with geometry
    return(
      esri2sf::esri2sf(
        url = url,
        where = where,
        geometry = bbox,
        crs = NULL,
        progress = TRUE,
        .name_repair = .name_repair,
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
      .name_repair = .name_repair,
      ...
    )

  if (is_null(coords)) {
    return(data)
  }

  df_to_sf(data, from_crs = from_crs, coords = coords)
}


#' @name read_sf_gist
#' @rdname read_sf_ext
#' @param nth For [read_sf_gist()], the file to return from the gist, e.g. 1 for
#'   first, 2 for second. Defaults to 1.
#' @inheritParams gistr::gist
#' @export
read_sf_gist <- function(url,
                         id = NULL,
                         bbox = NULL,
                         nth = 1,
                         ...) {
  check_installed("gistr")

  if (!is_missing(url) && is_null(id)) {
    id <- url
  }

  gist_data <-
    gistr::gist(
      id = id
    )

  check_null(gist_data[["files"]])

  read_sf_url(
    url = gist_data[["files"]][[nth]][["raw_url"]],
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
                         layer = NULL,
                         combine_layers = FALSE,
                         zm_drop = TRUE,
                         .name_repair = "check_unique") {
  url <- make_gmap_url(url)

  layer <- layer %||% sf::st_layers(dsn = url)[["name"]]

  if (has_min_length(layer, 2)) {
    cli_progress_layers <-
      cli::cli_progress_along(
        layer,
        "Downloading map layers"
      )

    map_gmap_layers <-
      function(x) {
        dplyr::bind_cols(
          Layer = x,
          read_sf_gmap(
            url = url,
            bbox = bbox,
            layer = x,
            combine_layers = FALSE,
            zm_drop = zm_drop
          )
        )
      }

    if (combine_layers) {
      data <-
        purrr::map_dfr(
          cli_progress_layers,
          ~ map_gmap_layers(layer[.x])
        )
    } else {
      data <-
        set_names(
          map(
            cli_progress_layers,
            ~ map_gmap_layers(layer[.x])
          ),
          layer
        )
    }

    return(data)
  }

  data <-
    sf::read_sf(
      dsn = url,
      layer = layer
    )

  if (has_name(data, "Description")) {
    check_installed("naniar")
    data <- naniar::replace_with_na(data, replace = list("Description" = ""))
  }

  data <-
    set_names_repair(data, .name_repair = .name_repair)

  if (zm_drop) {
    data <- sf::st_zm(data)
  }

  st_filter_ext(data, bbox)
}

#' Get map ID from url
#' @noRd
get_gmap_id <- function(url) {
  str_extract(
    url,
    regex("(?<=mid=)([[:alnum:]]|_)+((?=&)|(?=/$)|$)", TRUE)
  )
}

#' Make a Google Maps KML format URL
#' @noRd
make_gmap_url <- function(url = NULL, mid = NULL, format = "kml") {
  cli_abort_ifnot(
    "{.arg url} must be a valid Google Maps url." = is_gmap_url(url)
  )

  if (!is_null(url)) {
    mid <- get_gmap_id(url)
  }

  if (format != "kml") {
    cli_abort("{.arg format} must be {.val kml}.")
  }

  glue("https://www.google.com/maps/d/u/0/kml?forcekml=1&mid={mid}")
}

#' @name read_sf_download
#' @rdname read_sf_ext
#' @param unzip If `TRUE`, url must be a zip file that is downloaded to a cache
#'   folder, unzipped into a temporary directory (created with [tempdir()]), and
#'   then read to a file using the specified file type.
#' @inheritParams filenamr::get_data_dir
#' @inheritParams filenamr::make_filename
#' @export
#' @importFrom sf st_crs
#' @importFrom utils download.file unzip
#' @importFrom filenamr make_filename get_data_dir
read_sf_download <-
  function(url,
           filename,
           bbox = NULL,
           path = NULL,
           filetype = "geojson",
           prefix = "date",
           method = "auto",
           unzip = FALSE,
           .name_repair = "check_unique",
           ...) {
    path <- filenamr::get_data_dir(path = path, cache = TRUE)

    destfile <-
      filenamr::make_filename(
        prefix = prefix,
        filename = filename,
        path = path,
        fileext = filetype
      )

    utils::download.file(
      url = url,
      destfile = destfile,
      method = method
    )

    if (is_true(unzip)) {
      zipdest <-
        filenamr::make_filename(
          prefix = prefix,
          filename = filename,
          path = tempdir(),
          fileext = filetype
        )

      utils::unzip(
        zipfile = destfile,
        exdir = tempdir(),
        overwrite = TRUE
      )

      destfile <- zipdest
    }

    read_sf_path(path = destfile, bbox = bbox, .name_repair = .name_repair, ...)
  }

#' @name read_sf_gsheet
#' @rdname read_sf_ext
#' @inheritParams googlesheets4::read_sheet
#' @param ask If `TRUE`, ask for the name of the Google Sheet to read if ss is
#'   not provided to [read_sf_gsheet].
#' @export
#' @importFrom rlang is_missing
#' @importFrom cliExtras cli_ask
read_sf_gsheet <- function(url,
                           sheet = NULL,
                           ss = NULL,
                           bbox = NULL,
                           ask = FALSE,
                           coords = c("lon", "lat"),
                           from_crs = 4326,
                           geo = FALSE,
                           address = "address",
                           .name_repair = "check_unique",
                           ...) {
  # Convert Google Sheet with coordinates to sf
  check_installed("googlesheets4")

  if (is_true(ask)) {
    ss <-
      googlesheets4::gs4_find(
        cliExtras::cli_ask("What is the name of the Google Sheet to return?")
      )
  }

  ss <- ss %||% url

  data <-
    googlesheets4::read_sheet(
      ss = ss,
      sheet = sheet,
      .name_repair = .name_repair,
      ...
    )

  if (is_null(coords) && is_false(geo)) {
    return(data)
  }

  data <-
    df_to_sf(
      data,
      coords = coords,
      geo = geo,
      address = address,
      from_crs = from_crs
    )

  st_filter_ext(data, bbox)
}

#' Join data from a Google Sheet to a simple feature object
#'
#' @noRd
#' @importFrom dplyr left_join
#' @importFrom sf st_drop_geometry
#' @importFrom cliExtras cli_yesno
join_sf_gsheet <- function(data,
                           ss = NULL,
                           sheet = 1,
                           key = NULL,
                           suffix = c("", "_gsheet")) {
  if (cli_yesno("Are you ready to sync from Google Sheets
               back to an sf object?")) {
    sheet_data <-
      sf::st_drop_geometry(
        read_sf_gsheet(
          ss = ss,
          sheet = sheet,
          ask = TRUE
        )
      )

    if (!is_null(key)) {
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

#' Set names from vctrs::vec_as_names
#'
#' @noRd
#' @importFrom vctrs vec_as_names
set_names_repair <- function(data = NULL,
                             nm = NULL,
                             .name_repair = "check_unique",
                             repair_arg = ".name_repair",
                             quiet = FALSE,
                             call = caller_env()) {
  check_character(nm, allow_null = TRUE, call = call)

  nm <- nm %||% names(data)

  if (is_null(nm)) {
    return(data)
  }

  set_names(
    data,
    nm = vctrs::vec_as_names(
      nm,
      repair = .name_repair,
      quiet = quiet,
      repair_arg = repair_arg
    )
  )
}

#' Make options parameter for read_sf_csv
#'
#' @noRd
make_sf_options <- function(options = NULL,
                            coords = NULL,
                            wkt = NULL,
                            rev = TRUE) {
  if (!is_null(wkt) && has_length(wkt, 1)) {
    options <- c(options, glue("GEOM_POSSIBLE_NAMES={wkt}"))
  } else if (!is_null(coords) && has_length(coords, 2)) {
    coords <- check_coords(coords = coords, rev = rev)
    options <-
      c(
        options,
        glue("X_POSSIBLE_NAMES={coords[[1]]}"),
        glue("Y_POSSIBLE_NAMES={coords[[2]]}")
      )
  }

  options
}

#' Make a wkt_filter from a bbox for read_sf_query
#'
#' @noRd
make_sf_wkt_filter <- function(wkt_filter = NULL,
                               bbox = NULL) {
  if (is_null(bbox)) {
    return(wkt_filter %||% character(0))
  }

  cli_warn_ifnot(
    "{.arg wkt_filter} is ignored if {.arg bbox} is provided." =
      is_null(wkt_filter)
  )

  # Convert bbox to well known text
  sf_bbox_to_wkt(bbox = bbox)
}

#' Make a query from a name and name_col value for read_sf_query
#'
#' @noRd
make_sf_query <- function(dsn = NULL,
                          table = NULL,
                          name = NULL,
                          name_col = NULL,
                          query = NULL) {
  if (any(c(is_null(name), is_null(name_col), is_geojson_fileext(dsn)))) {
    return(query %||% NA)
  }

  table <-
    table %||%
    str_extract(
      basename(dsn),
      regex("[[:graph:]]+(?=\\.)", TRUE)
    )

  table <-
    arg_match(
      table,
      as.character(sf::st_layers(dsn = dsn)[["name"]])
    )

  cli_warn_ifnot(
    "{.arg query} is ignored if {.arg name}
      and {.arg name_col} are provided." = is_null(query)
  )

  glue(
    "select * from {table} where {name_col} IN
    ({glue_collapse(paste0(\"'\", name, \"'\"), sep = ', ')})"
  )
}

#' Helper to make query for read_sf_esri
#'
#' @noRd
make_where_query <- function(where = NULL,
                             name = NULL,
                             name_col = NULL,
                             bbox = NULL,
                             coords = c("lon", "lat")) {
  if (!is_null(where)) {
    where <- paste0("(", where, ")")
  }

  if (!is_null(name) && !is_null(name_col)) {
    where <- c(where, glue("({name_col} = '{name}')"))
  }

  if (!is_null(bbox) && !is_null(coords)) {
    where <-
      c(where, sf_bbox_to_lonlat_query(bbox = bbox, coords = coords))
  }

  if (is_null(where)) {
    return(where)
  }

  paste(where[!is.na(where)], collapse = " AND ")
}
