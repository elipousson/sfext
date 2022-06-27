## code to prepare `DATASET` dataset goes here
select_unit_options <-
  c(
    "arc_degree", "arc_minute", "arc_second", "cm", "m", "metre", "meter", "meters", "km", "kilometer", "kilometers",
    "inch", "in", "ft", "foot", "feet", "yard", "yards", "mi", "mile", "miles", "nautical_mile", "radian",
    "mm", "millimeter", "millimeters", "millimetre", "millimetres"
  )

add_metric_units <-
  tibble::tribble(
    ~symbol, ~symbol_aliases, ~name_singular, ~name_singular_aliases, ~name_plural, ~name_plural_aliases, ~def, ~definition, ~comment, ~dimensionless, ~source_xml,
    "km", "", "kilometer", "kilometre", "kilometers", "kilometres", "m * 1000", "length equivalent to 1000 meters", NA, FALSE, NA,
    "cm", "", "centimeter", "centimetre", "centimeters", "centimetres", "m/100", "length equivalent to 0.01 meter", NA, FALSE, NA
  )

valid_units <-
  units::valid_udunits()

valid_units <-
  valid_units |>
  dplyr::bind_rows(add_metric_units)

units_filter_cols <- c("symbol", "symbol_aliases", "name_singular", "name_singular_aliases", "name_plural", "name_plural_aliases")

dist_units <-
  valid_units |>
  dplyr::filter(
    dplyr::if_any(
      dplyr::any_of(units_filter_cols),
      ~ (.x %in% select_unit_options)
    )
  )

dist_units <-
  naniar::replace_with_na_if(dist_units, is.character, ~ .x == "")

usethis::use_data(
  dist_units,
  overwrite = TRUE
)

dist_unit_options <-
  unique(c(
    dist_units$name_singular,
    dist_units$name_plural,
    stringr::str_split(dist_units$name_singular_aliases, ", ", simplify = TRUE),
    stringr::str_split(dist_units$name_plural_aliases, ", ", simplify = TRUE),
    dist_units$symbol,
    dist_units$symbol_aliases,
    select_unit_options
  ))

dist_unit_options <- dist_unit_options[!is.na(dist_unit_options)]
dist_unit_options <- dist_unit_options[dist_unit_options != ""]

usethis::use_data(
  dist_unit_options,
  overwrite = TRUE,
  internal = FALSE
)

area_units <-
  valid_units |>
  dplyr::filter(
    dplyr::if_any(
      dplyr::any_of(units_filter_cols),
      ~ (.x %in% c("hectare", "acre", "acre_foot"))
    )
  )

area_units <-
  naniar::replace_with_na_if(area_units, is.character, ~ .x == "")

area_unit_options <-
  c(paste0(c(
    "meter", "meters", "nautical_mile", "international_inch", "international_foot",
    "international_yard", "international_mile", "kilometer", "centimeter", "m", "in",
    "ft", "yd", "mi", "km", "cm", "metre", "inch", "foot", "yard", "mile", "kilometre",
    "centimetre", "international_feet", "kilometers", "centimeters", "feet", "kilometres",
    "centimetres"
  ), "^2"), "hectare", "hectares", "acre", "acres", "acre_foot", "acre_feet")

usethis::use_data(
  area_unit_options,
  overwrite = TRUE
)
