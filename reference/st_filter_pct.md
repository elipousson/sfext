# Filter by share of length or area of one geometry overlapping with a second geometry

**\[experimental\]**

## Usage

``` r
st_filter_pct(x, y, pct = NULL, ...)

st_filter_pct_area(x, y, pct = NULL)

st_filter_pct_length(x, y, pct = NULL)
```

## Arguments

- x:

  A sf object to filter.

- y:

  A sf or sfc object to filter by.

- pct:

  Percent of length or area to use as a threshold value for filter.
  Numeric value of 1 or less.

- ...:

  Additional parameters. Not used currently.

## Value

A filtered version of the input sf object.
