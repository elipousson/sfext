# Get social media image size to match platform and format

See `paper_sizes[paper_sizes$type == "social",]$name` for support image
options.

## Usage

``` r
get_social_image(
  image = NULL,
  platform = NULL,
  format = NULL,
  orientation = NULL
)
```

## Arguments

- image:

  Image size name, Default: `NULL`

- platform:

  Social media platform, "Instagram", "Facebook", or "Twitter", Default:
  `NULL`

- format:

  Image format, "post", "story", or "cover", Default: `NULL`

- orientation:

  Image orientation, Default: `NULL`.
