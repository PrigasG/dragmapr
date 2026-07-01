# Inspect editable spatial features

Builds a plain data frame for Shiny tables, review screens, and scripted
edit workflows. Geometry is summarized instead of expanded so users can
decide which features to remove without accidentally dropping the active
geometry column.

## Usage

``` r
spatial_feature_table(x, key_col = NULL, include_geometry = FALSE)

view_spatial_features(x, key_col = NULL, include_geometry = FALSE)
```

## Arguments

- x:

  An `sf` object.

- key_col:

  Optional column containing stable feature ids. When omitted,
  `.feature_id` is the row number as a character string.

- include_geometry:

  Include the active geometry list-column. Defaults to `FALSE` for table
  display.

## Value

A data frame with `.feature_id`, `.row`, `.geometry_type`, bbox columns,
and the non-geometry attributes from `x`.
