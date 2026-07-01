# Remove or keep spatial features

Remove or keep spatial features

## Usage

``` r
remove_spatial_features(
  x,
  ids,
  key_col = NULL,
  invert = FALSE,
  prune_empty = TRUE
)

delete_spatial_features(
  x,
  ids,
  key_col = NULL,
  invert = FALSE,
  prune_empty = TRUE
)

keep_spatial_features(x, ids, key_col = NULL, prune_empty = TRUE)
```

## Arguments

- x:

  An `sf` object.

- ids:

  Feature ids to remove or keep. Matched against `key_col` when
  supplied, otherwise against row numbers. Character row ids such as
  `"3"` are accepted when `key_col = NULL`.

- key_col:

  Optional stable id column.

- invert:

  When `FALSE`, remove matching features. When `TRUE`, keep only
  matching features.

- prune_empty:

  Drop empty geometries after filtering.

## Value

An `sf` object with the same CRS and geometry column as `x`.
