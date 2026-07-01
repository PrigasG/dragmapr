# Add or replace spatial features

Add or replace spatial features

## Usage

``` r
add_spatial_features(x, features)

replace_spatial_features(x, ids, features, key_col = NULL)
```

## Arguments

- x:

  An `sf` object.

- features:

  An `sf` object containing new or replacement features.

- ids:

  Existing feature ids to replace. Matched like
  [`remove_spatial_features()`](https://prigasg.github.io/dragmapr/reference/remove_spatial_features.md).

- key_col:

  Optional stable id column.

## Value

An `sf` object.
