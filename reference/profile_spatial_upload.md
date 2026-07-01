# Profile a spatial upload for dragmapr

Combines CRS information, geometry counts, candidate hierarchies, and a
recommended parent-child bloom setup.

## Usage

``` r
profile_spatial_upload(x, parent_col = NULL)
```

## Arguments

- x:

  An `sf` object.

- parent_col:

  Optional preferred parent column.

## Value

A list with `crs`, `geometry_type`, `n_features`, `hierarchy`, and
`animation`.
