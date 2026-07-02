# Derive one label anchor per draggable region

`make_labels()` is deprecated. Use
[`make_region_labels()`](https://prigasg.github.io/dragmapr/reference/make_region_labels.md)
instead.

## Usage

``` r
make_labels(
  x,
  region_col,
  label_col = region_col,
  point = c("point_on_surface", "centroid")
)
```

## Arguments

- x:

  An `sf` object in a projected CRS.

- region_col:

  Column defining draggable groups.

- label_col:

  Column used for label text. Defaults to `region_col`.

- point:

  One of `"point_on_surface"` or `"centroid"`.

## Value

A drag label table.
