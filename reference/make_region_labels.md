# Derive one default label per draggable region

Derive one default label per draggable region

## Usage

``` r
make_region_labels(
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

A drag label table with `label_id`, `region`, `label`, `x`, and `y`.

## See also

[`as_drag_labels()`](https://prigasg.github.io/dragmapr/reference/as_drag_labels.md)
for user-supplied labels;
[`as_drag_annotations()`](https://prigasg.github.io/dragmapr/reference/as_drag_annotations.md)
for draggable info boxes;
[`apply_label_state()`](https://prigasg.github.io/dragmapr/reference/apply_label_state.md)
to restore saved positions;
[`drag_map_prototype()`](https://prigasg.github.io/dragmapr/reference/drag_map_prototype.md)
which accepts the result as its `labels` argument.

## Examples

``` r
poly <- sf::st_sf(
  region = c("North", "South"),
  geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0,1e5),c(1e5,1e5),c(1e5,2e5),c(0,2e5),c(0,1e5)))),
    sf::st_polygon(list(rbind(c(0,0),c(1e5,0),c(1e5,1e5),c(0,1e5),c(0,0)))),
    crs = 3857
  )
)
make_region_labels(poly, region_col = "region")
#>   label_id region label     x      y label_type width_px height_px connector
#> 1    North  North North 50000 150000      label       NA        NA     FALSE
#> 2    South  South South 50000  50000      label       NA        NA     FALSE
#>   connector_type connector_mid_x connector_mid_y connector_start_x
#> 1       straight              NA              NA                NA
#> 2       straight              NA              NA                NA
#>   connector_start_y
#> 1                NA
#> 2                NA
```
