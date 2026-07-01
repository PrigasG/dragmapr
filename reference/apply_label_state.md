# Apply draggable label state to a label table

Apply draggable label state to a label table

## Usage

``` r
apply_label_state(labels, label_state = NULL)
```

## Arguments

- labels:

  A data frame from
  [`make_region_labels()`](https://prigasg.github.io/dragmapr/reference/make_region_labels.md)
  or
  [`as_drag_labels()`](https://prigasg.github.io/dragmapr/reference/as_drag_labels.md).

- label_state:

  A data frame with `label_id`, `region`, `dx_m`, and `dy_m`, a CSV
  path, or `NULL`.

## Value

A label data frame with adjusted `x` and `y`.

## Examples

``` r
labels <- as_drag_labels(data.frame(
  label_id = "North", region = "North", label = "North",
  x = 50000, y = 150000, stringsAsFactors = FALSE
))
state <- data.frame(
  label_id = "North", region = "North", dx_m = 5000, dy_m = -2000,
  stringsAsFactors = FALSE
)
apply_label_state(labels, state)
#>   label_id region label     x      y label_type width_px height_px connector
#> 1    North  North North 55000 148000      label       NA        NA     FALSE
#>   connector_type connector_mid_x connector_mid_y connector_start_x
#> 1       straight              NA              NA                NA
#>   connector_start_y
#> 1                NA
```
