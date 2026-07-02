# Coerce data to a drag label table

Use this for user-supplied labels. Extra columns are preserved so
interactive applications can carry styling, tooltip, or grouping
metadata alongside the core label position columns.

## Usage

``` r
as_drag_labels(labels)
```

## Arguments

- labels:

  A data frame with `label_id`, `region`, `label`, `x`, and `y`.
  Optional columns include `label_type` (`"label"` or `"box"`),
  `width_px` and `height_px` for draggable info boxes, and `connector`,
  `connector_type`, `connector_start_x`, `connector_start_y`,
  `connector_mid_x`, and `connector_mid_y` for leader lines.

## Value

A normalized label data frame.

## Examples

``` r
as_drag_labels(data.frame(
  label_id = "note-1", region = "North", label = "Check this",
  x = 50000, y = 150000, tooltip = "extra metadata"
))
#>   label_id region      label     x      y        tooltip label_type width_px
#> 1   note-1  North Check this 50000 150000 extra metadata      label       NA
#>   height_px connector connector_type connector_mid_x connector_mid_y
#> 1        NA     FALSE       straight              NA              NA
#>   connector_start_x connector_start_y
#> 1                NA                NA
```
