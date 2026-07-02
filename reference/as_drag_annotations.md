# Coerce data to draggable annotation boxes

Annotation boxes use the same position and state workflow as labels, but
render as larger text boxes for notes, callouts, or location
descriptions.

## Usage

``` r
as_drag_annotations(
  labels,
  width_px = 150,
  height_px = 72,
  connector = FALSE,
  connector_type = c("straight", "elbow", "curve", "squiggle")
)
```

## Arguments

- labels:

  A data frame with `label_id`, `region`, `label`, `x`, and `y`.

- width_px, height_px:

  Default browser box dimensions for rows that do not already include
  `width_px` or `height_px`.

- connector:

  Add connector lines from anchors to annotation boxes.

- connector_type:

  One of `"straight"`, `"elbow"`, `"curve"`, or `"squiggle"`.

## Value

A normalized label data frame with `label_type = "box"`.

## Examples

``` r
as_drag_annotations(data.frame(
  label_id = "note-1", region = "North",
  label = "This note can hold more text than a short label.",
  x = 50000, y = 150000
))
#>   label_id region                                            label     x      y
#> 1   note-1  North This note can hold more text than a short label. 50000 150000
#>   label_type width_px height_px connector connector_type connector_mid_x
#> 1        box      150        72     FALSE       straight              NA
#>   connector_mid_y connector_start_x connector_start_y
#> 1              NA                NA                NA
```
