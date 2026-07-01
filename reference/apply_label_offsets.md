# Apply label-specific offsets to a label table

`apply_label_offsets()` is deprecated. Use
[`apply_label_state()`](https://prigasg.github.io/dragmapr/reference/apply_label_state.md)
instead.

## Usage

``` r
apply_label_offsets(labels, label_offsets = NULL)
```

## Arguments

- labels:

  A data frame from
  [`make_region_labels()`](https://prigasg.github.io/dragmapr/reference/make_region_labels.md)
  or
  [`as_drag_labels()`](https://prigasg.github.io/dragmapr/reference/as_drag_labels.md).

- label_offsets:

  A data frame with `label_id`, `region`, `dx_m`, and `dy_m`, a CSV
  path, or `NULL`.

## Value

A label data frame with adjusted `x` and `y`.
