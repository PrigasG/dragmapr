# Read label offsets from CSV

`read_label_offsets()` is deprecated. Use
[`read_label_state()`](https://prigasg.github.io/dragmapr/reference/read_label_state.md)
instead.

## Usage

``` r
read_label_offsets(path)
```

## Arguments

- path:

  Path to a CSV with `label_id`, `region`, `dx_m`, and `dy_m`.

## Value

A normalized label state data frame.
