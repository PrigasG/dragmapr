# Restore a layout snapshot

Extracts the offset data frame from a snapshot created by
[`create_layout_snapshot()`](https://prigasg.github.io/dragmapr/reference/create_layout_snapshot.md).

## Usage

``` r
restore_layout_snapshot(snapshot)
```

## Arguments

- snapshot:

  A named list as returned by
  [`create_layout_snapshot()`](https://prigasg.github.io/dragmapr/reference/create_layout_snapshot.md).

## Value

A data frame with `region`, `dx_m`, and `dy_m` columns, or `NULL` if the
snapshot was created with `offsets = NULL`.

## See also

[`create_layout_snapshot()`](https://prigasg.github.io/dragmapr/reference/create_layout_snapshot.md).

## Examples

``` r
offsets <- data.frame(region = c("A", "B"), dx_m = c(10000, -10000), dy_m = 0)
snap <- create_layout_snapshot(
  data.frame(county = c("A", "B")),
  group   = "county",
  offsets = offsets
)
restore_layout_snapshot(snap)
#>   region   dx_m dy_m
#> 1      A  10000    0
#> 2      B -10000    0
```
