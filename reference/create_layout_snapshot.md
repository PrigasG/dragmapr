# Save a layout snapshot for a grouped sf dataset

Captures the current drag offsets for a given grouping and returns a
named list. Snapshots can be passed to
[`inherit_layout()`](https://prigasg.github.io/dragmapr/reference/inherit_layout.md)
when switching to a finer grouping, or stored and later recovered with
[`restore_layout_snapshot()`](https://prigasg.github.io/dragmapr/reference/restore_layout_snapshot.md).

## Usage

``` r
create_layout_snapshot(x, group, offsets = NULL)
```

## Arguments

- x:

  A data frame or `sf` object.

- group:

  Column name (scalar character) defining the current grouping.

- offsets:

  A data frame with `region`, `dx_m`, and `dy_m` columns, a CSV path to
  such a file, or `NULL` for an all-zero starting state.

## Value

A named list with elements `group` (character), `offsets` (data frame or
`NULL`), and `timestamp` (POSIXct).

## See also

[`restore_layout_snapshot()`](https://prigasg.github.io/dragmapr/reference/restore_layout_snapshot.md),
[`inherit_layout()`](https://prigasg.github.io/dragmapr/reference/inherit_layout.md).

## Examples

``` r
poly <- sf::st_sf(
  county = c("A", "B"),
  geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0,1e5),c(1e5,1e5),c(1e5,2e5),c(0,2e5),c(0,1e5)))),
    sf::st_polygon(list(rbind(c(0,0),c(1e5,0),c(1e5,1e5),c(0,1e5),c(0,0)))),
    crs = 3857
  )
)
offsets <- data.frame(region = c("A", "B"), dx_m = c(10000, -10000), dy_m = 0)
snap <- create_layout_snapshot(poly, group = "county", offsets = offsets)
snap$group
#> [1] "county"
snap$offsets
#>   region   dx_m dy_m
#> 1      A  10000    0
#> 2      B -10000    0
```
