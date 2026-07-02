# Inherit region offsets from a coarser to a finer grouping column

When switching from a parent grouping (e.g., county) to a child grouping
(e.g., municipality), `inherit_layout()` maps the parent's drag offsets
to every child region that belongs to each parent. Each child inherits
the mean offset of the parent group it belongs to.

## Usage

``` r
inherit_layout(x, from, to, parent_offsets)
```

## Arguments

- x:

  An `sf` object or data frame containing both `from` and `to` columns.

- from:

  A character vector of one or more column names defining the coarser
  (parent) grouping. The `region` values in `parent_offsets` must match
  the keys produced by `make_hierarchy_key(x, from)`.

- to:

  A character vector of one or more column names defining the finer
  (child) grouping. The `region` column of the returned data frame will
  contain the keys produced by `make_hierarchy_key(x, to)`.

- parent_offsets:

  A data frame with `region`, `dx_m`, and `dy_m` columns keyed to the
  `from` grouping, as returned by
  [`read_offsets()`](https://prigasg.github.io/dragmapr/reference/read_offsets.md)
  or the Spatial Studio CSV export. Rows whose `region` does not match
  any parent key in `x` are silently ignored.

## Value

A data frame with `region`, `dx_m`, and `dy_m` columns. Rows whose
parent has no offset entry receive zero movement.

## Details

When child names repeat across parents (e.g., "Fairfield" exists in both
Essex and Morris counties), pass both columns in `to` so that composite
keys are used: `to = c("county", "mun")`. The resulting `region` values
will be composite strings like `"county=Essex | mun=Fairfield"`, which
can be passed directly to
[`drag_map_prototype()`](https://prigasg.github.io/dragmapr/reference/drag_map_prototype.md)
or
[`render_dragged_map()`](https://prigasg.github.io/dragmapr/reference/render_dragged_map.md)
via
[`make_hierarchy_key()`](https://prigasg.github.io/dragmapr/reference/make_hierarchy_key.md).

## See also

[`make_hierarchy_key()`](https://prigasg.github.io/dragmapr/reference/make_hierarchy_key.md)
for composite key construction,
[`create_layout_snapshot()`](https://prigasg.github.io/dragmapr/reference/create_layout_snapshot.md)
for capturing and restoring layouts.

## Examples

``` r
poly <- sf::st_sf(
  county = c("Essex", "Essex", "Morris", "Morris"),
  mun    = c("Fairfield", "Caldwell", "Fairfield", "Roxbury"),
  geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0,1e5),c(1e5,1e5),c(1e5,2e5),c(0,2e5),c(0,1e5)))),
    sf::st_polygon(list(rbind(c(0,0),c(1e5,0),c(1e5,1e5),c(0,1e5),c(0,0)))),
    sf::st_polygon(list(rbind(c(1e5,1e5),c(2e5,1e5),c(2e5,2e5),c(1e5,2e5),c(1e5,1e5)))),
    sf::st_polygon(list(rbind(c(1e5,0),c(2e5,0),c(2e5,1e5),c(1e5,1e5),c(1e5,0)))),
    crs = 3857
  )
)
county_offsets <- data.frame(
  region = c("Essex", "Morris"),
  dx_m   = c(50000, -50000),
  dy_m   = c(0, 0)
)
# "Fairfield" repeats, so use composite to= key
mun_offsets <- inherit_layout(
  poly,
  from           = "county",
  to             = c("county", "mun"),
  parent_offsets = county_offsets
)
mun_offsets
#>                          region   dx_m dy_m
#> 1   county=Essex | mun=Caldwell  50000    0
#> 2  county=Essex | mun=Fairfield  50000    0
#> 3 county=Morris | mun=Fairfield -50000    0
#> 4   county=Morris | mun=Roxbury -50000    0
```
