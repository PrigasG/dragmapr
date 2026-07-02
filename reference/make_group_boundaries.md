# Build dotted group-drag boundaries for expanded groups

Computes one padded rectangular boundary per group. The boundary is
returned as real geometry with the group's id so it can be drawn inside
the same container as the children and therefore moves with the group
when dragged. In Spatial Studio this boundary is a drag handle, not a
collapse control.

## Usage

``` r
make_group_boundaries(sf_obj, group_col, padding = NULL)
```

## Arguments

- sf_obj:

  An `sf` object containing the (expanded) child regions.

- group_col:

  Column in `sf_obj` identifying the expanded group, usually the parent
  id column.

- padding:

  Single positive number in map units, or `NULL` (default) to use 2.5
  percent of the bounding-box diagonal of `sf_obj`.

## Value

An `sf` object with one row per group and columns `group_id`, `xmin`,
`xmax`, `ymin`, `ymax` (padded bounds), plus the rectangle geometry.

## See also

[`build_elastic_transition()`](https://prigasg.github.io/dragmapr/reference/build_elastic_transition.md),
which calls this automatically when `group_drag_frame = TRUE`.

## Examples

``` r
rect <- function(x0, x1, y0, y1) {
  sf::st_polygon(list(cbind(c(x0, x1, x1, x0, x0), c(y0, y0, y1, y1, y0))))
}
children <- sf::st_sf(
  county = c("A", "A", "B"),
  geometry = sf::st_sfc(
    rect(0, 1, 0, 1), rect(2, 3, 0, 1), rect(5, 6, 0, 1),
    crs = 3857
  )
)
make_group_boundaries(children, group_col = "county")
#> Simple feature collection with 2 features and 5 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -0.1520691 ymin: -0.1520691 xmax: 6.152069 ymax: 1.152069
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   group_id       xmin     xmax       ymin     ymax
#> 1        A -0.1520691 3.152069 -0.1520691 1.152069
#> 2        B  4.8479309 6.152069 -0.1520691 1.152069
#>                         geometry
#> 1 POLYGON ((-0.1520691 -0.152...
#> 2 POLYGON ((4.847931 -0.15206...
```
