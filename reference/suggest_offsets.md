# Suggest automatic region offsets for a draggable map

Computes a starting layout for
[`drag_map_prototype()`](https://prigasg.github.io/dragmapr/reference/drag_map_prototype.md)
or
[`render_dragged_map()`](https://prigasg.github.io/dragmapr/reference/render_dragged_map.md)
without manual dragging. The result is a data frame of offsets that can
be refined interactively in the browser.

## Usage

``` r
suggest_offsets(
  x,
  region_col,
  method = c("radial", "horizontal", "vertical", "grid", "none"),
  scale = 1
)
```

## Arguments

- x:

  An `sf` object in a projected CRS.

- region_col:

  Column in `x` defining draggable groups.

- method:

  Layout algorithm. One of `"radial"` (default), `"horizontal"`,
  `"vertical"`, `"grid"`, or `"none"`.

- scale:

  Positive numeric multiplier applied to all offsets. Defaults to `1`.
  Values greater than `1` push regions further apart.

## Value

A data frame with `region`, `dx_m`, and `dy_m` columns, one row per
unique region value. Suitable for use as `region_offsets` in
[`drag_map_prototype()`](https://prigasg.github.io/dragmapr/reference/drag_map_prototype.md)
or
[`render_dragged_map()`](https://prigasg.github.io/dragmapr/reference/render_dragged_map.md).

## Details

Available layout methods:

- `"radial"`:

  Each region is pushed outward from the collective centroid in the
  direction of its own centroid. Regions that are already spread out are
  pushed further than regions near the centre.

- `"horizontal"`:

  Regions are ranked by centroid x-coordinate and spread evenly along
  the x-axis. Useful for left-to-right layouts.

- `"vertical"`:

  Regions are ranked by centroid y-coordinate and spread evenly along
  the y-axis.

- `"grid"`:

  Regions are arranged in a rectangular grid ordered by centroid
  position (top-to-bottom, left-to-right).

- `"none"`:

  Returns zero offsets for every region. Useful as a placeholder when
  you want to start from an undisplaced state.

The `scale` argument multiplies all computed offsets uniformly. Increase
it to spread regions further apart; decrease it for a tighter layout.

## See also

[`drag_map_prototype()`](https://prigasg.github.io/dragmapr/reference/drag_map_prototype.md)
to open the interactive helper with the suggested layout as a starting
point;
[`apply_offsets()`](https://prigasg.github.io/dragmapr/reference/apply_offsets.md)
to apply offsets directly to an `sf` object.

## Examples

``` r
poly <- sf::st_sf(
  region = c("A", "B", "C", "D"),
  geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0,1e5),c(1e5,1e5),c(1e5,2e5),c(0,2e5),c(0,1e5)))),
    sf::st_polygon(list(rbind(c(0,0),c(1e5,0),c(1e5,1e5),c(0,1e5),c(0,0)))),
    sf::st_polygon(list(rbind(c(1e5,1e5),c(2e5,1e5),c(2e5,2e5),c(1e5,2e5),c(1e5,1e5)))),
    sf::st_polygon(list(rbind(c(1e5,0),c(2e5,0),c(2e5,1e5),c(1e5,1e5),c(1e5,0)))),
    crs = 3857
  )
)
suggest_offsets(poly, region_col = "region", method = "radial")
#>   region      dx_m      dy_m
#> 1      A -34641.02  34641.02
#> 2      B -34641.02 -34641.02
#> 3      C  34641.02  34641.02
#> 4      D  34641.02 -34641.02
suggest_offsets(poly, region_col = "region", method = "horizontal")
#>   region      dx_m dy_m
#> 1      A -73484.69    0
#> 2      B -24494.90    0
#> 3      C  24494.90    0
#> 4      D  73484.69    0
suggest_offsets(poly, region_col = "region", method = "grid")
#>   region      dx_m      dy_m
#> 1      A -48989.79  48989.79
#> 2      B -48989.79 -48989.79
#> 3      C  48989.79  48989.79
#> 4      D  48989.79 -48989.79
```
