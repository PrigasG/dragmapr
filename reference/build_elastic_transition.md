# Build a local elastic parent-to-child transition

Prepares everything needed to animate child regions blooming out of
their parent region: per-child anchor and final positions, relative
motion strengths, ready-to-plot animation frames, and the dotted
group-drag boundary for each expanded group. The parent layout itself is
never moved (stable skeleton); only the expanded branch animates.

## Usage

``` r
build_elastic_transition(
  child_sf,
  parent_sf,
  parent_col,
  parent_id_col = parent_col,
  options = transition_options()
)
```

## Arguments

- child_sf:

  An `sf` object with the child regions in their final (expanded)
  positions.

- parent_sf:

  An `sf` object with the parent regions.

- parent_col:

  Column in `child_sf` holding each child's parent id.

- parent_id_col:

  Column in `parent_sf` holding the parent id. Defaults to `parent_col`,
  which covers the common case where both layers share the same column
  name (e.g. `"COUNTY"`).

- options:

  A list created by
  [`transition_options()`](https://prigasg.github.io/dragmapr/reference/transition_options.md).

## Value

A list of class `"dragmapr_transition"` with elements:

- `anchored`:

  `child_sf` plus `anchor_x`, `anchor_y`, `final_x`, `final_y`,
  `move_dist`, `move_ratio`, `stretch_strength`, and `duration_ms`
  columns.

- `frames`:

  An `sf` object of `n_frames` stacked copies of the children with
  `frame_id`, `frame_progress`, and `frame_eased` columns and
  interpolated geometry, suitable for `ggplot2` playback or export.

- `boundaries`:

  The dotted group-drag boundary per parent group from
  [`make_group_boundaries()`](https://prigasg.github.io/dragmapr/reference/make_group_boundaries.md),
  or `NULL` when `options$group_drag_frame` is `FALSE`.

- `options`:

  The validated options used.

## Details

Children whose parent id has no match in `parent_sf` fall back to their
own centroid as the anchor (they fade in place instead of blooming), and
a warning lists the unmatched ids so data problems are visible early.

The returned `frames` interpolate each child from a small seed at the
parent centroid (frame 1) to its exact final geometry (last frame) using
an ease-out-back curve, so the last frame equals `child_sf` and can be
used as the settled state.

## See also

[`transition_options()`](https://prigasg.github.io/dragmapr/reference/transition_options.md),
[`make_group_boundaries()`](https://prigasg.github.io/dragmapr/reference/make_group_boundaries.md),
[`layout_metrics()`](https://prigasg.github.io/dragmapr/reference/layout_metrics.md),
[`inherit_layout()`](https://prigasg.github.io/dragmapr/reference/inherit_layout.md)
for carrying drag offsets from parent to child groupings.

## Examples

``` r
rect <- function(x0, x1, y0, y1) {
  sf::st_polygon(list(cbind(c(x0, x1, x1, x0, x0), c(y0, y0, y1, y1, y0))))
}
parents <- sf::st_sf(
  county = c("A", "B"),
  geometry = sf::st_sfc(rect(0, 3, 0, 3), rect(4, 7, 0, 3), crs = 3857)
)
children <- sf::st_sf(
  county = c("A", "A", "B", "B"),
  mun    = c("A1", "A2", "B1", "B2"),
  geometry = sf::st_sfc(
    rect(-1, 1, 0, 3), rect(2, 4, 0, 3),
    rect(3, 5, 0, 3), rect(6, 8, 0, 3),
    crs = 3857
  )
)
tr <- build_elastic_transition(
  children, parents,
  parent_col = "county",
  options    = transition_options(n_frames = 5)
)
names(tr)
#> [1] "anchored"   "frames"     "boundaries" "options"   
head(tr$anchored[, c("county", "mun", "anchor_x", "final_x")])
#> Simple feature collection with 4 features and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -1 ymin: 0 xmax: 8 ymax: 3
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   county mun anchor_x final_x                       geometry
#> 1      A  A1      1.5       0 POLYGON ((-1 0, 1 0, 1 3, -...
#> 2      A  A2      1.5       3 POLYGON ((2 0, 4 0, 4 3, 2 ...
#> 3      B  B1      5.5       4 POLYGON ((3 0, 5 0, 5 3, 3 ...
#> 4      B  B2      5.5       7 POLYGON ((6 0, 8 0, 8 3, 6 ...
tr$boundaries
#> Simple feature collection with 2 features and 5 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -1.237171 ymin: -0.2371708 xmax: 8.237171 ymax: 3.237171
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   group_id      xmin     xmax       ymin     ymax
#> 1        A -1.237171 4.237171 -0.2371708 3.237171
#> 2        B  2.762829 8.237171 -0.2371708 3.237171
#>                         geometry
#> 1 POLYGON ((-1.237171 -0.2371...
#> 2 POLYGON ((2.762829 -0.23717...
```
