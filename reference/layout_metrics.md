# Measure mental-map stability between two layouts

Compares region centroids before and after a transition and reports how
far regions drifted. Use it to confirm that expanding one branch left
the rest of the map stable: pass the non-expanded (sibling) regions as
`before` and `after`. Lower drift means the user's mental map survived.

## Usage

``` r
layout_metrics(before, after, id_col)
```

## Arguments

- before:

  An `sf` object with the layout before the transition.

- after:

  An `sf` object with the layout after the transition.

- id_col:

  Column present in both objects identifying each region.

## Value

A named list with `mean_drift` and `max_drift` (map units), `stability`
(0-100; 100 means no drift, 0 means mean drift of one third of the
`before` bounding-box diagonal or more), and `n_matched` (regions
compared). Regions missing from `after` are dropped with a warning.

## See also

[`build_elastic_transition()`](https://prigasg.github.io/dragmapr/reference/build_elastic_transition.md).

## Examples

``` r
rect <- function(x0, x1, y0, y1) {
  sf::st_polygon(list(cbind(c(x0, x1, x1, x0, x0), c(y0, y0, y1, y1, y0))))
}
before <- sf::st_sf(
  region = c("A", "B"),
  geometry = sf::st_sfc(rect(0, 1, 0, 1), rect(2, 3, 0, 1), crs = 3857)
)
after <- before
sf::st_geometry(after)[2] <- sf::st_geometry(after)[[2]] + c(0.5, 0)
layout_metrics(before, after, id_col = "region")
#> $mean_drift
#> [1] 0.25
#> 
#> $max_drift
#> [1] 0.5
#> 
#> $stability
#> [1] 76
#> 
#> $n_matched
#> [1] 2
#> 
```
