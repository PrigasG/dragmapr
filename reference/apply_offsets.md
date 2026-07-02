# Apply rigid offsets to grouped sf geometries

Apply rigid offsets to grouped sf geometries

## Usage

``` r
apply_offsets(x, offsets, region_col)
```

## Arguments

- x:

  An `sf` object in a projected CRS.

- offsets:

  A data frame with `region`, `dx_m`, and `dy_m`, or a CSV path.

- region_col:

  Column in `x` defining draggable groups.

## Value

An `sf` object with geometries translated by group.

## Examples

``` r
regions <- sf::st_sf(
  region = c("A", "B"),
  geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(100, 100)),
                        crs = 3857)
)
offsets <- data.frame(region = "A", dx_m = 5000, dy_m = -2000)
apply_offsets(regions, offsets, region_col = "region")
#> Simple feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 100 ymin: -2000 xmax: 5000 ymax: 100
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   region           geometry
#> 1      A POINT (5000 -2000)
#> 2      B    POINT (100 100)
```
