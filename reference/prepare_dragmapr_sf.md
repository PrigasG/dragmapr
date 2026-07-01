# Prepare an sf object for use with dragmapr

Repairs invalid geometry, filters to polygon types, assigns a fallback
CRS when none is present, and reprojects geographic (longitude/latitude)
data to a projected CRS so metre offsets work correctly. CRS-less inputs
trigger a warning because the fallback is an assumption; pass
`target_crs` explicitly when your data uses a different projected CRS.

## Usage

``` r
prepare_dragmapr_sf(x, target_crs = 3857)
```

## Arguments

- x:

  An `sf` object.

- target_crs:

  EPSG code for the projected CRS to use when the input is geographic.
  Defaults to `3857` (Web Mercator).

## Value

A projected `sf` object containing only polygon or multipolygon features
with valid geometry.

## See also

[`read_dragmapr_sf_upload()`](https://prigasg.github.io/dragmapr/reference/read_dragmapr_sf_upload.md)
and
[`read_dragmapr_sf_url()`](https://prigasg.github.io/dragmapr/reference/read_dragmapr_sf_url.md)
to read spatial files before passing them to this function;
[`drag_map_prototype()`](https://prigasg.github.io/dragmapr/reference/drag_map_prototype.md)
which requires a projected sf object.

## Examples

``` r
poly <- sf::st_sf(
  region = "A",
  geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(-1,0),c(1,0),c(1,1),c(-1,1),c(-1,0)))),
    crs = 4326
  )
)
out <- prepare_dragmapr_sf(poly)
#> dragmapr: transforming from geographic to EPSG:3857 for metre offsets.
sf::st_is_longlat(out)  # FALSE
#> [1] FALSE
```
