# Summarise a spatial dataset for use with dragmapr

Inspects an `sf` object and reports geometry type, coordinate reference
system, feature count, candidate grouping columns (including empty-value
counts), detected parent-to-child column hierarchies (including cases
where child names repeat across parents), and invalid geometry counts.
The output guides column selection in
[`drag_map_prototype()`](https://prigasg.github.io/dragmapr/reference/drag_map_prototype.md)
and helps diagnose unexpected behaviour when uploading custom spatial
files to Spatial Studio.

## Usage

``` r
dragmapr_diagnostics(x, region_col = NULL, quiet = FALSE)
```

## Arguments

- x:

  An `sf` object.

- region_col:

  Optional column name. When supplied, hierarchy details are reported
  relative to that column and it is highlighted in the summary.

- quiet:

  Logical. When `TRUE`, the summary is not printed and the result list
  is returned invisibly without any output.

## Value

An invisible named list with components:

- `geometry_type`:

  Character vector of unique geometry type names.

- `crs`:

  CRS identifier string (e.g. `"EPSG:3857"` or `"Unknown CRS"`).

- `is_longlat`:

  Logical — `TRUE` when the CRS is geographic.

- `n_features`:

  Integer row count.

- `candidate_cols`:

  Character vector of candidate grouping columns.

- `column_details`:

  Named list with `n_unique` and `n_empty` per candidate column.

- `n_invalid_geometries`:

  Number of features with invalid geometry.

- `hierarchy_pairs`:

  List of detected parent-to-child column pairs, each with `parent`,
  `child`, `n_parent`, `n_child`, and `child_repeats_across_parents`.

- `recommended_path`:

  Suggested grouping path string, or `NULL`.

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
diag <- dragmapr_diagnostics(poly)
#> dragmapr spatial diagnostics
#> ------------------------------------------ 
#> Geometry type      : POLYGON
#> CRS                : EPSG:3857  (projected)
#> Features           : 4
#> Invalid geometries : 0
#> 
#> Candidate grouping columns:
#>   - county               (2 unique)
#>   - mun                  (3 unique)
#> 
#> Detected column hierarchies:
#>   - county -> mun  (2 -> 3 groups)  [child names repeat across parents - use make_hierarchy_key()]
#> 
#> Recommended grouping : county -> mun
#> ------------------------------------------ 
```
