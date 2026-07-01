# Detect parent-child grouping columns

Finds candidate hierarchy pairs in a data frame or `sf` object. A pair
is treated as parent-child when the child column has more groups than
the parent column and most child values sit inside one parent group.

## Usage

``` r
detect_hierarchy_columns(
  x,
  cols = NULL,
  max_child_groups = 600L,
  min_confidence = 0.72
)
```

## Arguments

- x:

  A data frame or `sf` object.

- cols:

  Optional character vector of columns to inspect. `NULL` uses
  non-geometry atomic columns.

- max_child_groups:

  Maximum number of child groups to recommend for interactive bloom.
  Pairs above this limit are returned but flagged.

- min_confidence:

  Minimum confidence for the `recommended` flag.

## Value

A data frame with one row per detected pair.

## Examples

``` r
df <- data.frame(
  county = c("A", "A", "B", "B"),
  town = c("A1", "A2", "B1", "B2")
)
detect_hierarchy_columns(df)
#>   parent child n_parent n_child composite_groups child_repeats_across_parents
#> 1 county  town        2       4                4                        FALSE
#>   nesting_score confidence recommended
#> 1             1          1        TRUE
#>                                        reason
#> 1 Child values nest inside the parent column.
```
