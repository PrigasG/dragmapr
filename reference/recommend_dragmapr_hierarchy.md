# Recommend a hierarchy for dragmapr

Picks a parent column and, when available, a child column suitable for
bloom/leaf-flip animation.

## Usage

``` r
recommend_dragmapr_hierarchy(x, parent_col = NULL, max_child_groups = 600L)
```

## Arguments

- x:

  A data frame or `sf` object.

- parent_col:

  Optional preferred parent column.

- max_child_groups:

  Maximum number of child groups to recommend.

## Value

A list with `parent`, `child`, `confidence`, `reason`, and `pairs`.
