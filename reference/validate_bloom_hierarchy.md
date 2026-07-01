# Validate a parent-child bloom hierarchy

Checks whether `child_col` can subdivide `parent_col` for leaf-flip
bloom.

## Usage

``` r
validate_bloom_hierarchy(x, parent_col, child_col, max_child_groups = 600L)
```

## Arguments

- x:

  A data frame or `sf` object.

- parent_col:

  Parent grouping column.

- child_col:

  Child grouping column.

- max_child_groups:

  Maximum number of child groups allowed.

## Value

A list with `valid`, `message`, `parent_key`, `child_key`, and summary
counts.
