# Collapse drag offsets between hierarchy levels

Collapse drag offsets between hierarchy levels

## Usage

``` r
collapse_drag_offsets(state, from, to, relation, method = "centroid")
```

## Arguments

- state:

  A `dragmapr_state` object.

- from, to:

  Column names in `relation` describing the source and target levels.

- relation:

  Data frame mapping child keys to parent keys.

- method:

  Collapse method. Currently `"centroid"` averages child offsets for
  each parent.

## Value

A `dragmapr_state` at level `to`.
