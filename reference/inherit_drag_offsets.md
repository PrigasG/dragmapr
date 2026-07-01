# Inherit drag offsets between hierarchy levels

Inherit drag offsets between hierarchy levels

## Usage

``` r
inherit_drag_offsets(state, from, to, relation)
```

## Arguments

- state:

  A `dragmapr_state` object.

- from, to:

  Column names in `relation` describing the source and target levels.

- relation:

  Data frame mapping parent keys to child keys.

## Value

A `dragmapr_state` at level `to`.
