# Merge dragmapr state updates

Merge dragmapr state updates

## Usage

``` r
merge_dragmapr_state(state, update)
```

## Arguments

- state:

  Base `dragmapr_state`.

- update:

  Update `dragmapr_state`. Non-empty offset tables replace rows in
  `state` by key.

## Value

A merged `dragmapr_state`. Its `version` is one greater than the higher
of the two input revisions, so the merged state always sorts after both
of its inputs.
