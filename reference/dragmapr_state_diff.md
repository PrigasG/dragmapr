# Compare dragmapr states

`dragmapr_state_diff()` reports composition and optional interaction
changes between two states. `dragmapr_state_equal()` is the
corresponding predicate.

## Usage

``` r
dragmapr_state_diff(
  draft,
  canonical,
  tolerance = 1,
  compare = c("composition", "interaction", "all")
)

dragmapr_state_equal(
  a,
  b,
  tolerance = 1,
  compare = c("composition", "interaction", "all")
)
```

## Arguments

- draft, canonical, a, b:

  `dragmapr_state` objects.

- tolerance:

  Numeric tolerance for offset changes.

- compare:

  One of `"composition"`, `"interaction"`, or `"all"`.

## Value

`dragmapr_state_diff()` returns a `dragmapr_state_diff` object.
`dragmapr_state_equal()` returns `TRUE` or `FALSE`.
