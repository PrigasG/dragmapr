# Read and write dragmapr state JSON

Read and write dragmapr state JSON

## Usage

``` r
write_dragmapr_state(state, path)

read_dragmapr_state(path)
```

## Arguments

- state:

  A `dragmapr_state` object.

- path:

  File path.

## Value

`write_dragmapr_state()` invisibly returns `path`;
`read_dragmapr_state()` returns a `dragmapr_state`.
