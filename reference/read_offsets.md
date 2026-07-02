# Read region offsets from CSV

Read region offsets from CSV

## Usage

``` r
read_offsets(path)
```

## Arguments

- path:

  Path to a CSV with `region`, `dx_m`, and `dy_m` columns.

## Value

A data frame with normalized `region`, `dx_m`, and `dy_m` columns.

## Examples

``` r
path <- tempfile(fileext = ".csv")
writeLines(c("region,dx_m,dy_m", "North,1000,-500", "South,0,0"), path)
read_offsets(path)
#>   region dx_m dy_m
#> 1  North 1000 -500
#> 2  South    0    0
```
