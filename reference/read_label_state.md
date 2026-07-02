# Read draggable label state from CSV

Read draggable label state from CSV

## Usage

``` r
read_label_state(path)
```

## Arguments

- path:

  Path to a CSV with `label_id`, `region`, `dx_m`, and `dy_m`.

## Value

A normalized label state data frame.

## Examples

``` r
path <- tempfile(fileext = ".csv")
writeLines(c("label_id,region,dx_m,dy_m", "North,North,500,200"), path)
read_label_state(path)
#>   label_id region dx_m dy_m
#> 1    North  North  500  200
```
