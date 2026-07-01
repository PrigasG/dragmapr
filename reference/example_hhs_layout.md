# Build a small explodemap-style HHS example layout

This copies the HHS region membership, names, colors, and published
display offsets used by the explodemap paper workflow into a lightweight
fixture that does not depend on the explodemap package or its generated
paper outputs. See the explodemap vignette "Reproducing the paper
examples", section 6: <https://CRAN.R-project.org/package=explodemap>.

## Usage

``` r
example_hhs_layout()
```

## Value

A list with `states`, `labels`, `region_offsets`, `label_offsets`,
`region_names`, and `region_colors`.

## Examples

``` r
hhs <- example_hhs_layout()
names(hhs)
#> [1] "states"         "labels"         "region_offsets" "label_offsets" 
#> [5] "region_names"   "region_colors" 
```
