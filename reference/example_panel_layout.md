# Build a non-map panel layout example

This creates simple projected rectangles that behave like plot panels,
dashboard tiles, or diagram cards. It is useful for checking that
dragmapr's offset workflow is not tied to administrative map boundaries.

## Usage

``` r
example_panel_layout()
```

## Value

A list with `panels`, `labels`, `region_offsets`, `label_offsets`,
`region_names`, and `region_colors`.

## Examples

``` r
panels <- example_panel_layout()
names(panels)
#> [1] "panels"         "labels"         "region_offsets" "label_offsets" 
#> [5] "region_names"   "region_colors" 
```
