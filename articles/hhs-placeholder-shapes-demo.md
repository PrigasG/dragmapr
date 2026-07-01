# HHS placeholder shapes demo

This short video shows the embedded HHS placeholder shapes workflow in
Spatial Studio. It uses the bundled HHS-style fixture as an actual
map-shaped starting point for dragging regions, adjusting labels, and
exporting reproducible artifacts.

Your browser does not support embedded video. [Download the HHS
placeholder shapes demo
video.](https://prigasg.github.io/dragmapr/hhs-placeholder-shapes-demo.mp4)

The same HHS-style fixture is available from R with
[`example_hhs_layout()`](https://prigasg.github.io/dragmapr/reference/example_hhs_layout.md):

``` r

library(dragmapr)

hhs <- example_hhs_layout()

drag_map_prototype(
  hhs$states,
  region_col = "hhs_region",
  labels = hhs$labels,
  region_offsets = hhs$region_offsets,
  label_offsets = hhs$label_offsets,
  region_palette = hhs$region_colors,
  show_legend = TRUE,
  legend_title = "HHS region",
  open = interactive()
)
```

For a full Spatial Studio session, launch the bundled app:

``` r

if (interactive()) {
  shiny::runApp(system.file(
    "examples",
    "shiny_spatial_studio.R",
    package = "dragmapr"
  ))
}
```
