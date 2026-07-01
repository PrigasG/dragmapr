# Dragmap demo

This short video shows the core `dragmapr` loop on an actual map: open
the draggable helper, move regions and labels, and use the exported
state to create a reproducible static map.

Your browser does not support embedded video. [Download the dragmapr
demo video.](https://prigasg.github.io/dragmapr/dragmap-demo.mp4)

The same workflow can be run locally with the bundled examples:

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
  open = interactive()
)

render_dragged_map(
  hhs$states,
  region_offsets = hhs$region_offsets,
  region_col = "hhs_region",
  labels = hhs$labels,
  label_offsets = hhs$label_offsets,
  region_palette = hhs$region_colors,
  region_labels = hhs$region_names,
  title = "US Map by HHS Regions"
)
```
