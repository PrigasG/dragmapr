# dragmapr widget options

These constructors separate options that affect source geometry from
options that can be updated live in the browser.

## Usage

``` r
dragmapr_geometry_options(width = 7200, height = 4800)

dragmapr_display_options(
  region_palette = NULL,
  show_origin_outlines = FALSE,
  show_movement_connectors = FALSE,
  show_drag_trail = FALSE,
  map_background = c("white", "transparent", "light_grid", "dark"),
  connector_color = "#334155",
  connector_linewidth = 1.3
)

dragmapr_interaction_options(draggable_regions = TRUE, draggable_labels = TRUE)
```

## Arguments

- width, height:

  Widget canvas dimensions in CSS pixels.

- region_palette:

  Optional named color vector.

- show_origin_outlines, show_movement_connectors, show_drag_trail:

  Logical display toggles.

- map_background:

  One of `"white"`, `"transparent"`, `"light_grid"`, or `"dark"`.

- connector_color:

  Browser label-connector color.

- connector_linewidth:

  Browser label-connector width.

- draggable_regions, draggable_labels:

  Enable region and label dragging.

## Value

A named list of options.
