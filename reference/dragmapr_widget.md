# Create a native dragmapr htmlwidget

Create a native dragmapr htmlwidget

## Usage

``` r
dragmapr_widget(
  x,
  region_col,
  label_col = region_col,
  labels = TRUE,
  state = NULL,
  region_palette = NULL,
  geometry_options = dragmapr_geometry_options(),
  display_options = dragmapr_display_options(),
  interaction_options = dragmapr_interaction_options(),
  width = "100%",
  height = "650px",
  elementId = NULL
)
```

## Arguments

- x:

  An `sf` object in a projected CRS.

- region_col:

  Column defining draggable groups.

- label_col:

  Column used for default region-label text.

- labels:

  Show draggable labels, omit labels, or pass a label table.

- state:

  Optional `dragmapr_state`.

- region_palette:

  Optional named colour vector passed to
  [`dragmapr_display_options()`](https://prigasg.github.io/dragmapr/reference/dragmapr_geometry_options.md).
  This is a convenience alias for users who want widget, proxy, and
  static-render palettes to share one argument name.

- geometry_options:

  Options from
  [`dragmapr_geometry_options()`](https://prigasg.github.io/dragmapr/reference/dragmapr_geometry_options.md).

- display_options:

  Options from
  [`dragmapr_display_options()`](https://prigasg.github.io/dragmapr/reference/dragmapr_geometry_options.md).

- interaction_options:

  Options from
  [`dragmapr_interaction_options()`](https://prigasg.github.io/dragmapr/reference/dragmapr_geometry_options.md).

- width, height:

  htmlwidget container size.

- elementId:

  Optional widget element id.

## Value

An htmlwidget.
