# Write a draggable map in your browser

Opens an interactive HTML page where you can drag map regions and labels
into any layout you like. When you are happy with the result, click the
copy or download buttons to save two small CSV files (one for regions,
one for labels). Pass those CSVs to
[`render_dragged_map()`](https://prigasg.github.io/dragmapr/reference/render_dragged_map.md)
to reproduce the layout as a static image.

## Usage

``` r
drag_map_prototype(
  x,
  region_col,
  label_col = region_col,
  labels = TRUE,
  draggable_labels = TRUE,
  label_marker = TRUE,
  label_marker_shape = c("rect", "circle", "none"),
  label_radius = 12,
  label_text_size = 11,
  label_width = 64,
  label_height = 30,
  label_box_width = 150,
  label_box_height = 72,
  connector_color = "#334155",
  connector_linewidth = 1.3,
  region_offsets = NULL,
  label_offsets = NULL,
  region_palette = NULL,
  show_legend = FALSE,
  max_legend_keys = 25L,
  legend_position = c("bottom", "top", "left", "right", "none"),
  legend_title = "Region",
  legend_values = NULL,
  label_values = NULL,
  legend_reflects_bloom = FALSE,
  map_background = c("white", "transparent", "light_grid", "dark"),
  connector_linetype = c("solid", "dashed", "dotted"),
  connector_endpoint = c("none", "arrow"),
  connector_smart = FALSE,
  show_origin_outlines = FALSE,
  show_movement_connectors = FALSE,
  show_movement_band = FALSE,
  movement_connector_color = "#64748b",
  movement_connector_opacity = 0.72,
  movement_connector_linewidth = 1.4,
  movement_connector_linetype = c("solid", "dashed", "dotted"),
  movement_connector_endpoint = c("closed", "open", "none"),
  show_drag_trail = FALSE,
  side_panel = TRUE,
  transition = NULL,
  file = NULL,
  open = FALSE
)
```

## Arguments

- x:

  An `sf` object. Run
  [`prepare_dragmapr_sf()`](https://prigasg.github.io/dragmapr/reference/prepare_dragmapr_sf.md)
  first if your data is in longitude/latitude.

- region_col:

  Column defining draggable groups.

- label_col:

  Column used for default region-label text. Defaults to `region_col`.

- labels:

  Show draggable labels. Use `TRUE` to derive one label per region,
  `FALSE` to omit labels, or a data frame accepted by
  [`as_drag_labels()`](https://prigasg.github.io/dragmapr/reference/as_drag_labels.md)
  for user-supplied labels.

- draggable_labels:

  Allow labels to be dragged independently of regions.

- label_marker:

  Draw a marker behind ordinary text labels. Set to `FALSE` for
  text-only draggable labels.

- label_marker_shape:

  Marker shape for ordinary draggable text labels: `"rect"` for rounded
  rectangles, `"circle"` for circles, or `"none"` for a transparent drag
  target with text only. If `label_marker = FALSE`, `label_marker_shape`
  is treated as `"none"`.

- label_radius:

  Retained for compatibility with earlier circle markers. Used when
  `label_marker_shape = "circle"`.

- label_text_size:

  Label text size in screen pixels.

- label_width, label_height:

  Default browser dimensions for ordinary draggable text-label markers.

- label_box_width, label_box_height:

  Default browser dimensions for draggable annotation boxes.

- connector_color:

  Browser label-connector color.

- connector_linewidth:

  Browser connector line width in pixels.

- region_offsets:

  Optional data frame with `region`, `dx_m`, and `dy_m` columns used to
  initialize region positions in the browser helper.

- label_offsets:

  Optional data frame with `label_id`, `region`, `dx_m`, and `dy_m`
  columns used to initialize label positions in the browser helper.

- region_palette:

  Optional named character vector mapping region values to hex color
  strings (e.g. `c(North = "#2166ac", South = "#d73027")`). When
  supplied the D3 helper uses these colors instead of its built-in
  palette, so the interactive colors match a
  [`render_dragged_map()`](https://prigasg.github.io/dragmapr/reference/render_dragged_map.md)
  call that uses the same palette.

- show_legend:

  Show a compact region legend inside the browser helper.

- max_legend_keys:

  Maximum number of legend keys to show in the browser helper before
  suppressing the legend. Set to `Inf` to always show it.

- legend_position:

  Browser-helper legend position. One of `"bottom"`, `"top"`, `"left"`,
  `"right"`, or `"none"`.

- legend_title:

  Title shown above the browser-helper legend.

- legend_values:

  Optional character vector of region values to include in the
  browser-helper legend. `NULL` includes all region values.

- label_values:

  Optional character vector of label IDs to display. `NULL` displays all
  labels while preserving all label offsets.

- legend_reflects_bloom:

  When `TRUE`, an armed bloom helper lets the browser legend switch
  between parent and child keys as branches expand. The default `FALSE`
  keeps a parent-only legend during bloom animations.

- map_background:

  Browser helper background. One of `"white"`, `"transparent"`,
  `"light_grid"`, or `"dark"`.

- connector_linetype:

  Browser helper connector line style. One of `"solid"`, `"dashed"`, or
  `"dotted"`.

- connector_endpoint:

  Browser helper connector endpoint. One of `"none"` or `"arrow"`.

- connector_smart:

  Choose connector geometry dynamically in the browser based on label
  displacement. When `TRUE`, the helper chooses among the available
  connector paths instead of using each row's `connector_type`.

- show_origin_outlines:

  Show the original, unshifted outlines of regions with non-zero offsets
  beneath the moved regions.

- show_movement_connectors:

  Draw a connector from each moved region's original representative
  point to its current location. Hidden for zero- offset regions.
  Defaults to `FALSE`.

- show_movement_band:

  Draw a swept shadow in the browser between each region's original
  polygon footprint and its translated position. The shadow traces the
  actual boundary of the shape rather than a flat bounding-box band.
  Defaults to `FALSE`.

- movement_connector_color, movement_connector_opacity,
  movement_connector_linewidth:

  Browser styling for movement connectors and the swept movement shadow.

- movement_connector_linetype:

  Movement connector line style. One of `"solid"`, `"dashed"`, or
  `"dotted"`.

- movement_connector_endpoint:

  Movement connector endpoint. One of `"none"`, `"open"`, or `"closed"`.

- show_drag_trail:

  Show a short fading trail of outline snapshots while a region is
  actively being dragged. The trail is cleared immediately when dragging
  ends and does not appear in static exports or project metadata.
  Defaults to `FALSE`.

- side_panel:

  Show the built-in copy/download side panel in the helper HTML.
  Defaults to `TRUE`; Shiny apps that provide their own controls can set
  this to `FALSE`.

- transition:

  Optional branch-bloom transition for parent/child grouping. A named
  list with elements: `groups` (named list mapping each parent label to
  the character vector of child region values it contains; used to draw
  a dotted group-drag boundary per expanded group), `child_region_col`
  (column in `x` holding each feature's child-level region key, enabling
  client-side expand/collapse), `shell_col` (column flagging dissolved
  parent-shell features with `1`; while a parent is collapsed only its
  shell is drawn and the child features stay hidden), `expanded` (parent
  values to start expanded), `mode`, `animation`, `duration_ms`,
  `easing`, `overshoot` (retained for backward compatibility),
  `show_parent_ghost`, and `boundary` (set `FALSE` to skip the dotted
  frame). The frame is now treated as a two-gesture group handle by
  default: dragging it moves all bloomed children in the branch
  together, while a plain click compresses the branch back to the
  parent. Optional transition fields include `boundary_behavior`
  (`"drag"` or `"none"`), `debug`, `stagger_ms`,
  `boundary_drag_threshold` in screen pixels before a pointer gesture is
  treated as a drag, and `boundary_label` for helper text. Use
  `"Drag to"` so the helper can label each frame as
  `"Drag to <parent/location>"`. `NULL` (default) disables the
  animation.

- file:

  Output HTML path. When `NULL`, a temporary `.html` file is created.
  Pass an explicit path to save the helper somewhere durable.

- open:

  Open the written file in the default browser via
  [`utils::browseURL()`](https://rdrr.io/r/utils/browseURL.html).
  Defaults to `FALSE`.

## Value

Invisibly returns `file`.

## See also

[`render_dragged_map()`](https://prigasg.github.io/dragmapr/reference/render_dragged_map.md)
for the optional static ggplot2 render after dragging;
[`make_region_labels()`](https://prigasg.github.io/dragmapr/reference/make_region_labels.md)
and
[`as_drag_annotations()`](https://prigasg.github.io/dragmapr/reference/as_drag_annotations.md)
to build custom label tables;
[`prepare_dragmapr_sf()`](https://prigasg.github.io/dragmapr/reference/prepare_dragmapr_sf.md)
to project uploaded geometry.

## Examples

``` r
poly <- sf::st_sf(
  region = c("A", "B"),
  geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0,1e5),c(1e5,1e5),c(1e5,2e5),c(0,2e5),c(0,1e5)))),
    sf::st_polygon(list(rbind(c(0,0),c(1e5,0),c(1e5,1e5),c(0,1e5),c(0,0)))),
    crs = 3857
  )
)

# Primary usage: open the interactive draggable plot in the browser.
# Drag regions and labels, then copy or download the offset CSVs.
if(interactive()){
drag_map_prototype(poly, region_col = "region", open = interactive())
}

# Write to a specific path without opening (e.g. for Shiny or CI).
drag_map_prototype(poly, region_col = "region",
                  file = tempfile(fileext = ".html"))
```
