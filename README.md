---
title: dragmapr Spatial Studio
emoji: 🗺️
colorFrom: blue
colorTo: green
sdk: docker
pinned: false
---

# dragmapr

<img src="man/figures/logo.png" align="right" height="180" alt="dragmapr hex logo" />

[![R-CMD-check](https://github.com/PrigasG/dragmapr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PrigasG/dragmapr/actions/workflows/R-CMD-check.yaml)

`dragmapr` creates draggable plots from projected `sf` geometry. It writes a
browser-based D3 helper where grouped shapes, labels, annotation boxes, and
connector lines can be moved directly. When users want a reproducible static
image afterwards, the helper exports small region and label offset CSVs that can
be rendered with `ggplot2`.

The package started from an `explodemap` paper workflow, but it is now
self-contained. The bundled HHS example copies only the small reusable pieces
from that work: region membership, names/colors, and published display offsets.

## Install

```r
# install.packages("pak")
pak::pak("PrigasG/dragmapr")
```

For local development:

```r
devtools::load_all()
```

## Core Workflow

1. Start with a projected `sf` object and a grouping column.
2. Write a draggable browser plot helper.
3. Drag regions and labels until the layout works.
4. Use the draggable plot directly, or download/copy region and label offset CSVs.
5. Optionally render a static plot or image from the source geometry plus those CSVs.

```r
library(dragmapr)

drag_map_prototype(
  my_sf,
  region_col = "region",
  label_col = "region",
  labels = TRUE,
  draggable_labels = TRUE,
  open = TRUE
)

render_dragged_map(
  my_sf,
  region_offsets = "drag_region_offsets.csv",
  region_col = "region",
  label_col = "region",
  label_offsets = "drag_label_offsets.csv",
  file = "dragged-map.png"
)
```

## Labels, Info Boxes, And Connectors

Labels are optional when creating the draggable plot. There are four label
concepts:

- `make_region_labels()` derives one default label per draggable group.
- `as_drag_labels()` accepts user-supplied labels and preserves extra columns
  such as tooltip or styling metadata.
- `as_drag_annotations()` turns label rows into draggable info boxes for longer
  notes or callouts.
- `read_label_state()` and `apply_label_state()` restore label movements from
  CSV state exported by the browser helper.

```r
drag_map_prototype(my_sf, "region", labels = FALSE)
drag_map_prototype(my_sf, "region", label_col = "name", label_marker = FALSE)
drag_map_prototype(my_sf, "region", label_col = "name", label_marker_shape = "circle")
drag_map_prototype(my_sf, "region", show_legend = TRUE, legend_position = "right")
drag_map_prototype(my_sf, "region", label_col = "name", label_text_size = 14)

region_labels <- make_region_labels(my_sf, region_col = "region", label_col = "name")
custom_labels <- as_drag_labels(data.frame(
  label_id = "note-1",
  region = "North",
  label = "Custom note",
  x = 50000,
  y = 150000
))

labels <- apply_label_state(region_labels, "drag_label_offsets.csv")
```

Label tables can also opt into connector lines:

```r
notes <- as_drag_annotations(data.frame(
  label_id = "north-note",
  region = "North",
  label = "Longer text about this location",
  x = 50000,
  y = 150000
), connector = TRUE, connector_type = "squiggle")

render_dragged_map(
  my_sf,
  region_offsets = "drag_region_offsets.csv",
  region_col = "region",
  labels = notes,
  label_offsets = "drag_label_offsets.csv",
  connector_linewidth = 0.8,
  show_label_marker = FALSE,
  label_marker_shape = "none",
  file = "annotated-layout.png"
)
```

Supported connector styles are `"straight"`, `"elbow"`, `"curve"`, and
`"squiggle"`. Advanced users can set `connector_start_x` /
`connector_start_y` or `connector_mid_x` / `connector_mid_y` columns for custom
line starts and breakpoints.

## Static Export From Offsets

The draggable helper exports two small tables:

- region offsets: `region`, `dx_m`, `dy_m`
- label offsets: `label_id`, `region`, `dx_m`, `dy_m`

Those CSVs are enough to reconstruct a static image later without re-running a
Shiny app. `render_dragged_map()` applies region movement first, then label
movement, then optional connectors and labels. It also expands plot limits
around displaced labels/connectors so exported PNGs do not clip callouts.

Use `read_offsets()` for downloaded region CSVs and `read_label_state()` for
downloaded label CSVs before passing them into `apply_offsets()`,
`apply_label_state()`, or `render_dragged_map()`. The older
`make_labels()`, `read_label_offsets()`, and `apply_label_offsets()` names are
kept as aliases for existing scripts.

For Shiny upload workflows, `read_dragmapr_sf_upload()`,
`read_dragmapr_sf_url()`, and `prepare_dragmapr_sf()` read and normalize user
geometry before calling `drag_map_prototype()`. `dragmapr_iframe_bridge()`
provides the JavaScript bridge used by Shiny apps to receive region and label
state from the helper iframe.

## Built-in Examples

`dragmapr` includes examples that avoid external downloads:

The exported `example_hhs_layout()` and `example_panel_layout()` helpers return
small self-contained layouts used by these examples and tests.

- `basic_draggable_map.R`: four synthetic map regions.
- `explodemap_hhs_labels.R`: bundled HHS-style layout with explodemap colors and offsets.
- `label_nudging.R`: independent text-marker nudges after region movement.
- `non_map_panels.R`: dashboard/diagram rectangles, useful for non-map geometry checks.
- `roundtrip_csv.R`: write offsets to CSV, read them back, and render.
- `shiny_custom_labels.R`: Shiny app with user-supplied draggable labels.
- `shiny_draggable_export.R`: Shiny app that captures drag state, previews a static plot, toggles labels/legends/connectors/info boxes, and exports PNG.
- `shiny_draggable_plot.R`: embed a draggable plot helper in a Shiny app.
- `shiny_spatial_studio.R`: first-pass spatial studio for local zipped shapefile, GeoJSON, or GPKG upload; choose grouping/labels/colors, show the legend in drag and preview panes, switch text labels between circle/rounded-box/text-only markers, drag the map, and export PNG/CSV/GeoJSON/HTML.
- `shiny_static_export.R`: Shiny static PNG export after offsets are available.
- `smoke_examples.R`: runs all bundled examples in a temporary directory.

Run the full smoke suite:

```r
source(system.file("examples", "smoke_examples.R", package = "dragmapr"))
```

## Vignettes And Site

The package includes four vignettes:

- Getting started with dragmapr
- Labels and static output
- Example gallery
- Shiny workflows

Build the pkgdown site locally with:

```r
pkgdown::build_site()
```

A GitHub Pages workflow is included at `.github/workflows/pkgdown.yaml`. Once
the repository has a GitHub remote and Pages is enabled for GitHub Actions, the
site will publish to <https://prigasg.github.io/dragmapr/>.

## Design Goals

- Rigid region movement: every feature in a group receives the same offset.
- Reproducible manual layout: dragging creates data, not hidden state.
- Stable metre offsets: browser resizing should not change exported offsets.
- Static output friendly: outputs are ordinary `ggplot2` plots and image files.
- Label-aware: labels can move with regions and be nudged independently.
- Annotation-aware: info boxes, text-only labels, connector lines, and static
  export controls are part of the same label table.
- Small CSV surface: hand edits stay reviewable in Git.
