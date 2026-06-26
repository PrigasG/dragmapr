---
title: dragmapr Spatial Studio
emoji: 🗺️
colorFrom: blue
colorTo: green
sdk: docker
pinned: false
---

# dragmapr

<img src="man/figures/logo.png" alt="dragmapr hex logo" align="right" height="180"/>

[![R-CMD-check](https://github.com/PrigasG/dragmapr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PrigasG/dragmapr/actions/workflows/R-CMD-check.yaml)

`dragmapr` lets you drag map regions into any layout you want, then save that layout as a reproducible image. Open a draggable map in your browser, move regions and labels around until things look right, download two small CSV files, and turn them into a publication-ready `ggplot2` map.

Watch a short walkthrough in the [dragmap demo vignette](https://prigasg.github.io/dragmapr/articles/dragmap-demo.html), or see the [HHS placeholder shapes demo](https://prigasg.github.io/dragmapr/articles/hhs-placeholder-shapes-demo.html) for a complete Spatial Studio workflow.

## Links

- [HHS placeholder shapes demo](https://prigasg.github.io/dragmapr/articles/hhs-placeholder-shapes-demo.html)
- [CRAN package page](https://CRAN.R-project.org/package=dragmapr)
- [GitHub repository](https://github.com/PrigasG/dragmapr)

## Install

``` r
install.packages("dragmapr")

# Development version
# install.packages("pak")
# pak::pak("PrigasG/dragmapr")
```

## Quick Start

``` r
library(dragmapr)

# Step 1: open the interactive map in your browser and drag things around
drag_map_prototype(my_sf, region_col = "region", open = TRUE)

# Step 2: after downloading the offset CSVs, render a static image
render_dragged_map(
  my_sf,
  region_col    = "region",
  region_offsets = "drag_region_offsets.csv",
  label_offsets  = "drag_label_offsets.csv",
  file = "my-map.png"
)
```

If your data is in longitude/latitude, run `prepare_dragmapr_sf()` first:

``` r
my_sf <- prepare_dragmapr_sf(my_sf)
```

## State-first workflow (recommended)

The cleanest way to use dragmapr is to treat the **layout** (the computed
geometry) and the **state** (your editorial composition over it) as separate
things, and carry one `dragmapr_state` through the whole pipeline. `explodemap`
and `dragmapr` share this exact workflow — the five lines below are identical in
both packages' READMEs:

``` r
# layout = the computed geometry;  state = your editorial composition over it
library(explodemap)
library(dragmapr)

layout <- explode_grouped(my_sf, region_col = "region")   # 1. compute
state  <- as_dragmapr_state(layout)                        # 2. hand off as state

dragmapr_edit(layout, state = state)                       # 3. compose (edit)

focus_map(layout, state = state)                           # 4a. render — interactive
render_dragged_map(layout$sf_grouped,                      # 4b. render — static
                   region_col = "region", state = state)
```

`dragmapr_edit()` is the friendly front door (it accepts a projected `sf`, an
explodemap layout, or a `dragmapr_layout`). In Shiny, capture each edit back
into a state with `dragmapr_widget_state()`, and persist it with
`write_dragmapr_state()`. The `state` object is plain, reproducible data: region
and label offsets, the projected `crs`, a `geometry_id`, the selected feature,
and a monotonically increasing `version`. Offset CSVs and `as_dragmapr()` still
work, but they are now the low-level escape hatch rather than the main road. See
`inst/examples/full_state_roundtrip.R` for the full loop.

For a stronger cross-package example that starts with
`explodemap::optimize_grouped_layout()` and finishes with a persisted
`dragmapr_state`, `focus_map()`, and `render_dragged_map()`:

``` r
source(system.file("examples/explodemap_dragmapr_pipeline.R",
                   package = "dragmapr"))
```

## How It Works

1. Start with a polygon `sf` object and pick a column that groups your regions.
2. Call `drag_map_prototype()` to open a browser page — drag regions and labels wherever you like.
3. Click **Copy** or **Download** in the side panel to save the offset CSVs.
4. Call `render_dragged_map()` with those CSVs to get a static `ggplot2` image anytime, without re-running a browser session.

If you used Spatial Studio, you can skip steps 3–4 and call `render_dragmapr_project()` with the project ZIP instead:

``` r
render_dragmapr_project(
  "dragmapr-project.zip",
  file = "final-map.png",
  width = 10, height = 8, dpi = 300
)
```

## Labels and Connectors

Labels are optional. You can show one label per region, supply your own label table, or turn labels off entirely.

``` r
# One label per region (default)
drag_map_prototype(my_sf, "region")

# Text-only labels, no marker
drag_map_prototype(my_sf, "region", label_marker = FALSE)

# Circle markers instead of rectangles
drag_map_prototype(my_sf, "region", label_marker_shape = "circle")
```

For longer notes or callout boxes, use `as_drag_annotations()`:

``` r
notes <- as_drag_annotations(data.frame(
  label_id = "north-note",
  region   = "North",
  label    = "A note about this region",
  x = 50000, y = 150000
), connector = TRUE, connector_type = "curve")

render_dragged_map(my_sf, "region",
  region_offsets = "drag_region_offsets.csv",
  labels         = notes,
  label_offsets  = "drag_label_offsets.csv",
  connector_endpoint = "arrow",
  file = "annotated-map.png"
)
```

Connector styles: `"straight"`, `"elbow"`, `"curve"`, `"squiggle"`. Endpoints: `"none"` or `"arrow"`.

## RStudio Addin

After installing, go to **Addins > Launch dragmapr** in RStudio. Pick your `sf` object, choose columns and styling, drag the layout, then click **Done**. The addin saves `region_offsets` and `label_offsets` to `.GlobalEnv` so you can pass them straight to `render_dragged_map()`.

``` r
# Load your sf object first, then launch the addin
regions <- prepare_dragmapr_sf(sf::st_read("regions.shp", quiet = TRUE))
dragmapr_addin()

render_dragged_map(regions, region_col = "name",
  region_offsets = region_offsets,
  label_offsets  = label_offsets,
  file = "map.png"
)
```

## Built-in Examples

Run any example to see the package in action without needing your own data:

- `basic_draggable_map.R` — four simple map regions
- `explodemap_hhs_labels.R` — HHS-style exploded layout with colors and offsets
- `label_nudging.R` — nudge labels independently after moving regions
- `shiny_draggable_export.R` — Shiny app with live preview and PNG export
- `shiny_spatial_studio.R` — full Spatial Studio: upload files, switch groupings, drag, export
- `branch-bloom-tester.R` — test parent/child bloom animations in isolation

``` r
# Run all examples in a temp folder
source(system.file("examples", "smoke_examples.R", package = "dragmapr"))
```

Need an export format not available in Spatial Studio? Download the GeoJSON or GeoPackage and open it in [Mapshaper](https://mapshaper.org/) to convert.

## Vignettes

- Getting started with dragmapr
- Dragmap demo
- Labels and static output
- Shiny workflows
- Example gallery
- HHS placeholder shapes demo

## Known Limitations

- Designed for polygon and multipolygon data. Point and line workflows may work in parts but are not the main supported path.
- Offsets are stored in projected coordinate units. Use `prepare_dragmapr_sf()` to convert longitude/latitude data before dragging.
- Dragged geometry is for display and communication, not geographic analysis.
- Very large datasets can be slow in the browser. Simplify or group geometry first when working at large scale.

## Thank You

Thank you for trying `dragmapr`, sharing feedback, and helping make clearer map workflows easier to build.
