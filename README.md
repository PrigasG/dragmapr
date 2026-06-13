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

`dragmapr` creates draggable plots from projected `sf` geometry. It writes a browser-based D3 helper where grouped shapes, labels, annotation boxes, and connector lines can be moved directly. When users want a reproducible static image afterwards, the helper exports small region and label offset CSVs that can be rendered with `ggplot2`.

The package started from an `explodemap` paper workflow, but it is now self-contained. The bundled HHS example copies only the small reusable pieces from that work: region membership, names/colors, and published display offsets.

Watch a short actual-map walkthrough in the
[dragmap demo vignette](https://prigasg.github.io/dragmapr/articles/dragmap-demo.html),
or see the
[HHS placeholder shapes demo](https://prigasg.github.io/dragmapr/articles/hhs-placeholder-shapes-demo.html)
for the embedded Spatial Studio HHS workflow.

## Install

``` r
# install.packages("pak")
pak::pak("PrigasG/dragmapr")
```

For local development:

``` r
devtools::load_all()
```

## Core Workflow

1.  Start with a projected `sf` object and a grouping column.
2.  Write a draggable browser plot helper.
3.  Drag regions and labels until the layout works.
4.  Use the draggable plot directly, or download/copy region and label offset CSVs.
5.  Render a static image either from those CSVs with `render_dragged_map()` or from a Spatial Studio project ZIP with `render_dragmapr_project()`.

``` r
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
  legend_title = "Region",
  map_background = "light_grid",
  file = "dragged-map.png"
)

render_dragmapr_project(
  "dragmapr-project.zip",
  file = "dragged-map.png"
)
```

## Labels, Info Boxes, And Connectors

Labels are optional when creating the draggable plot. There are four label concepts:

-   `make_region_labels()` derives one default label per draggable group.
-   `as_drag_labels()` accepts user-supplied labels and preserves extra columns such as tooltip or styling metadata.
-   `as_drag_annotations()` turns label rows into draggable info boxes for longer notes or callouts.
-   `read_label_state()` and `apply_label_state()` restore label movements from CSV state exported by the browser helper.

``` r
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

``` r
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
  connector_linetype = "dashed",
  connector_endpoint = "arrow",
  show_label_marker = FALSE,
  label_marker_shape = "none",
  file = "annotated-layout.png"
)
```

Supported connector geometry styles are `"straight"`, `"elbow"`, `"curve"`, and `"squiggle"`. Connector exports can also use `connector_linetype = "dashed"` or `"dotted"` and `connector_endpoint = "arrow"`. Advanced users can set `connector_start_x` / `connector_start_y` or `connector_mid_x` / `connector_mid_y` columns for custom line starts and breakpoints.

Browser and static connector lines can be styled consistently. Use
`connector_color`, `connector_linewidth`, `connector_linetype`, and
`connector_endpoint` for label connector lines. Movement context can also be
shown with `show_origin_outlines`, `show_movement_connectors`, and
`show_drag_trail` in the browser helper; static exports support origin outlines
and movement connectors with configurable color, opacity, line width, line type,
and open/closed arrow endpoints.

`legend_values` and `label_values` let Shiny apps or scripts show only selected
legend keys or selected label IDs while preserving all underlying offsets. Use
`NULL` to include everything, or pass a character vector to include a subset.

## Static Export From Offsets

The draggable helper exports two small tables:

-   region offsets: `region`, `dx_m`, `dy_m`
-   label offsets: `label_id`, `region`, `dx_m`, `dy_m`

Those CSVs are enough to reconstruct a static image later without re-running a Shiny app. `render_dragged_map()` applies region movement first, then label movement, then optional connectors and labels. It also expands plot limits around displaced labels/connectors so exported PNGs do not clip callouts.

If you used Spatial Studio, the most direct static workflow is the project ZIP:

``` r
render_dragmapr_project(
  "dragmapr-project.zip",
  file = "final-map.png",
  width = 10,
  height = 8,
  dpi = 300,
  legend_title = "Region",
  map_background = "white"
)
```

The project helper reads `source.gpkg`, `drag_region_offsets.csv`, `drag_label_offsets.csv`, `labels.csv`, `palette.csv`, and `metadata.json` from the bundle. It reports missing offset rows, unknown regions, and malformed files with messages intended to point to the exact file that needs attention. The R script exported by Spatial Studio is a convenience wrapper around this helper; keep `dragmapr-project.zip` in the same folder as the script, or edit `project_path` before running it.

Use `read_offsets()` for downloaded region CSVs and `read_label_state()` for downloaded label CSVs before passing them into `apply_offsets()`, `apply_label_state()`, or `render_dragged_map()`. The older `make_labels()`, `read_label_offsets()`, and `apply_label_offsets()` names are kept as aliases for existing scripts.

For Shiny upload workflows, `read_dragmapr_sf_upload()`, `read_dragmapr_sf_url()`, and `prepare_dragmapr_sf()` read and normalize user geometry before calling `drag_map_prototype()`. `dragmapr_iframe_bridge()` provides the JavaScript bridge used by Shiny apps to receive region and label state from the helper iframe.

## Switching Grouping Columns Without Losing Your Layout

Spatial Studio lets you change the **Group / region column** at any time while
keeping the drag layout intact. The offset tables are stored per column, and
when you switch to a column you have not visited before the positions are
**inherited** from the column you are leaving.

### Parent → child (coarser → finer grouping)

If your `sf` object has both a coarse column (e.g. `hhs_region`, ten groups)
and a fine column (e.g. `NAME`, fifty individual states), switching to the finer
column restores that column's last saved layout:

1. Drag HHS regions into an exploded layout.
2. Switch the grouping column to `NAME`.

Each state starts at its **own previously saved position** (if you have visited
`NAME` before) or at its **natural geographic position** (if this is the first
visit). Individual states are never displaced by a parent region's drag offset —
the two columns maintain independent layouts.

### Child → parent (finer → coarser grouping)

The reverse also works. After making individual fine-grained moves:

1. Drag individual states — some more than others.
2. Switch back to `hhs_region`.

Each HHS region is placed at the **average** of its member states' current
positions. This means a region whose states were all moved identically will land
exactly where you left it, while a region with mixed individual moves will land
at the centroid of those moves.

### Practical implications for your map

| Scenario | What you get |
|---|---|
| Group by county, switch to municipality | Municipalities start at their own last saved positions (or zero if never visited) |
| Group by municipality, switch back to county | Each county lands at the **average** of its municipalities' current positions |
| Switch county → municipality → county | Counties land at the mean of whatever individual municipality moves were made |
| Switch to a parent and choose "Restore" | Parent lands at the position it had when you last left it |
| Change only the label column (same region column) | Region positions are **completely unchanged** |
| Load a new spatial file | All per-column caches are cleared |

The key rule: **each column keeps its own independent layout cache.** Child
columns are never displaced by parent drags — they start fresh on first visit
and resume their own saved positions on subsequent visits.

### What resets on column switch

-   **Label offsets always reset** when the region column changes, because
    label IDs are derived from the region names of the new column.
-   **The undo/redo stack resets** so the new column starts with a clean
    history.
-   **Palette overrides and filter selections are preserved** — only the
    offset cache is transferred.

### Round-trip precision

Going child → parent involves one averaging step. If all child regions had the
same offset (e.g. you haven't individually moved any of them), the round-trip is
lossless. If you individually moved some children before switching back, those
moves are summarised into an average offset for the parent group — or you can
choose **"Restore parent's last position"** in the Column switching setting to
skip the averaging and go straight back to where the parent was.

## RStudio Addin

After installing `dragmapr`, RStudio users can open **Addins \> Launch dragmapr** to start a compact prototype gadget from an `sf` object in the global environment. Pick the region and label columns, adjust labels, connectors, legend, colours, and static output settings, render the prototype, drag the layout, then click **Done**. The addin assigns `region_offsets`, `label_offsets`, and `dragmapr_static_options` in `.GlobalEnv` so they can be passed directly to `render_dragged_map()`. Load or create the `sf` object before launching the addin; programmatic calls can pass `env =` when the object lives in another environment.

### Example: zipped shapefile to static map

If your data are a zipped shapefile, read the `.shp` file into R first, project it with `prepare_dragmapr_sf()`, then launch the addin.

``` r
library(dragmapr)
library(sf)

zip_file <- "path/to/regions.zip"
work_dir <- tempfile("dragmapr_shapefile_")
dir.create(work_dir)
unzip(zip_file, exdir = work_dir)

shp_files <- list.files(work_dir, pattern = "\\.shp$", full.names = TRUE)
if (length(shp_files) == 0L) {
  stop("Could not find a .shp file inside ", zip_file, call. = FALSE)
}

shp_file <- shp_files[1]
regions <- st_read(shp_file, quiet = TRUE)
regions <- prepare_dragmapr_sf(regions)

# Opens the RStudio gadget. Drag the layout, then click Done.
dragmapr_addin()

render_dragged_map(
  regions,
  region_col = "region_name",
  label_col = "region_name",
  region_offsets = region_offsets,
  label_offsets = label_offsets,
  file = "dragmapr-static-map.png"
)
```

Replace `"region_name"` with the column that identifies each region in your shapefile. After you click **Done**, the addin creates `region_offsets` and `label_offsets` in `.GlobalEnv`, which the final `render_dragged_map()` call uses to recreate the dragged layout.

## Built-in Examples

`dragmapr` includes examples that avoid external downloads:

The exported `example_hhs_layout()` and `example_panel_layout()` helpers return small self-contained layouts used by these examples and tests.

-   `basic_draggable_map.R`: four synthetic map regions.
-   `branch-bloom-tester.R`: standalone Shiny tester for clean branch-bloom and leaf-flip animation modes.
-   `explodemap_hhs_labels.R`: bundled HHS-style layout with explodemap colors and offsets.
-   `label_nudging.R`: independent text-marker nudges after region movement.
-   `non_map_panels.R`: dashboard/diagram rectangles, useful for non-map geometry checks.
-   `roundtrip_csv.R`: write offsets to CSV, read them back, and render.
-   `shiny_custom_labels.R`: Shiny app with user-supplied draggable labels.
-   `shiny_draggable_export.R`: Shiny app that captures drag state, previews a static plot, toggles labels/legends/connectors/info boxes, and exports PNG.
-   `shiny_draggable_plot.R`: embed a draggable plot helper in a Shiny app.
-   `shiny_spatial_studio.R`: first-pass spatial studio for local zipped shapefile, GeoJSON, or GPKG upload; reopen saved project ZIP bundles; choose grouping/labels/colors; filter visible legend keys and labels; edit label text; undo and redo drag-state changes; show the legend in drag and preview panes; switch text labels between circle/rounded-box/text-only markers; style connector and movement-context lines; drag the map; and export PNG/PDF/R-script/CSV/GeoJSON/GPKG/HTML bundles.
-   `shiny_static_export.R`: Shiny static PNG export after offsets are available.
-   `smoke_examples.R`: runs all bundled examples in a temporary directory.

### Need Another Spatial Export Format?

Spatial Studio exports adjusted geometry as GeoJSON and GeoPackage. If the
format you need is not available, download either file and open it in
[Mapshaper](https://mapshaper.org/), then export the shape format required by
your next workflow. You can use the same approach with any `sf` object created
in R: write it to a supported spatial file, open that file in Mapshaper, and
choose the desired export format.

Run the full smoke suite:

``` r
source(system.file("examples", "smoke_examples.R", package = "dragmapr"))
```

## Vignettes And Site

The package includes six vignettes:

-   Dragmap demo
-   HHS placeholder shapes demo
-   Getting started with dragmapr
-   Labels and static output
-   Example gallery
-   Shiny workflows

## Known Limitations

-   `dragmapr` is currently focused on polygon and multipolygon `sf` geometry. Point and line workflows may work in pieces, but they are not the primary supported path yet.
-   Offsets are stored in projected coordinate units, so longitude/latitude data should be prepared with `prepare_dragmapr_sf()` or another projected CRS before dragging.
-   The browser helper is designed for manual layout editing, not geodetic analysis. Dragged geometry is useful for display, reporting, and communication, but it is no longer a literal geographic position.
-   Movement context features such as origin outlines, movement connectors, and drag preview trails are visual aids. They do not change the saved region or label offset tables.
-   Spatial Studio exports adjusted geometry as GeoJSON and GeoPackage. Other formats can be produced by opening either export in Mapshaper or another GIS tool.
-   Very large datasets can be slow in the browser. For presentations and production workflows, start with simplified or grouped geometry when possible.

## Design Goals

-   Rigid region movement: every feature in a group receives the same offset.
-   Reproducible manual layout: dragging creates data, not hidden state.
-   Stable metre offsets: browser resizing should not change exported offsets.
-   Static output friendly: outputs are ordinary `ggplot2` plots and image files.
-   Label-aware: labels can move with regions and be nudged independently.
-   Annotation-aware: info boxes, text-only labels, connector lines, and static export controls are part of the same label table.
-   Small CSV surface: hand edits stay reviewable in Git.
