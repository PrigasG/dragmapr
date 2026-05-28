# dragmapr 0.0.0.9000 (third pass - studio polish + high-cardinality fix)

* Polished the Spatial Studio example for hosted demos: the embedded D3 helper
  can now hide its built-in side panel with `side_panel = FALSE`, giving Shiny
  apps the full helper canvas while keeping standalone HTML exports unchanged.
* Improved D3 helper drag performance by updating transforms/connectors during
  drag instead of rebuilding the full SVG on every mouse move.
* Added `stats` to `Imports` in `DESCRIPTION` (`stats::setNames` was used in
  `R/examples.R` without being declared - R CMD check error on strict platforms).
* Removed `--compact-vignettes=gs+qpdf` from the GitHub Actions check workflow;
  Ghostscript and qpdf are not reliably available on macOS/Windows runners.

* `render_dragged_map()` gains a `max_legend_keys` parameter (default `25`).
  When the number of distinct region groups exceeds the threshold the legend is
  suppressed automatically with an informational message.  Set `Inf` to always
  show.
* `shiny_spatial_studio.R`: when the chosen region column has more than 20
  unique values, the sidebar switches from individual per-region colorPickr
  swatches to a compact cycle-palette editor (10 base colors that repeat).
* Added `post_load_hints()` inside the studio: after loading any file, the
  status bar reports group count and warns when the legend will be auto-hidden.

# dragmapr 0.0.0.9000 (third pass - studio polish)

* Polished `shiny_spatial_studio.R` UI: per-region `shinyWidgets::colorPickr()` swatches
  (with graceful fallback to comma-separated text input when shinyWidgets is not installed),
  inline status bar with colored ok / info / error states, sidebar section headers, and a
  two-column download grid.
* Added `shinyWidgets` to `Suggests`.

# dragmapr 0.0.0.9000 (second pass)

* Added draggable region and label prototype export.
* Added region-offset and label-offset CSV readers and appliers.
* Added static rendering with `ggplot2`.
* Added draggable info boxes with `as_drag_annotations()`.
* Added text-only draggable labels, optional label markers, and static label
  styling controls.
* Added connector/leader lines for labels and info boxes, including straight,
  elbow, curve, and squiggle styles, adjustable thickness, optional custom
  starts/breakpoints, endpoint trimming, and static plot padding to avoid
  clipping callouts.
* Added Shiny examples for draggable plots, custom labels, live state capture,
  labels/info boxes/connectors, and optional static PNG export.
* Added a first-pass Shiny spatial studio example for local polygon uploads,
  grouping/label selection, palette editing, dragging, annotation controls, and
  PNG/CSV/GeoJSON/HTML downloads.
* Added second-pass spatial studio: URL input via `read_dragmapr_sf_url()`,
  GPKG download, and project bundle ZIP download (source geometry + region/label
  offset CSVs + palette + metadata).
* Extracted four reusable Shiny-facing helpers from the spatial studio:
  `read_dragmapr_sf_upload()`, `read_dragmapr_sf_url()`,
  `prepare_dragmapr_sf()`, and `dragmapr_iframe_bridge()`.  Users can import
  these into their own apps rather than copying the studio internals.
* Added bundled HHS and non-map panel examples.
* Added vignettes and pkgdown site scaffolding.
