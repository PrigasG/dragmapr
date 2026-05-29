# dragmapr 0.1.0

* `as_drag_annotations()`: fixed a bug where the `connector` and
  `connector_type` arguments were silently ignored when labels came from
  `make_region_labels()`. `normalize_labels()` pre-fills those columns with
  `FALSE` / `"straight"`, so the previous "column absent" guard always skipped
  the assignment. Both arguments are now applied unconditionally.
* `shiny_spatial_studio.R`: all four Shiny custom-message handlers that relay
  commands into the drag-map iframe now target the specific
  `iframe.studio-helper-frame` element rather than the generic first `<iframe>`
  in the document. Shiny adds invisible internal iframes (download handlers,
  etc.) that could intercept messages and prevent the legend, label data, and
  label-option updates from reaching the map helper.
* Loading veil now dismisses only after the drag-map iframe has fully loaded in
  the browser - the last step in the full data-to-project-to-build-to-render
  pipeline.

## Development history: studio polish and high-cardinality fix

* Polished the Spatial Studio example for hosted demos: the embedded D3 helper
  can now hide its built-in side panel with `side_panel = FALSE`, giving Shiny
  apps the full helper canvas while keeping standalone HTML exports unchanged.
* Improved D3 helper drag performance by updating transforms/connectors during
  drag instead of rebuilding the full SVG on every mouse move.
* Added a package hex logo and tightened Spatial Studio upload/URL error
  messages so unsupported files, invalid zip archives, and unreadable spatial
  data fail with clearer guidance.
* Spatial Studio now keeps the D3 helper's region/label offset panel visible by
  default, with a toggle for users who want a larger drag canvas.
* Ordinary draggable text labels now use adjustable rounded-rectangle markers
  in the D3 helper, with width and height controls in Spatial Studio. Connector
  paths are also kept alive from initial render, so connectors appear correctly
  after dragging info boxes or labels.
* Spatial Studio now shows a subtle loading overlay while uploads, URLs, or the
  bundled demo are being ingested, so large spatial files do not look frozen.
* The browser helper now identifies the active region on hover and while
  dragging, making it easier to pull polygons apart without first adding labels.
* The browser helper can now show a compact draggable-pane legend, and Spatial
  Studio uses the same legend toggle and position control for drag and preview
  panes.
* Ordinary text labels can now use circle markers, rounded-rectangle markers,
  or text-only drag targets. Spatial Studio and the Shiny export example expose
  these choices, including a circle-radius slider.
* Spatial Studio now gives the drag pane more horizontal and vertical room so
  larger info boxes are less likely to feel cramped while dragging.
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

## Development history: studio polish

* Polished `shiny_spatial_studio.R` UI: per-region `shinyWidgets::colorPickr()` swatches
  (with graceful fallback to comma-separated text input when shinyWidgets is not installed),
  inline status bar with colored ok / info / error states, sidebar section headers, and a
  two-column download grid.
* Added `shinyWidgets` to `Suggests`.

## Development history: second pass

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
