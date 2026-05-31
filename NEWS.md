# dragmapr 0.1.0

First public release.

## New features

* `drag_map_prototype()` gains `label_text_size` for controlling label font
  size in the browser helper, and `label_marker_shape` for choosing between
  rounded-box, circle, or text-only label markers.
* `drag_map_prototype()` gains `show_legend`, `legend_position`, and
  `max_legend_keys` to embed a compact colour legend in the helper; a
  `region_palette` argument keeps the D3 colours in sync with
  `render_dragged_map()`; and `side_panel = FALSE` lets Shiny apps hide the
  built-in copy/download panel.
* `render_dragged_map()` gains `max_legend_keys` (default 25): when the number
  of distinct groups exceeds the threshold the legend is suppressed
  automatically with an informational message. Set `Inf` to always show.
* `dragmapr_iframe_bridge()` gains `iframe_selector` so custom Shiny apps can
  target a specific iframe element when other iframes (e.g. Shiny download
  handlers) may be present in the page.
* Region group names are now sorted in natural (numeric-aware) order throughout
  the package: "1", "2", ..., "10" rather than lexicographic "1", "10", "2".
* The Spatial Studio label controls are now consolidated dynamically — only
  the sliders relevant to the current annotation style are shown.
* Four reusable Shiny helpers extracted to package level:
  `read_dragmapr_sf_upload()`, `read_dragmapr_sf_url()`,
  `prepare_dragmapr_sf()`, and `dragmapr_iframe_bridge()`.
* The Spatial Studio accepts polygon uploads and URLs; exports PNG, Region
  CSV, Label CSV, GeoJSON, GPKG, standalone HTML helper, and a bundle ZIP.

## Bug fixes

* `as_drag_annotations()`: the `connector` and `connector_type` arguments were
  silently ignored when labels came from `make_region_labels()`. Both arguments
  are now applied unconditionally.
* Legend and label-style changes in the Spatial Studio now reliably reach the
  drag-map iframe. All postMessage handlers now target
  `iframe.studio-helper-frame` specifically rather than the first `<iframe>`
  in the document.
* The loading veil in the Spatial Studio now dismisses only after D3 has
  completed its first `render()` call, using a `dragmapr-ready` postMessage,
  not the iframe `load` event (which races with listener setup for fast local
  resources).
* `connectorLayer.raise()` removed from the D3 template — it inverted the
  intended stacking order and caused connector lines to render above label
  text.
* CRS error messages in `drag_map_prototype()`, `make_region_labels()`, and
  `apply_offsets()` now mention `prepare_dragmapr_sf()` and
  `sf::st_transform()` as the remedy.
* `stats` added to `Imports` (`stats::setNames` was used without being
  declared — R CMD check error on strict platforms).

## Documentation

* Getting-started vignette expanded to cover all 22 parameters of
  `drag_map_prototype()` with a table and code example per parameter group.
* `@seealso` cross-links added across the five exported function families.
* Vignettes and examples audited and synced with current API.
