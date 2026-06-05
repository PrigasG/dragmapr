# dragmapr 0.1.0

Initial CRAN release.

* `drag_map_prototype()` writes a self-contained D3 browser helper where
  grouped `sf` regions, labels, and annotation boxes can be dragged freely.
  It exports region and label offset CSVs.
* `render_dragged_map()` reconstructs the dragged layout as a `ggplot2` image
  from the source geometry plus offset tables.
* `render_dragmapr_project()` renders a complete Spatial Studio project bundle
  (ZIP) in one call.
* `make_region_labels()`, `as_drag_labels()`, and `as_drag_annotations()`
  build label tables; `read_label_state()` and `apply_label_state()` restore
  saved label positions.
* `read_offsets()` and `apply_offsets()` handle region offset I/O.
* `read_dragmapr_sf_upload()`, `read_dragmapr_sf_url()`, and
  `prepare_dragmapr_sf()` read and normalise spatial files for Shiny workflows.
* `dragmapr_iframe_bridge()` provides the JavaScript bridge for relaying drag
  state from the helper iframe back to Shiny inputs.
* `dragmapr_addin()` registers an RStudio gadget under **Addins > Launch
  dragmapr** that embeds the prototype in the viewer pane and assigns
  `region_offsets` and `label_offsets` to the target environment on completion
  (`.GlobalEnv` when launched from the RStudio Addins menu).
* Interactive and static outputs support selective legend and label rendering
  with `legend_values` and `label_values`, preserving stored offsets for hidden
  labels.
* Label connector lines can be styled by color, width, line pattern, and arrow
  endpoint in both the browser helper and static exports.
* Movement context controls can show origin outlines, movement connector lines,
  and browser-only drag preview trails. Movement connectors support configurable
  color, opacity, width, line pattern, and open/closed endpoints.
* Spatial Studio demonstrates legend and label multiselect filters, connector
  styling, movement context controls, project persistence, and static bundle
  export.
* Release hardening: package-internal `%||%` is defined for the RStudio addin,
  prototype output defaults to a temporary HTML file unless `file` is supplied,
  connector linetypes are validated in static exports, the Shiny iframe bridge
  stops polling on disconnect/unload, CRS-less inputs warn before assuming the
  target CRS, legacy label helper aliases emit deprecation warnings, and the
  bundled D3 license is included under `inst/prototype/`.
