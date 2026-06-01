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
