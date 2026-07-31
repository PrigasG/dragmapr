# Package index

## State-first composition

Keep manual edits in a dragmapr_state, then use the same state for
browser editing, Shiny apps, static rendering, snapshots, and JSON
round-trips.

- [`dragmapr_edit()`](https://prigasg.github.io/dragmapr/reference/dragmapr_edit.md)
  : Open the interactive dragmapr editor
- [`dragmapr_state()`](https://prigasg.github.io/dragmapr/reference/dragmapr_state.md)
  : Create a dragmapr state object
- [`dragmapr_state_diff()`](https://prigasg.github.io/dragmapr/reference/dragmapr_state_diff.md)
  [`dragmapr_state_equal()`](https://prigasg.github.io/dragmapr/reference/dragmapr_state_diff.md)
  : Compare dragmapr states
- [`summary(`*`<dragmapr_state>`*`)`](https://prigasg.github.io/dragmapr/reference/summary.dragmapr_state.md)
  : Summarise a dragmapr state
- [`validate_dragmapr_state()`](https://prigasg.github.io/dragmapr/reference/validate_dragmapr_state.md)
  : Validate a dragmapr state object
- [`merge_dragmapr_state()`](https://prigasg.github.io/dragmapr/reference/merge_dragmapr_state.md)
  : Merge dragmapr state updates
- [`inherit_drag_offsets()`](https://prigasg.github.io/dragmapr/reference/inherit_drag_offsets.md)
  : Inherit drag offsets between hierarchy levels
- [`collapse_drag_offsets()`](https://prigasg.github.io/dragmapr/reference/collapse_drag_offsets.md)
  : Collapse drag offsets between hierarchy levels
- [`apply_dragmapr_state()`](https://prigasg.github.io/dragmapr/reference/apply_dragmapr_state.md)
  : Apply a dragmapr state to sf geometry
- [`snapshot_dragmapr_state()`](https://prigasg.github.io/dragmapr/reference/snapshot_dragmapr_state.md)
  [`restore_dragmapr_state()`](https://prigasg.github.io/dragmapr/reference/snapshot_dragmapr_state.md)
  : Snapshot or restore dragmapr state
- [`write_dragmapr_state()`](https://prigasg.github.io/dragmapr/reference/write_dragmapr_state.md)
  [`read_dragmapr_state()`](https://prigasg.github.io/dragmapr/reference/write_dragmapr_state.md)
  : Read and write dragmapr state JSON

## Native widget (htmlwidget and Shiny)

Render a draggable map directly in Shiny, receive structured edit
events, and update display options without rebuilding the geometry.

- [`dragmapr_widget()`](https://prigasg.github.io/dragmapr/reference/dragmapr_widget.md)
  : Create a native dragmapr htmlwidget
- [`dragmapr_widget_state()`](https://prigasg.github.io/dragmapr/reference/dragmapr_widget_state.md)
  : Reconstruct a dragmapr state from a browser state event
- [`dragmaprOutput()`](https://prigasg.github.io/dragmapr/reference/dragmaprOutput.md)
  [`renderDragmapr()`](https://prigasg.github.io/dragmapr/reference/dragmaprOutput.md)
  : Shiny bindings for dragmapr widgets
- [`updateDragmapr()`](https://prigasg.github.io/dragmapr/reference/updateDragmapr.md)
  : Update a dragmapr widget in place
- [`dragmapr_geometry_options()`](https://prigasg.github.io/dragmapr/reference/dragmapr_geometry_options.md)
  [`dragmapr_display_options()`](https://prigasg.github.io/dragmapr/reference/dragmapr_geometry_options.md)
  [`dragmapr_interaction_options()`](https://prigasg.github.io/dragmapr/reference/dragmapr_geometry_options.md)
  : dragmapr widget options

## Interactive prototype

Write a standalone HTML editor for quick layout work outside a Shiny
app.

- [`drag_map_prototype()`](https://prigasg.github.io/dragmapr/reference/drag_map_prototype.md)
  : Write a draggable map in your browser

## Region offsets

Read and apply exported region movement tables.

- [`read_offsets()`](https://prigasg.github.io/dragmapr/reference/read_offsets.md)
  : Read region offsets from CSV
- [`apply_offsets()`](https://prigasg.github.io/dragmapr/reference/apply_offsets.md)
  : Apply rigid offsets to grouped sf geometries

## Labels

Derive default label anchors, supply custom labels or annotation boxes,
configure connectors, and restore label positions from exported state.

- [`make_region_labels()`](https://prigasg.github.io/dragmapr/reference/make_region_labels.md)
  : Derive one default label per draggable region
- [`as_drag_labels()`](https://prigasg.github.io/dragmapr/reference/as_drag_labels.md)
  : Coerce data to a drag label table
- [`as_drag_annotations()`](https://prigasg.github.io/dragmapr/reference/as_drag_annotations.md)
  : Coerce data to draggable annotation boxes
- [`select_label_ids()`](https://prigasg.github.io/dragmapr/reference/select_label_ids.md)
  : Select a readable subset of label IDs
- [`read_label_state()`](https://prigasg.github.io/dragmapr/reference/read_label_state.md)
  : Read draggable label state from CSV
- [`apply_label_state()`](https://prigasg.github.io/dragmapr/reference/apply_label_state.md)
  : Apply draggable label state to a label table

## Static rendering

Rebuild edited layouts as ggplot2 images, PNGs, PDFs, or project
bundles.

- [`render_dragged_map()`](https://prigasg.github.io/dragmapr/reference/render_dragged_map.md)
  : Save the dragged layout as a static image
- [`render_dragmapr_project()`](https://prigasg.github.io/dragmapr/reference/render_dragmapr_project.md)
  : Render a static map from a Spatial Studio project bundle
- [`read_dragmapr_project()`](https://prigasg.github.io/dragmapr/reference/read_dragmapr_project.md)
  : Read a dragmapr project bundle
- [`write_dragmapr_project()`](https://prigasg.github.io/dragmapr/reference/write_dragmapr_project.md)
  : Write a dragmapr project bundle

## Built-in examples

Self-contained fixtures for testing and demonstrations.

- [`example_hhs_layout()`](https://prigasg.github.io/dragmapr/reference/example_hhs_layout.md)
  : Build a small explodemap-style HHS example layout
- [`example_panel_layout()`](https://prigasg.github.io/dragmapr/reference/example_panel_layout.md)
  : Build a non-map panel layout example

## Hierarchy and branch bloom

Detect parent-child grouping columns, prepare branch-bloom helper data,
and explain CRS meaning for uploaded spatial files.

- [`make_hierarchy_key()`](https://prigasg.github.io/dragmapr/reference/make_hierarchy_key.md)
  : Build composite group keys from multiple columns
- [`inherit_layout()`](https://prigasg.github.io/dragmapr/reference/inherit_layout.md)
  : Inherit region offsets from a coarser to a finer grouping column
- [`create_layout_snapshot()`](https://prigasg.github.io/dragmapr/reference/create_layout_snapshot.md)
  : Save a layout snapshot for a grouped sf dataset
- [`restore_layout_snapshot()`](https://prigasg.github.io/dragmapr/reference/restore_layout_snapshot.md)
  : Restore a layout snapshot
- [`detect_hierarchy_columns()`](https://prigasg.github.io/dragmapr/reference/detect_hierarchy_columns.md)
  : Detect parent-child grouping columns
- [`recommend_dragmapr_hierarchy()`](https://prigasg.github.io/dragmapr/reference/recommend_dragmapr_hierarchy.md)
  : Recommend a hierarchy for dragmapr
- [`validate_bloom_hierarchy()`](https://prigasg.github.io/dragmapr/reference/validate_bloom_hierarchy.md)
  : Validate a parent-child bloom hierarchy
- [`build_branch_transition_data()`](https://prigasg.github.io/dragmapr/reference/build_branch_transition_data.md)
  : Build leaf-flip transition data
- [`make_branch_bloom_labels()`](https://prigasg.github.io/dragmapr/reference/make_branch_bloom_labels.md)
  : Build parent and child labels for branch bloom
- [`transition_options()`](https://prigasg.github.io/dragmapr/reference/transition_options.md)
  : Options for local elastic hierarchy transitions
- [`build_elastic_transition()`](https://prigasg.github.io/dragmapr/reference/build_elastic_transition.md)
  : Build a local elastic parent-to-child transition
- [`make_group_boundaries()`](https://prigasg.github.io/dragmapr/reference/make_group_boundaries.md)
  : Build dotted group-drag boundaries for expanded groups
- [`layout_metrics()`](https://prigasg.github.io/dragmapr/reference/layout_metrics.md)
  : Measure mental-map stability between two layouts
- [`summarise_spatial_crs()`](https://prigasg.github.io/dragmapr/reference/summarise_spatial_crs.md)
  : Summarise CRS meaning for dragmapr
- [`profile_spatial_upload()`](https://prigasg.github.io/dragmapr/reference/profile_spatial_upload.md)
  : Profile a spatial upload for dragmapr
- [`suggest_offsets()`](https://prigasg.github.io/dragmapr/reference/suggest_offsets.md)
  : Suggest automatic region offsets for a draggable map
- [`dragmapr_diagnostics()`](https://prigasg.github.io/dragmapr/reference/dragmapr_diagnostics.md)
  : Summarise a spatial dataset for use with dragmapr

## Spatial I/O helpers

Read spatial files from Shiny uploads or URLs, project and validate
geometry, and reuse the bridge helpers that power the spatial studio
apps.

- [`read_dragmapr_sf_upload()`](https://prigasg.github.io/dragmapr/reference/read_dragmapr_sf_upload.md)
  : Read an sf object from a Shiny file upload
- [`read_dragmapr_sf_url()`](https://prigasg.github.io/dragmapr/reference/read_dragmapr_sf_url.md)
  : Download and read an sf object from a URL
- [`prepare_dragmapr_sf()`](https://prigasg.github.io/dragmapr/reference/prepare_dragmapr_sf.md)
  : Prepare an sf object for use with dragmapr
- [`dragmapr_iframe_bridge()`](https://prigasg.github.io/dragmapr/reference/dragmapr_iframe_bridge.md)
  : Build the Shiny iframe bridge JavaScript for a draggable helper

## Editable spatial features

Inspect, remove, keep, add, or replace source geography before or during
a dragmapr workflow. These helpers support Shiny review tables and
reversible geography edits.

- [`spatial_feature_table()`](https://prigasg.github.io/dragmapr/reference/spatial_feature_table.md)
  [`view_spatial_features()`](https://prigasg.github.io/dragmapr/reference/spatial_feature_table.md)
  : Inspect editable spatial features
- [`remove_spatial_features()`](https://prigasg.github.io/dragmapr/reference/remove_spatial_features.md)
  [`delete_spatial_features()`](https://prigasg.github.io/dragmapr/reference/remove_spatial_features.md)
  [`keep_spatial_features()`](https://prigasg.github.io/dragmapr/reference/remove_spatial_features.md)
  : Remove or keep spatial features
- [`add_spatial_features()`](https://prigasg.github.io/dragmapr/reference/add_spatial_features.md)
  [`replace_spatial_features()`](https://prigasg.github.io/dragmapr/reference/add_spatial_features.md)
  : Add or replace spatial features

## RStudio addin

Launch the interactive prototype from the RStudio Addins menu.

- [`dragmapr_addin()`](https://prigasg.github.io/dragmapr/reference/dragmapr_addin.md)
  : RStudio addin for the interactive drag-map prototype

## Compatibility aliases

Earlier names kept for backward compatibility.

- [`make_labels()`](https://prigasg.github.io/dragmapr/reference/make_labels.md)
  : Derive one label anchor per draggable region
- [`read_label_offsets()`](https://prigasg.github.io/dragmapr/reference/read_label_offsets.md)
  : Read label offsets from CSV
- [`apply_label_offsets()`](https://prigasg.github.io/dragmapr/reference/apply_label_offsets.md)
  : Apply label-specific offsets to a label table
