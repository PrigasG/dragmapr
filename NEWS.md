# dragmapr 0.3.1

* Fixed `keep_spatial_features()` so an empty id vector correctly returns an
  empty `sf` object instead of retaining every feature.
* Strengthened `sf` validation to reject an active geometry column that is not
  an `sfc` vector, including at the start of `render_dragged_map()`.
* Spatial feature alignment now preserves missing attribute types, including
  units and list columns, and feature-table metadata names remain unique when
  source attributes use reserved helper names.
* Package and site images plus the installed cheatsheet are stored as regular
  Git files so GitHub source archives contain the real assets; only demo MP4s
  remain in Git LFS.

# dragmapr 0.3.0

* Release A of the Pipeline Studio extraction adds state comparison helpers:
  `dragmapr_state_diff()` reports changed regions, labels, and interaction
  fields, while `dragmapr_state_equal()` provides the matching predicate with
  composition/interaction/all comparison modes. `summary.dragmapr_state()` now
  reports moved region and label counts, selected feature, CRS, geometry ID,
  and revision.
* Spatial feature editing is now first-class. New helpers
  `spatial_feature_table()` / `view_spatial_features()`,
  `remove_spatial_features()` / `delete_spatial_features()`,
  `keep_spatial_features()`, `add_spatial_features()`, and
  `replace_spatial_features()` let Shiny apps inspect, remove, keep, add, or
  replace source `sf` geography without hand-editing geometry columns.
  `updateDragmapr()` can now send `remove_features =` or
  `delete_selected = TRUE` to the native widget; the browser removes the
  geography from the live editor and emits `input$<id>_feature_delete`.
  Pipeline Studio exposes this in the Refine tab with source-row inspection
  and Undo/Redo so accidental deletions can be reversed.
* `dragmapr_widget()` now accepts a direct `region_palette =` argument, matching
  `render_dragged_map()` and `updateDragmapr(region_palette = )` while keeping
  the existing `dragmapr_display_options(region_palette = )` route intact.
* New `inst/shiny/pipeline-studio` Shiny app (shipped with both dragmapr and
  explodemap) demonstrates the full cross-package workflow on real US geography:
  an explodemap national HHS map and county drill-down, the draggable editor
  with `dragmapr_state` round-trip, and a combined compute -> compose -> render
  -> persist studio. Run with
  `shiny::runApp(system.file("shiny/pipeline-studio", package = "dragmapr"))`.
* Added `inst/examples/explodemap_dragmapr_pipeline.R`, a complete
  cross-package example covering `explodemap` layout optimization and
  diagnostics, editable `dragmapr_state`, JSON persistence, `focus_map()`, and
  `render_dragged_map()`.
* `dragmapr_state` carries a `view` (viewport) field that round-trips through
  JSON and the widget snapshot. It is preserved as composition metadata;
  restoring the browser to a saved viewport on load is intentionally left for a
  future release (selection restoration is the clean first cut).
* Every public entry point that accepts `sf` geometry (`dragmapr_widget()`,
  `dragmapr_edit()`, `apply_offsets()`, `apply_dragmapr_state()`,
  `render_dragged_map()`) now validates the active sf geometry column up front
  via a shared check, so a malformed `sf` fails with one clear message instead
  of a deep, cryptic error.
* New `dragmapr_edit()` is a friendly front-door to the interactive editor and
  the composition step of the state-first workflow. It accepts a projected
  `sf`, an explodemap `grouped_exploded_map`, or a `dragmapr_layout` (plus an
  optional `dragmapr_state`) and returns a configured `dragmapr_widget()`.
  Capture edits back into a state with `dragmapr_widget_state()`.

* `dragmapr_state()` gains three optional, back-compatible fields that make a
  saved composition durable across sessions: `crs` (the projected CRS the
  metre offsets are expressed in, stored as an EPSG integer or WKT string so it
  round-trips through JSON), `geometry_id` (a provenance tag identifying the
  geometry the state was composed against), and `selected_feature` (the feature
  focused in an editor or dashboard). The fields are threaded through
  `validate_`, `merge_`, `snapshot_`/`restore_`, `inherit_drag_offsets()` and
  `collapse_drag_offsets()`; `selected_feature` is dropped on a level change.
  `apply_dragmapr_state()` now warns when a state's `crs` differs from the
  target geometry. Older state JSON written before these fields still restores.
* `explodemap::as_dragmapr_state()` is the matching producer: it converts a
  grouped layout's absolute anchors into `dx_m`/`dy_m` deltas and emits a
  `dragmapr_state`, keeping the computed layout and the editorial state as
  separate objects.
* The native widget bridge now carries `crs`, `geometry_id` and
  `selected_feature`. They are surfaced at the top of the widget payload, the
  browser highlights the selected region (`.is-selected`), a region click
  updates the selection and reports it back to Shiny, and the round-trip state
  snapshot includes all three fields. `updateDragmapr()` gains live
  `selected_feature` updates (pass `NULL` or `""` to clear).
* New `dragmapr_widget_state()` is the inbound half of the bridge: it
  reconstructs a `dragmapr_state` from the `paste0(outputId, "_state")` Shiny
  input the widget emits, so server code can persist, merge, or re-render a
  live composition. The browser's `revision` is preserved as the state
  `version` (the client owns the live counter).
* New `inst/examples/full_state_roundtrip.R` Shiny app walks the whole state
  contract: an upstream layout state is edited in the native widget, every edit
  is rebuilt server-side with `dragmapr_widget_state()`, a selection is pushed
  back from R with `updateDragmapr()`, and the live `dragmapr_state` drives both
  a saved JSON file and a static `render_dragged_map()` re-render.
* `render_dragged_map()` now accepts `state =` a `dragmapr_state`, using its
  region and label offsets for static output while preserving the existing
  `region_offsets` / `label_offsets` table arguments as explicit overrides.
* Revision semantics are now monotonic: `merge_dragmapr_state()`,
  `inherit_drag_offsets()` and `collapse_drag_offsets()` bump `version` to one
  past the highest input revision (via the internal `bump_revision()`), so a
  state produced by an accepted edit always sorts after its inputs. Previously
  `merge_dragmapr_state()` took the maximum of the two revisions unchanged.

# dragmapr 0.2.0

* `drag_map_prototype()` gains an optional `transition` argument: the browser
  helper runs one client-side leaf-flip branch animation for parent-to-child
  bloom and child-to-parent unbloom, and draws a dotted group-drag boundary per
  expanded group. Clicking a boundary posts a
  `dragmapr-collapse-branch` message to the embedding page, or resets the
  branch when the helper runs standalone; a plain click on a region posts
  `dragmapr-region-click`.
* New `inst/examples/branch-bloom-tester.R` Shiny example isolates the
  branch-bloom helper so clean bloom and leaf-flip behavior can be tested
  without the full Spatial Studio shell.
* New packaged cheatsheet in `inst/cheatsheets/` covers the core workflow,
  labels, static rendering, Spatial Studio, hierarchy/bloom helpers, CRS
  diagnostics, layout snapshots, and transition utilities.
* New upload intelligence helpers: `detect_hierarchy_columns()`,
  `recommend_dragmapr_hierarchy()`, `validate_bloom_hierarchy()`,
  `build_branch_transition_data()`, `make_branch_bloom_labels()`,
  `summarise_spatial_crs()`, and `profile_spatial_upload()` move
  parent/child detection, CRS meaning, branch-bloom data prep, and safe
  parent-first label setup into reusable package functions.
* Spatial Studio: new **Bloom** sidebar section. Pick a child column ("bloom
  into"), then click a parent region on the map (or choose parents in the
  panel) to expand just that parent into its children with a leaf flip -
  the rest of the map keeps the parent grouping. At most two parents can be
  expanded at a time (expanding another replaces the oldest), the dotted
  reset boundary can be toggled off, and clicking the boundary or the
  children compresses the branch and restores the parent's saved position.
  The bloom target must genuinely nest inside the current grouping
  (data-driven check with a clear error otherwise). Arming bloom embeds both
  parent and child keys in the helper once; expanding and collapsing then
  run fully client-side over postMessage, so there is no iframe rebuild, no
  loading overlay, and the animation always plays. The dotted frame is also
  the group's drag handle: dragging it moves the whole expanded branch
  (children, labels, and frame together), and collapsing places the parent
  at the mean of its children's positions. With "Dissolve to parent shells
  when collapsed" (on by default), child-level uploads display as clean
  dissolved parent outlines until a parent blooms, which both sharpens the
  reveal and reduces the number of drawn shapes.
* Spatial Studio: the Bloom panel now includes an animation selector so users
  can switch between the leaf-flip proxy animation and the clean branch-bloom
  animation. The selected mode is sent to the live helper without rebuilding
  the iframe.
* Spatial Studio: one unified full-app busy veil replaces the previous mix of
  map overlay, sidebar freeze, and full veil. Two modes share the same
  blocker - "Loading spatial data" (uploads, project opens) and "Processing
  changes" (helper rebuilds) - with lock counting so overlapping tasks never
  hide the veil early, a short show delay so fast rebuilds don't flash, and
  safety timers so a missed release can never freeze the app. The Shiny busy
  state only drives the slim top progress bar; client-side bloom
  interactions never show a veil.
* Spatial Studio fixes for files with arbitrary column names: hierarchy
  detection on column switches is data-driven (the child column must actually
  nest inside the parent grouping), and the default group/region column
  detection no longer assumes specific column names or excludes
  one-row-per-region files (e.g. `COUNTY_NAM` instead of `COUNTY`).

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
