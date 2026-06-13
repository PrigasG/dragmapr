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
