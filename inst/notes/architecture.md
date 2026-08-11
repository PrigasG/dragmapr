# dragmapr Architecture Notes

## Data Model

The smallest useful persistent object is now a `dragmapr_state`. It keeps
editorial composition separate from source geometry while still preserving the
legacy offset-table workflow:

```r
d_state(
  level = "region",
  region_offsets = region_offsets,
  label_offsets = label_offsets,
  expanded_groups = character(),
  view = NULL,
  version = 12,
  crs = 3857,
  geometry_id = "hhs-2026",
  selected_feature = "North"
)
```

Region offsets remain the table-level contract inside the state:

```csv
region,dx_m,dy_m
North,1200,-500
South,-800,0
```

For labels, use a parallel table later:

```csv
label_id,region,dx_m,dy_m
North label,North,200,100
```

Region offsets move both geometries and their default label anchors. Label
offsets are an additional fine-tuning layer for text placement only.

## Rendering Model

The interactive map should never be the only source of truth. It is an editor
for `dragmapr_state`. Static plots can be regenerated from:

- source geometry,
- region grouping,
- region offsets from `state$region_offsets`,
- optional label offsets from `state$label_offsets`,
- plotting theme and legend settings.

`render_dragged_map()` accepts either legacy offset tables or `state =` a
`dragmapr_state`; explicit offset-table arguments override matching state
tables for backwards compatibility.

## Package Boundary

`dragmapr` should not know about `explodemap` specifically. It should accept any
projected `sf` object and a group column. `explodemap` can call into it later.

## Resolved Design Notes

- Label dragging exports separate label offset tables keyed by `label_id`.
- Undo/redo and keyboard nudging are implemented in the Spatial Studio app and
  can continue to mature there before becoming lower-level package APIs.
- Static rendering lives in this package through `render_dragged_map()` and
  `render_dragmapr_project()` so interactive layouts can be reproduced from
  saved state.

## Shiny Architecture Status And Roadmap

The current Shiny integration embeds the self-contained D3 helper in an iframe
and relays region and label state to Shiny through a small postMessage bridge.
That design remains valuable for standalone HTML output, but it becomes brittle
inside larger reactive applications where the helper is one component in a
larger state graph.

The main pain point is the iframe boundary:

- display-option changes can rebuild the iframe even when geometry is
  unchanged;
- a retiring iframe can send stale state after a new iframe has started;
- polling helper CSV text can invalidate export previews repeatedly;
- initial state and live state need separate handling;
- rebuilds need temporary bridge locks to prevent old messages from winning;
- division-level and state-level offsets need separate state stores;
- the Shiny app must infer when the helper is truly ready.

The package keeps `drag_map_prototype()` for standalone HTML and now includes a
native Shiny/htmlwidget surface:

```r
dragmaprOutput("map")

output$map <- renderDragmapr({
  d_widget(...)
})
```

The widget exposes structured inputs directly:

- `input$map_state`
- `input$map_region_click`
- `input$map_drag_start`
- `input$map_drag_end`
- `input$map_ready`

### Event-driven State

The Shiny widget sends state when something meaningful changes, rather than
polling CSV text:

```js
Shiny.setInputValue(
  "map_state",
  state,
  { priority: "event" }
);
```

Useful event types include:

- `ready`
- `dragstart`
- `drag`
- `dragend`
- `labeldragend`
- `branchopen`
- `branchclose`
- `selectionchange`
- `viewchange`

The 0.2.0 transition engine already points in this direction: parent and child
data are embedded once, and branch-bloom interactions run client-side without
rebuilding the iframe. The same principle should extend to ordinary display
controls.

### Client-side Update Proxy

Display-only changes are live widget updates, not geometry rebuilds:

```r
updateDragmapr(
  session,
  "map",
  show_origin_outlines = TRUE,
  show_movement_connectors = FALSE,
  show_drag_trail = TRUE,
  map_background = "white"
)
```

The proxy should toggle SVG layers, update styles, and change visibility without
reconstructing geometry. Arguments that affect geometry should remain explicit
rebuild triggers.

### Unified State Object

Region and label offsets are parallel tables inside the formal state object:

```r
d_state(
  level = "division",
  region_offsets = ...,
  label_offsets = ...,
  expanded_groups = ...,
  view = ...,
  version = 12
)
```

Implemented methods:

- `validate_dragmapr_state()`
- `merge_dragmapr_state()`
- `snapshot_dragmapr_state()`
- `restore_dragmapr_state()`
- `write_dragmapr_state()`
- `read_dragmapr_state()`
- `apply_dragmapr_state()`

### Versioned Messages

Every generated helper or widget instance should include a version token:

```r
list(
  widget_id = "composer",
  generation = 7,
  revision = 31,
  state = ...
)
```

Shiny can then ignore messages from generation 6 after generation 7 is active.
The native widget emits both `generation` and monotonic `revision`; app-level
filtering can use those fields where stale message races remain possible.

### Parent-child State Inheritance

Switching between parent and child geography levels should become a package
operation rather than app-specific logic:

```r
inherit_drag_offsets(
  state,
  from = "division",
  to = "state",
  relation = division_lookup
)

collapse_drag_offsets(
  state,
  from = "state",
  to = "division",
  method = "centroid"
)
```

### Option Boundaries

Separate options by whether they require geometry reconstruction:

- `d_geometry_options(...)`
- `d_display_options(...)`
- `d_interaction_options(...)`

This should make it clear which changes are safe to apply through
`updateDragmapr()` and which require a fresh widget build.

### Cross-package State Contract

`explodemap` now acts as a layout producer and state consumer:

```r
layout <- explodemap::explode_grouped(...)
state <- explodemap::as_dragmapr_state(layout)

d_widget(layout$sf_grouped, region_col = "region", state = state)

layout2 <- explodemap::update_exploded_layout(layout, state)
explodemap::focus_map(layout, state = state)
render_dragged_map(layout$sf_grouped, region_col = "region", state = state)
```

The computed layout and editorial state remain separate. This allows a solver
run to be recomputed while preserving manual composition as an overlay.

### Remaining Priority Order

1. Harden generation/revision filtering in larger Shiny apps.
2. Broaden `expanded_groups` and `view` restoration in the native widget.
3. Add visual QA examples for `explodemap -> dragmapr -> focus_map`.
4. Continue parent-child offset inheritance refinements.
