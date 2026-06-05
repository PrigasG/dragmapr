# dragmapr Architecture Notes

## Data Model

The smallest useful persistent object is an offset table:

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
for offset tables. Static plots should be regenerated from:

- source geometry,
- region grouping,
- region offsets,
- optional label offsets,
- plotting theme and legend settings.

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
