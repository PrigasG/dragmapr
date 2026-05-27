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

## Open Questions

- Should label dragging export separate label offsets or region-relative label
  coordinates?
- Should the widget support snapping, undo, and keyboard nudging?
- Should static rendering live in this package or be left to `ggplot2` recipes?
