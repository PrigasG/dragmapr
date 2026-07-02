# Create a dragmapr state object

A `dragmapr_state` stores the mutable, editorial layout of a draggable
map separately from the source geometry and from display options. It is
the shared composition contract handed between layout producers (such as
[`explodemap::as_dragmapr_state()`](https://prigasg.github.io/explodemap/reference/as_dragmapr_state.html)),
the interactive editor, and static renderers.

## Usage

``` r
dragmapr_state(
  level = "region",
  region_offsets = NULL,
  label_offsets = NULL,
  expanded_groups = character(),
  view = NULL,
  version = 0L,
  crs = NULL,
  geometry_id = NULL,
  selected_feature = NULL
)
```

## Arguments

- level:

  Character label for the active geography level.

- region_offsets:

  Data frame with `region`, `dx_m`, and `dy_m`.

- label_offsets:

  Data frame with `label_id`, `region`, `dx_m`, and `dy_m`.

- expanded_groups:

  Character vector of expanded parent groups.

- view:

  Optional list describing the client view, such as scale and
  translation.

- version:

  Integer state revision.

- crs:

  Optional coordinate reference system the metre offsets are expressed
  in. Accepts anything
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
  understands (an EPSG code, a PROJ/WKT string, or a `crs` object).
  Stored as an EPSG integer when available, otherwise as a WKT string,
  so it round-trips through JSON. Because `dx_m`/`dy_m` are only
  meaningful in a projected CRS, recording it here makes a saved state
  safe to reapply in a later session.

- geometry_id:

  Optional single string identifying the geometry this state was
  composed against (provenance / binding tag). Lets a downstream
  consumer detect when a state is being applied to a different dataset
  than it was authored for.

- selected_feature:

  Optional single string naming the feature currently selected in a
  dashboard or editor. Carried so a composition can be restored with
  focus intact.

## Value

An object of class `"dragmapr_state"`.

## Details

The state is deliberately geometry-free: it carries *deltas*, not
absolute geometry, and is bound to features at apply time via a join
key. That key is the `region` column of the offset tables. For durable
round-trips prefer a stable code (e.g. a FIPS/ISO id) over a display
name, and record which geometry the state was composed against with
`geometry_id`.
