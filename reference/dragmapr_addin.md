# RStudio addin for the interactive drag-map prototype

Launches a compact Shiny gadget that lets you pick a projected `sf`
object from an environment, choose the region and label columns, adjust
label, connector, legend, label-filter, movement-context, colour, and
static-output settings, and interact with the D3 drag-map prototype
inside the IDE viewer pane. When you click **Done**, the current region
and label offsets are assigned as `region_offsets` and `label_offsets`
in the same environment, ready to pass to
[`render_dragged_map()`](https://prigasg.github.io/dragmapr/reference/render_dragged_map.md).

## Usage

``` r
dragmapr_addin(env = dragmapr_global_env())
```

## Arguments

- env:

  Environment to scan for `sf` objects and receive exported offset
  tables. Defaults to `.GlobalEnv`, which is what RStudio uses for
  addins.

## Value

Invisibly returns a list with elements `region_offsets`,
`label_offsets`, and `static_options`. The offset data frames are also
assigned into `env` as `region_offsets` and `label_offsets`; static
render settings are assigned as `dragmapr_static_options`.

## Details

The addin appears under **Addins \> Launch dragmapr** in RStudio once
the package is installed.

## See also

[`drag_map_prototype()`](https://prigasg.github.io/dragmapr/reference/drag_map_prototype.md)
for the underlying HTML generator;
[`render_dragged_map()`](https://prigasg.github.io/dragmapr/reference/render_dragged_map.md)
to reconstruct a static ggplot2 image from the returned offsets.
