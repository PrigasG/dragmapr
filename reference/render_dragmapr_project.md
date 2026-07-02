# Render a static map from a Spatial Studio project bundle

`render_dragmapr_project()` turns a Spatial Studio project ZIP, or an
extracted project directory, into the same `ggplot2` image produced by
[`render_dragged_map()`](https://prigasg.github.io/dragmapr/reference/render_dragged_map.md).
It reads the saved source geometry, region offsets, label offsets, label
table, palette, and metadata, then validates that the pieces still match
before rendering.

## Usage

``` r
render_dragmapr_project(
  project,
  file = NULL,
  width = 10,
  height = 8,
  dpi = 300,
  title = NULL,
  quiet = FALSE,
  ...
)
```

## Arguments

- project:

  Path to a `dragmapr-project.zip` file or an extracted project
  directory created by Spatial Studio.

- file:

  Optional output image path. When supplied, the rendered plot is saved
  with
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

- width, height, dpi:

  Output settings used when `file` is supplied.

- title:

  Optional plot title. Defaults to the title saved in project metadata,
  when available.

- quiet:

  Use `TRUE` to suppress validation messages about zero-filled or extra
  offset rows.

- ...:

  Additional arguments passed to
  [`render_dragged_map()`](https://prigasg.github.io/dragmapr/reference/render_dragged_map.md),
  such as `legend_position`, `show_legend`, or `label_marker_shape`.

## Value

A `ggplot` object.

## Examples

``` r
if(interactive()){
render_dragmapr_project("dragmapr-project.zip", file = "map.png")
}
```
