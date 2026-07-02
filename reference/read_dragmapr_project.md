# Read a dragmapr project bundle

Reads a Spatial Studio project ZIP or extracted project directory and
returns its components as a named list. This is the low-level companion
to
[`render_dragmapr_project()`](https://prigasg.github.io/dragmapr/reference/render_dragmapr_project.md):
use it when you want to access the raw source geometry, offsets, labels,
or palette programmatically before rendering.

## Usage

``` r
read_dragmapr_project(project)
```

## Arguments

- project:

  Path to a `dragmapr-project.zip` file or an extracted project
  directory created by Spatial Studio.

## Value

A named list with elements:

- `source`:

  The source `sf` object read from `source.gpkg`.

- `region_offsets`:

  Data frame from `drag_region_offsets.csv`, or `NULL` if not present.

- `label_offsets`:

  Data frame from `drag_label_offsets.csv`, or `NULL` if not present.

- `labels`:

  Label table from `labels.csv` (as returned by
  [`as_drag_labels()`](https://prigasg.github.io/dragmapr/reference/as_drag_labels.md)),
  or `NULL`.

- `region_palette`:

  Named character vector of colors from `palette.csv`, or `NULL`.

- `metadata`:

  Named list parsed from `metadata.json`.

- `path`:

  Path to the extracted project directory.

## See also

[`render_dragmapr_project()`](https://prigasg.github.io/dragmapr/reference/render_dragmapr_project.md)
to render a project bundle directly;
[`write_dragmapr_project()`](https://prigasg.github.io/dragmapr/reference/write_dragmapr_project.md)
to create a project bundle from R objects.

## Examples

``` r
if(interactive()){
bundle <- read_dragmapr_project("dragmapr-project.zip")
names(bundle)
nrow(bundle$source)
}
```
