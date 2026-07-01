# Write a dragmapr project bundle

Packages a source `sf` object together with region offsets, label
offsets, labels, a palette, and metadata into a ZIP file that can be
reopened in Spatial Studio or rendered with
[`render_dragmapr_project()`](https://prigasg.github.io/dragmapr/reference/render_dragmapr_project.md).
This lets you create and share reproducible project bundles entirely
from R, without opening the Shiny app.

## Usage

``` r
write_dragmapr_project(
  x,
  region_col,
  file = NULL,
  region_offsets = NULL,
  label_offsets = NULL,
  labels = NULL,
  region_palette = NULL,
  label_col = region_col,
  title = NULL,
  ...
)
```

## Arguments

- x:

  An `sf` object in a projected CRS — the source geometry for the
  project.

- region_col:

  Column in `x` defining draggable groups.

- file:

  Output path for the ZIP file. Should end in `.zip`. When `NULL`, a
  temporary file is created and its path returned invisibly.

- region_offsets:

  Optional data frame with `region`, `dx_m`, and `dy_m` columns, or a
  path to such a CSV. When `NULL`, all regions are placed at their
  original positions.

- label_offsets:

  Optional data frame with `label_id`, `region`, `dx_m`, and `dy_m`
  columns, or a path to such a CSV.

- labels:

  Optional label table as returned by
  [`make_region_labels()`](https://prigasg.github.io/dragmapr/reference/make_region_labels.md)
  or
  [`as_drag_labels()`](https://prigasg.github.io/dragmapr/reference/as_drag_labels.md).

- region_palette:

  Optional named character vector of fill colors (names are region
  values, values are hex strings).

- label_col:

  Column used for default label text. Defaults to `region_col`.

- title:

  Optional project title stored in metadata.

- ...:

  Additional named metadata fields written to `metadata.json`.

## Value

Invisibly returns `file` (the path to the written ZIP).

## See also

[`read_dragmapr_project()`](https://prigasg.github.io/dragmapr/reference/read_dragmapr_project.md)
to read a project back into R;
[`render_dragmapr_project()`](https://prigasg.github.io/dragmapr/reference/render_dragmapr_project.md)
to render a project bundle directly.

## Examples

``` r
if(interactive()){
poly <- sf::st_sf(
  region = c("A", "B"),
  geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0,1e5),c(1e5,1e5),c(1e5,2e5),c(0,2e5),c(0,1e5)))),
    sf::st_polygon(list(rbind(c(0,0),c(1e5,0),c(1e5,1e5),c(0,1e5),c(0,0)))),
    crs = 3857
  )
)
offsets <- data.frame(region = c("A", "B"), dx_m = c(50000, -50000), dy_m = 0)
zip_path <- write_dragmapr_project(
  poly,
  region_col     = "region",
  region_offsets = offsets,
  title          = "Demo project",
  file           = tempfile(fileext = ".zip")
)
file.exists(zip_path)
}
```
