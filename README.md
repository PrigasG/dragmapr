# dragmapr

`dragmapr` makes manual layout edits reproducible. It writes a draggable browser
helper for projected `sf` geometry, exports region and label movements as small
CSV files, and applies those CSVs back to the original data for static plots.

The package started from an `explodemap` paper workflow, but it is now
self-contained. The bundled HHS example copies only the small reusable pieces
from that work: region membership, names/colors, and published display offsets.

## Install

```r
# install.packages("pak")
pak::pak("PrigasG/dragmapr")
```

For local development:

```r
devtools::load_all()
```

## Core Workflow

1. Start with a projected `sf` object and a grouping column.
2. Write a draggable browser helper.
3. Drag regions and labels until the layout works.
4. Download or copy region and label offset CSVs.
5. Render a static plot or image from the source geometry plus those CSVs.

```r
library(dragmapr)

drag_map_prototype(
  my_sf,
  region_col = "region",
  label_col = "region",
  file = "drag-helper.html"
)

render_dragged_map(
  my_sf,
  region_offsets = "drag_region_offsets.csv",
  region_col = "region",
  label_col = "region",
  label_offsets = "drag_label_offsets.csv",
  file = "dragged-map.png"
)
```

## Labels

Region offsets move grouped geometries and their default label anchors. Label
offsets are an additional fine-tuning layer for the marker/text only.

```r
labels <- make_labels(my_sf, region_col = "region", label_col = "name")
labels <- apply_label_offsets(labels, "drag_label_offsets.csv")
```

## Built-in Examples

`dragmapr` includes examples that avoid external downloads:

- `basic_draggable_map.R`: four synthetic map regions.
- `explodemap_hhs_labels.R`: bundled HHS-style layout with explodemap colors and offsets.
- `label_nudging.R`: independent text-marker nudges after region movement.
- `non_map_panels.R`: dashboard/diagram rectangles, useful for non-map geometry checks.
- `roundtrip_csv.R`: write offsets to CSV, read them back, and render.
- `smoke_examples.R`: runs all bundled examples in a temporary directory.

Run the full smoke suite:

```r
source(system.file("examples", "smoke_examples.R", package = "dragmapr"))
```

## Vignettes And Site

The package includes three vignettes:

- Getting started with dragmapr
- Labels and static output
- Example gallery

Build the pkgdown site locally with:

```r
pkgdown::build_site()
```

A GitHub Pages workflow is included at `.github/workflows/pkgdown.yaml`. Once
the repository has a GitHub remote and Pages is enabled for GitHub Actions, the
site will publish to <https://prigasg.github.io/dragmapr/>.

## Design Goals

- Rigid region movement: every feature in a group receives the same offset.
- Reproducible manual layout: dragging creates data, not hidden state.
- Stable metre offsets: browser resizing should not change exported offsets.
- Static output friendly: outputs are ordinary `ggplot2` plots and image files.
- Label-aware: labels can move with regions and be nudged independently.
- Small CSV surface: hand edits stay reviewable in Git.
