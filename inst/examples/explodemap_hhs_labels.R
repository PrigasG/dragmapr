# Render a bundled explodemap-style HHS dragged layout with dragmapr labels.
#
# This example is self-contained: the HHS membership, region names/colors, and
# published display offsets from the explodemap paper workflow are bundled in
# dragmapr. The geometry is a lightweight projected fixture rather than the
# large generated paper GeoJSON.
#
# Primary step: open the draggable helper and move regions/labels until the
# layout looks right, then copy or download the offset CSVs.
# open = interactive() opens the browser in an interactive R session and is a
# no-op when sourced non-interactively (e.g. from smoke_examples.R).

library(dragmapr)

hhs <- example_hhs_layout()

drag_map_prototype(
  hhs$states,
  region_col = "hhs_region",
  label_col = "hhs_region",
  file = "hhs_drag_helper.html",
  open = interactive()
)

render_dragged_map(
  hhs$states,
  region_offsets = hhs$region_offsets,
  region_col = "hhs_region",
  labels = hhs$labels,
  label_offsets = hhs$label_offsets,
  region_palette = hhs$region_colors,
  region_labels = hhs$region_names,
  title = "US Map by HHS Regions",
  file = "hhs_dragmapr_labeled.png",
  width = 9,
  height = 6
)
