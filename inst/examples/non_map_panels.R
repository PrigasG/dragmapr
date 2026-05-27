# Use dragmapr on non-map geometry: diagram/dashboard panels.
#
# Primary step: open the draggable browser helper and drag panels/labels until
# the layout looks right, then copy or download the offset CSVs.
# open = interactive() opens the browser in an interactive R session and is a
# no-op when sourced non-interactively (e.g. from smoke_examples.R).

library(dragmapr)

panels <- example_panel_layout()

drag_map_prototype(
  panels$panels,
  region_col = "group",
  label_col = "panel",
  file = "panel_drag_helper.html",
  open = interactive()
)

render_dragged_map(
  panels$panels,
  region_offsets = panels$region_offsets,
  region_col = "group",
  labels = panels$labels,
  label_offsets = panels$label_offsets,
  region_palette = panels$region_colors,
  region_labels = panels$region_names,
  title = "Draggable Non-map Panels",
  file = "panel_layout.png"
)
