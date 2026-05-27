# Write offset CSVs, read them back, and render from files.

library(dragmapr)

hhs <- example_hhs_layout()
utils::write.csv(hhs$region_offsets, "roundtrip_region_offsets.csv", row.names = FALSE)
utils::write.csv(hhs$label_offsets, "roundtrip_label_offsets.csv", row.names = FALSE)

render_dragged_map(
  hhs$states,
  region_offsets = "roundtrip_region_offsets.csv",
  region_col = "hhs_region",
  labels = hhs$labels,
  label_offsets = "roundtrip_label_offsets.csv",
  region_palette = hhs$region_colors,
  region_labels = hhs$region_names,
  title = "CSV Round Trip",
  file = "roundtrip_dragged_map.png"
)
