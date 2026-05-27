# Region movement plus independent label nudging.

library(dragmapr)

hhs <- example_hhs_layout()

label_offsets <- hhs$label_offsets
label_offsets$dx_m[label_offsets$label_id %in% c("2", "3")] <- c(30000, -45000)
label_offsets$dy_m[label_offsets$label_id %in% c("2", "3")] <- c(45000, -30000)

render_dragged_map(
  hhs$states,
  region_offsets = hhs$region_offsets,
  region_col = "hhs_region",
  labels = hhs$labels,
  label_offsets = label_offsets,
  region_palette = hhs$region_colors,
  region_labels = hhs$region_names,
  title = "HHS Regions with Label Nudges",
  file = "hhs_label_nudging.png"
)
