library(dragmapr)

make_square <- function(x0, y0, size = 100000) {
  sf::st_polygon(list(rbind(
    c(x0, y0),
    c(x0 + size, y0),
    c(x0 + size, y0 + size),
    c(x0, y0 + size),
    c(x0, y0)
  )))
}

regions <- sf::st_sf(
  region = c("North", "South", "East", "West"),
  label = c("North", "South", "East", "West"),
  geometry = sf::st_sfc(
    make_square(0, 140000),
    make_square(0, 0),
    make_square(140000, 70000),
    make_square(-140000, 70000),
    crs = 3857
  )
)

drag_map_prototype(regions, region_col = "region", label_col = "label",
                   file = "basic_drag_helper.html", open = interactive())

region_offsets <- data.frame(
  region = c("North", "South", "East", "West"),
  dx_m = c(0, 0, 60000, -60000),
  dy_m = c(50000, -50000, 0, 0)
)
label_offsets <- data.frame(
  label_id = c("North", "South", "East", "West"),
  region = c("North", "South", "East", "West"),
  dx_m = c(0, 0, 25000, -25000),
  dy_m = c(30000, -30000, 0, 0)
)

render_dragged_map(
  regions,
  region_offsets = region_offsets,
  region_col = "region",
  label_col = "label",
  label_offsets = label_offsets,
  title = "Synthetic Draggable Map",
  file = "basic_dragged_map.png"
)
