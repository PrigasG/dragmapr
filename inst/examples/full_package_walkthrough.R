# =============================================================================
# dragmapr — Full Package Walkthrough
# =============================================================================
# Exercises every major exported function, rendering option, and edge case.
# Each section is self-contained and annotated.
#
# Run from any working directory; every output file is written to a single
# temporary folder printed at the top. Nothing is written to the CWD.
# =============================================================================

library(dragmapr)

out <- tempfile("dragmapr_walkthrough_")
dir.create(out, recursive = TRUE)
message("=== dragmapr walkthrough — outputs: ",
        normalizePath(out, winslash = "/"), " ===\n")

png_out  <- function(name) file.path(out, paste0(name, ".png"))
html_out <- function(name) file.path(out, paste0(name, ".html"))
zip_out  <- function(name) file.path(out, paste0(name, ".zip"))
csv_out  <- function(name) file.path(out, paste0(name, ".csv"))

# ---- Shared geometry helpers -------------------------------------------------

make_square <- function(x0, y0, size = 100000) {
  sf::st_polygon(list(rbind(
    c(x0, y0), c(x0 + size, y0), c(x0 + size, y0 + size),
    c(x0, y0 + size), c(x0, y0)
  )))
}

make_geo_square <- function(lon, lat, size = 3) {
  sf::st_polygon(list(rbind(
    c(lon, lat), c(lon + size, lat), c(lon + size, lat + size),
    c(lon, lat + size), c(lon, lat)
  )))
}

make_grid_sf <- function(nrow = 3, ncol = 4, cell = 100000, gap = 20000) {
  step <- cell + gap
  k <- 0L
  rows <- lapply(seq_len(nrow), function(i) {
    lapply(seq_len(ncol), function(j) {
      k <<- k + 1L
      list(
        geom   = make_square((j - 1L) * step, (i - 1L) * step, size = cell),
        region = paste0("G", ceiling(k / 2L)),
        label  = paste0("Cell ", i, "-", j)
      )
    })
  })
  flat <- unlist(rows, recursive = FALSE)
  sf::st_sf(
    region   = vapply(flat, `[[`, character(1L), "region"),
    label    = vapply(flat, `[[`, character(1L), "label"),
    geometry = sf::st_sfc(lapply(flat, `[[`, "geom"), crs = 3857)
  )
}


# =============================================================================
# Section 1: prepare_dragmapr_sf — CRS handling
# =============================================================================

# 1a. Geographic (longlat) input is re-projected to EPSG:3857
geo_sf <- sf::st_sf(
  region = c("Northwest", "Central", "Southeast"),
  label  = c("NW", "C", "SE"),
  geometry = sf::st_sfc(
    make_geo_square(-120, 45),
    make_geo_square(-95,  38),
    make_geo_square(-80,  28),
    crs = 4326
  )
)
projected_geo <- suppressMessages(prepare_dragmapr_sf(geo_sf))
stopifnot(sf::st_crs(projected_geo)$epsg == 3857)
message("1a OK  prepare_dragmapr_sf: longlat → EPSG:3857")

# 1b. Already-projected input passes through unchanged
already <- prepare_dragmapr_sf(projected_geo)
stopifnot(sf::st_crs(already)$epsg == 3857)
message("1b OK  prepare_dragmapr_sf: projected passthrough")

# 1c. CRS-less input: message is issued and EPSG:3857 is assumed
no_crs_sf <- sf::st_sf(
  region = c("P", "Q"),
  geometry = sf::st_sfc(
    make_square(0,    0),
    make_square(2e5,  0)
  )
)
no_crs_projected <- suppressMessages(prepare_dragmapr_sf(no_crs_sf))
stopifnot(inherits(no_crs_projected, "sf"))
message("1c OK  prepare_dragmapr_sf: no-CRS input → assumed EPSG:3857")

# 1d. Custom target CRS (EPSG:32614 — UTM zone 14N)
geo_small <- sf::st_sf(
  region = c("A", "B"),
  geometry = sf::st_sfc(
    make_geo_square(-98, 29),
    make_geo_square(-94, 32),
    crs = 4326
  )
)
projected_utm <- suppressMessages(prepare_dragmapr_sf(geo_small, target_crs = 32614))
stopifnot(sf::st_crs(projected_utm)$epsg == 32614)
message("1d OK  prepare_dragmapr_sf: custom target_crs (UTM)")


# =============================================================================
# Section 2: Label derivation and construction
# =============================================================================

grid <- make_grid_sf(nrow = 2, ncol = 3)

# 2a. Default make_region_labels (point_on_surface)
labels_pos <- make_region_labels(grid, region_col = "region", label_col = "label")
stopifnot(is.data.frame(labels_pos), "label_id" %in% names(labels_pos))
message("2a OK  make_region_labels: point_on_surface, ",
        nrow(labels_pos), " labels")

# 2b. Centroid method
labels_cen <- make_region_labels(grid, region_col = "region", label_col = "label",
                                  point = "centroid")
stopifnot(nrow(labels_cen) == nrow(labels_pos))
# Centroid and point_on_surface positions should differ for non-convex groups
message("2b OK  make_region_labels: centroid")

# 2c. as_drag_labels — user-supplied coordinates and per-label connector config
custom_labs <- as_drag_labels(data.frame(
  label_id       = c("cust-1", "cust-2", "cust-3"),
  region         = c("G1", "G2", "G3"),
  label          = c("Alpha", "Beta", "Gamma"),
  x              = c( 50000, 350000, 650000),
  y              = c( 80000, 180000,  80000),
  connector      = c(TRUE, TRUE, FALSE),
  connector_type = c("elbow", "curve", "straight"),
  stringsAsFactors = FALSE
))
stopifnot(all(c("label_id", "x", "y", "connector", "connector_type") %in%
                names(custom_labs)))
message("2c OK  as_drag_labels: custom coords + per-label connector types")

# 2d. as_drag_annotations — info boxes with wrapped text
hhs <- example_hhs_layout()
box_labels <- as_drag_annotations(
  hhs$labels,
  width_px       = 160,
  height_px      = 58,
  connector      = TRUE,
  connector_type = "elbow"
)
box_labels$label <- paste0(
  as.character(hhs$region_names[as.character(box_labels$region)]),
  "\nRegion: ", box_labels$region
)
stopifnot(all(box_labels$label_type == "box"))
message("2d OK  as_drag_annotations: all label_type == 'box'")

# 2e. apply_label_state shifts x/y by offsets
shifted <- apply_label_state(hhs$labels, hhs$label_offsets)
stopifnot(nrow(shifted) == nrow(hhs$labels))
# At least some labels should have moved
diffs <- abs(shifted$x - hhs$labels$x) + abs(shifted$y - hhs$labels$y)
stopifnot(any(diffs > 0))
message("2e OK  apply_label_state: positions shifted")


# =============================================================================
# Section 3: Offset I/O round-trip
# =============================================================================

regions_in_grid <- unique(as.character(grid$region))
region_offsets <- data.frame(
  region = regions_in_grid,
  dx_m   = seq(0, by = 25000, length.out = length(regions_in_grid)),
  dy_m   = seq(0, by = -15000, length.out = length(regions_in_grid)),
  stringsAsFactors = FALSE
)
label_offsets <- data.frame(
  label_id = labels_pos$label_id,
  region   = labels_pos$region,
  dx_m     = rep(12000, nrow(labels_pos)),
  dy_m     = rep( 8000, nrow(labels_pos)),
  stringsAsFactors = FALSE
)

# Write CSVs
utils::write.csv(region_offsets, csv_out("region_offsets"), row.names = FALSE)
utils::write.csv(label_offsets,  csv_out("label_offsets"),  row.names = FALSE)

# Read back and verify
region_back <- read_offsets(csv_out("region_offsets"))
label_back  <- read_label_state(csv_out("label_offsets"))
stopifnot(identical(sort(region_back$region), sort(region_offsets$region)))
stopifnot(nrow(label_back) == nrow(label_offsets))
message("3a OK  CSV round-trip: region and label offsets")

# apply_offsets moves geometries
adjusted_grid <- apply_offsets(grid, region_offsets, region_col = "region")
stopifnot(inherits(adjusted_grid, "sf"), nrow(adjusted_grid) == nrow(grid))
coords_orig <- sf::st_coordinates(sf::st_centroid(
  suppressWarnings(sf::st_union(grid[grid$region == regions_in_grid[2], ]))))
coords_moved <- sf::st_coordinates(sf::st_centroid(
  suppressWarnings(sf::st_union(adjusted_grid[adjusted_grid$region == regions_in_grid[2], ]))))
stopifnot(abs(coords_moved[1, "X"] - coords_orig[1, "X"] -
                region_offsets$dx_m[region_offsets$region == regions_in_grid[2]]) < 1)
message("3b OK  apply_offsets: geometry shift verified numerically")

# Partial offsets — unmoved regions fall back to zero displacement
partial_offsets <- region_offsets[1:3, ]
adjusted_partial <- apply_offsets(grid, partial_offsets, region_col = "region")
stopifnot(nrow(adjusted_partial) == nrow(grid))
message("3c OK  apply_offsets: partial offsets (missing regions stay at origin)")


# =============================================================================
# Section 4: render_dragged_map — all four connector types
# =============================================================================

for (ctype in c("straight", "elbow", "curve", "squiggle")) {
  conn_labels <- hhs$labels
  conn_labels$connector      <- TRUE
  conn_labels$connector_type <- ctype
  render_dragged_map(
    hhs$states,
    region_offsets      = hhs$region_offsets,
    region_col          = "hhs_region",
    labels              = conn_labels,
    label_offsets       = hhs$label_offsets,
    region_palette      = hhs$region_colors,
    region_labels       = hhs$region_names,
    connector_color     = "#1d4ed8",
    connector_linewidth = 0.5,
    connector_linetype  = "solid",
    connector_endpoint  = if (ctype == "squiggle") "none" else "arrow",
    title = paste("Connector type:", ctype),
    file  = png_out(paste0("connector_", ctype)),
    width = 9, height = 6, dpi = 150
  )
  message("4  OK  connector type '", ctype, "'")
}


# =============================================================================
# Section 5: render_dragged_map — all connector line styles and endpoints
# =============================================================================

for (ltype in c("solid", "dashed", "dotted")) {
  render_dragged_map(
    hhs$states,
    region_offsets      = hhs$region_offsets,
    region_col          = "hhs_region",
    labels              = { l <- hhs$labels; l$connector <- TRUE; l },
    label_offsets       = hhs$label_offsets,
    region_palette      = hhs$region_colors,
    connector_linetype  = ltype,
    connector_linewidth = 0.6,
    title = paste("Connector linetype:", ltype),
    file  = png_out(paste0("linetype_", ltype)),
    width = 9, height = 6, dpi = 150
  )
  message("5  OK  connector linetype '", ltype, "'")
}


# =============================================================================
# Section 6: render_dragged_map — all map backgrounds
# =============================================================================

for (bg in c("white", "transparent", "light_grid", "dark")) {
  render_dragged_map(
    hhs$states,
    region_offsets = hhs$region_offsets,
    region_col     = "hhs_region",
    labels         = FALSE,
    region_palette = hhs$region_colors,
    show_legend    = TRUE,
    legend_title   = "HHS Region",
    map_background = bg,
    title = paste("Background:", bg),
    file  = png_out(paste0("background_", bg)),
    width = 7, height = 5, dpi = 150
  )
  message("6  OK  map_background '", bg, "'")
}


# =============================================================================
# Section 7: render_dragged_map — movement context
# =============================================================================

# 7a. Origin outlines only
render_dragged_map(
  hhs$states,
  region_offsets       = hhs$region_offsets,
  region_col           = "hhs_region",
  labels               = FALSE,
  region_palette       = hhs$region_colors,
  show_origin_outlines = TRUE,
  title = "Origin outlines only",
  file  = png_out("movement_origin_only"),
  width = 9, height = 6, dpi = 150
)
message("7a OK  show_origin_outlines = TRUE")

# 7b. Movement connectors — all three endpoint styles
for (ep in c("none", "open", "closed")) {
  render_dragged_map(
    hhs$states,
    region_offsets             = hhs$region_offsets,
    region_col                 = "hhs_region",
    labels                     = FALSE,
    region_palette             = hhs$region_colors,
    show_origin_outlines       = TRUE,
    show_movement_connectors   = TRUE,
    movement_connector_color   = "#dc2626",
    movement_connector_opacity = 0.85,
    movement_connector_linewidth = 0.7,
    movement_connector_linetype  = "dashed",
    movement_connector_endpoint  = ep,
    title = paste("Movement connector endpoint:", ep),
    file  = png_out(paste0("movement_endpoint_", ep)),
    width = 9, height = 6, dpi = 150
  )
  message("7b OK  movement_connector_endpoint '", ep, "'")
}

# 7c. Full movement context combined with labels
render_dragged_map(
  hhs$states,
  region_offsets             = hhs$region_offsets,
  region_col                 = "hhs_region",
  labels                     = hhs$labels,
  label_offsets              = hhs$label_offsets,
  region_palette             = hhs$region_colors,
  region_labels              = hhs$region_names,
  show_origin_outlines       = TRUE,
  show_movement_connectors   = TRUE,
  movement_connector_endpoint = "closed",
  title = "Full movement context with labels",
  file  = png_out("movement_full_context"),
  width = 9, height = 6, dpi = 150
)
message("7c OK  movement context + labels combined")


# =============================================================================
# Section 8: render_dragged_map — legend and label filtering
# =============================================================================

all_regions    <- unique(as.character(hhs$states$hhs_region))
subset_regions <- sort(all_regions)[1:5]
subset_ids     <- as.character(
  hhs$labels$label_id[as.character(hhs$labels$region) %in% subset_regions])

# 8a. legend_values subsets the legend without hiding any geometry
render_dragged_map(
  hhs$states,
  region_offsets = hhs$region_offsets,
  region_col     = "hhs_region",
  labels         = FALSE,
  region_palette = hhs$region_colors,
  legend_values  = subset_regions,
  show_legend    = TRUE,
  legend_title   = "Selected regions",
  title = "legend_values: 5 of 10 keys shown",
  file  = png_out("filter_legend_values"),
  width = 9, height = 6, dpi = 150
)
message("8a OK  legend_values subsetting")

# 8b. label_values hides labels outside the selected set
render_dragged_map(
  hhs$states,
  region_offsets = hhs$region_offsets,
  region_col     = "hhs_region",
  labels         = hhs$labels,
  label_offsets  = hhs$label_offsets,
  region_palette = hhs$region_colors,
  region_labels  = hhs$region_names,
  label_values   = subset_ids,
  title = "label_values: 5 of 10 labels shown",
  file  = png_out("filter_label_values"),
  width = 9, height = 6, dpi = 150
)
message("8b OK  label_values subsetting")

# 8c. Both filters together
render_dragged_map(
  hhs$states,
  region_offsets = hhs$region_offsets,
  region_col     = "hhs_region",
  labels         = hhs$labels,
  label_offsets  = hhs$label_offsets,
  region_palette = hhs$region_colors,
  region_labels  = hhs$region_names,
  legend_values  = subset_regions,
  label_values   = subset_ids,
  show_legend    = TRUE,
  title = "legend_values + label_values combined",
  file  = png_out("filter_combined"),
  width = 9, height = 6, dpi = 150
)
message("8c OK  legend_values + label_values combined")


# =============================================================================
# Section 9: render_dragged_map — label styles
# =============================================================================

# 9a. Circle markers
render_dragged_map(
  hhs$states,
  region_offsets     = hhs$region_offsets,
  region_col         = "hhs_region",
  labels             = hhs$labels,
  label_offsets      = hhs$label_offsets,
  region_palette     = hhs$region_colors,
  show_label_marker  = TRUE,
  label_marker_shape = "circle",
  marker_size        = 4,
  title = "Label style: circle marker",
  file  = png_out("label_circle"),
  width = 9, height = 6, dpi = 150
)
message("9a OK  label_marker_shape = 'circle'")

# 9b. Rect (box) markers
render_dragged_map(
  hhs$states,
  region_offsets     = hhs$region_offsets,
  region_col         = "hhs_region",
  labels             = hhs$labels,
  label_offsets      = hhs$label_offsets,
  region_palette     = hhs$region_colors,
  show_label_marker  = TRUE,
  label_marker_shape = "rect",
  title = "Label style: rect marker",
  file  = png_out("label_rect"),
  width = 9, height = 6, dpi = 150
)
message("9b OK  label_marker_shape = 'rect'")

# 9c. Text only
render_dragged_map(
  hhs$states,
  region_offsets     = hhs$region_offsets,
  region_col         = "hhs_region",
  labels             = hhs$labels,
  label_offsets      = hhs$label_offsets,
  region_palette     = hhs$region_colors,
  show_label_marker  = FALSE,
  title = "Label style: text only",
  file  = png_out("label_text_only"),
  width = 9, height = 6, dpi = 150
)
message("9c OK  show_label_marker = FALSE")

# 9d. Info box annotations
render_dragged_map(
  hhs$states,
  region_offsets      = hhs$region_offsets,
  region_col          = "hhs_region",
  labels              = box_labels,
  label_offsets       = hhs$label_offsets,
  region_palette      = hhs$region_colors,
  connector_color     = "#334155",
  connector_linewidth = 0.4,
  title = "Info box annotations",
  file  = png_out("label_info_boxes"),
  width = 9, height = 6, dpi = 150
)
message("9d OK  as_drag_annotations info boxes")

# 9e. Per-label colors via label_color column
colored_labels <- hhs$labels
colored_labels$label_color <- rep_len(
  c("#1d4ed8", "#166534", "#991b1b", "#7c3aed", "#b45309"),
  nrow(colored_labels)
)
render_dragged_map(
  hhs$states,
  region_offsets = hhs$region_offsets,
  region_col     = "hhs_region",
  labels         = colored_labels,
  label_offsets  = hhs$label_offsets,
  region_palette = hhs$region_colors,
  title = "Per-label colors via label_color column",
  file  = png_out("label_per_label_color"),
  width = 9, height = 6, dpi = 150
)
message("9e OK  per-label colors via label_color column")


# =============================================================================
# Section 10: drag_map_prototype — option coverage
# =============================================================================

# 10a. Full-featured prototype
drag_map_prototype(
  hhs$states,
  region_col               = "hhs_region",
  label_col                = "hhs_region",
  labels                   = hhs$labels,
  label_marker             = TRUE,
  label_marker_shape       = "circle",
  label_radius             = 16,
  label_text_size          = 12,
  connector_color          = "#1d4ed8",
  connector_linewidth      = 1.5,
  connector_linetype       = "solid",
  connector_endpoint       = "arrow",
  region_offsets           = hhs$region_offsets,
  label_offsets            = hhs$label_offsets,
  region_palette           = hhs$region_colors,
  show_legend              = TRUE,
  legend_title             = "HHS Region",
  legend_position          = "bottom",
  map_background           = "light_grid",
  show_origin_outlines     = TRUE,
  show_movement_connectors = TRUE,
  movement_connector_color   = "#dc2626",
  movement_connector_opacity = 0.7,
  movement_connector_linewidth = 1.5,
  movement_connector_endpoint  = "closed",
  show_drag_trail          = TRUE,
  side_panel               = TRUE,
  file                     = html_out("prototype_full"),
  open                     = FALSE
)
stopifnot(file.exists(html_out("prototype_full")))
message("10a OK prototype: full-featured")

# 10b. Rect label markers, dashed connectors
drag_map_prototype(
  hhs$states,
  region_col         = "hhs_region",
  labels             = TRUE,
  label_marker_shape = "rect",
  label_width        = 80,
  label_height       = 32,
  connector_linetype = "dashed",
  region_palette     = hhs$region_colors,
  file               = html_out("prototype_rect_dashed"),
  open               = FALSE
)
message("10b OK prototype: rect markers + dashed connectors")

# 10c. Annotation boxes, no side panel, dark background
drag_map_prototype(
  hhs$states,
  region_col     = "hhs_region",
  labels         = box_labels,
  label_marker   = FALSE,
  side_panel     = FALSE,
  map_background = "dark",
  file           = html_out("prototype_boxes_dark"),
  open           = FALSE
)
message("10c OK prototype: annotation boxes, dark, no side panel")

# 10d. Smart connector mode pre-seeded
drag_map_prototype(
  hhs$states,
  region_col      = "hhs_region",
  labels          = hhs$labels,
  region_offsets  = hhs$region_offsets,
  label_offsets   = hhs$label_offsets,
  region_palette  = hhs$region_colors,
  connector_smart = TRUE,
  show_legend     = TRUE,
  file            = html_out("prototype_smart_connector"),
  open            = FALSE
)
message("10d OK prototype: connector_smart = TRUE")

# 10e. Legend filtering pre-seeded into prototype
drag_map_prototype(
  hhs$states,
  region_col     = "hhs_region",
  labels         = hhs$labels,
  region_offsets = hhs$region_offsets,
  label_offsets  = hhs$label_offsets,
  region_palette = hhs$region_colors,
  legend_values  = subset_regions,
  label_values   = subset_ids,
  show_legend    = TRUE,
  file           = html_out("prototype_filtered"),
  open           = FALSE
)
message("10e OK prototype: pre-seeded legend + label filters")


# =============================================================================
# Section 11: dragmapr_iframe_bridge — JS output verification
# =============================================================================

bridge_js <- dragmapr_iframe_bridge(
  region_input    = "region_csv",
  label_input     = "label_csv",
  iframe_selector = "#my-helper",
  slow_poll_ms    = 3000L,
  fast_poll_ms    = 400L
)
stopifnot(is.character(bridge_js), nzchar(bridge_js))
stopifnot(grepl("_dragmaprBridgeStop",   bridge_js))
stopifnot(grepl("beforeunload",          bridge_js))
stopifnot(grepl("#my-helper",            bridge_js))
stopifnot(grepl("shiny:disconnected",    bridge_js))
stopifnot(grepl("shiny:connected",       bridge_js))
stopifnot(grepl("region_csv",            bridge_js))
stopifnot(grepl("label_csv",             bridge_js))
message("11  OK dragmapr_iframe_bridge: JS structure verified")


# =============================================================================
# Section 12: render_dragmapr_project — full cycle
# =============================================================================

bundle_dir <- tempfile("dragmapr_bundle_")
dir.create(bundle_dir, recursive = TRUE)

sf::st_write(hhs$states,
             file.path(bundle_dir, "source.gpkg"),
             driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
utils::write.csv(hhs$region_offsets,
                 file.path(bundle_dir, "drag_region_offsets.csv"),
                 row.names = FALSE)
utils::write.csv(hhs$label_offsets,
                 file.path(bundle_dir, "drag_label_offsets.csv"),
                 row.names = FALSE)
utils::write.csv(hhs$labels,
                 file.path(bundle_dir, "labels.csv"),
                 row.names = FALSE)
utils::write.csv(
  data.frame(region = names(hhs$region_colors),
             color  = unname(hhs$region_colors),
             stringsAsFactors = FALSE),
  file.path(bundle_dir, "palette.csv"),
  row.names = FALSE
)
writeLines(
  jsonlite::toJSON(list(
    region_col     = "hhs_region",
    label_col      = "hhs_region",
    title          = "Project bundle render",
    map_background = "light_grid",
    show_legend    = TRUE,
    legend_title   = "HHS Region",
    connector_smart         = FALSE,
    show_origin_outlines    = TRUE,
    show_movement_connectors = TRUE,
    movement_connector_endpoint = "closed"
  ), auto_unbox = TRUE, pretty = TRUE),
  file.path(bundle_dir, "metadata.json")
)

# 12a. Render from folder
render_dragmapr_project(
  bundle_dir,
  file   = png_out("project_from_folder"),
  width  = 9, height = 6, dpi = 150
)
stopifnot(file.exists(png_out("project_from_folder")))
message("12a OK render_dragmapr_project: from folder")

# 12b. Render from ZIP
zip_file <- zip_out("dragmapr_project")
old_wd <- setwd(bundle_dir)
utils::zip(zip_file, list.files(".", recursive = TRUE))
setwd(old_wd)

render_dragmapr_project(
  zip_file,
  file   = png_out("project_from_zip"),
  width  = 9, height = 6, dpi = 150
)
stopifnot(file.exists(png_out("project_from_zip")))
message("12b OK render_dragmapr_project: from ZIP")

# 12c. title override via argument takes priority over metadata
render_dragmapr_project(
  zip_file,
  title  = "Overridden title",
  file   = png_out("project_title_override"),
  width  = 9, height = 6, dpi = 150
)
message("12c OK render_dragmapr_project: title override")


# =============================================================================
# Section 13: Panel (non-map) layout
# =============================================================================

panels <- example_panel_layout()

drag_map_prototype(
  panels$panels,
  region_col     = "group",
  label_col      = "panel",
  labels         = panels$labels,
  region_palette = panels$region_colors,
  region_offsets = panels$region_offsets,
  label_offsets  = panels$label_offsets,
  show_legend    = TRUE,
  file           = html_out("panels_prototype"),
  open           = FALSE
)

render_dragged_map(
  panels$panels,
  region_offsets = panels$region_offsets,
  region_col     = "group",
  labels         = panels$labels,
  label_offsets  = panels$label_offsets,
  region_palette = panels$region_colors,
  region_labels  = panels$region_names,
  title          = "Panel layout — non-map geometry",
  file           = png_out("panels_render"),
  width = 8, height = 6, dpi = 150
)
message("13  OK panel (non-map) layout: prototype + render")


# =============================================================================
# Section 14: Edge cases
# =============================================================================

# 14a. Single region
single_sf <- sf::st_sf(
  region   = "Only",
  geometry = sf::st_sfc(make_square(0, 0), crs = 3857)
)
render_dragged_map(
  single_sf,
  region_col  = "region",
  labels      = FALSE,
  show_legend = TRUE,
  title       = "Single region",
  file        = png_out("edge_single_region"),
  width = 5, height = 4, dpi = 150
)
message("14a OK edge case: single region")

# 14b. Zero offsets — no movement at all
zero_offsets <- data.frame(
  region = unique(as.character(hhs$states$hhs_region)),
  dx_m   = 0, dy_m = 0,
  stringsAsFactors = FALSE
)
render_dragged_map(
  hhs$states,
  region_offsets = zero_offsets,
  region_col     = "hhs_region",
  labels         = FALSE,
  region_palette = hhs$region_colors,
  title          = "Zero offsets",
  file           = png_out("edge_zero_offsets"),
  width = 7, height = 5, dpi = 150
)
message("14b OK edge case: zero offsets")

# 14c. NULL region_offsets — function builds its own zero table
render_dragged_map(
  hhs$states,
  region_offsets = NULL,
  region_col     = "hhs_region",
  labels         = FALSE,
  region_palette = hhs$region_colors,
  title          = "NULL region_offsets",
  file           = png_out("edge_null_offsets"),
  width = 7, height = 5, dpi = 150
)
message("14c OK edge case: NULL region_offsets")

# 14d. High-cardinality regions (> 25) — legend auto-suppressed with message
n_many <- 30L
many_polys <- lapply(seq_len(n_many), function(i) {
  x0 <- ((i - 1L) %% 6L) * 120000
  y0 <- ((i - 1L) %/% 6L) * 120000
  make_square(x0, y0)
})
many_sf <- sf::st_sf(
  region   = paste0("R", sprintf("%02d", seq_len(n_many))),
  geometry = sf::st_sfc(many_polys, crs = 3857)
)
suppressMessages(render_dragged_map(
  many_sf,
  region_col  = "region",
  labels      = FALSE,
  show_legend = TRUE,
  title       = paste(n_many, "regions — legend auto-suppressed"),
  file        = png_out("edge_high_cardinality"),
  width = 8, height = 6, dpi = 150
))
message("14d OK edge case: ", n_many, "-region map, legend threshold")

# 14e. Disjoint polygons in the same group
disjoint_sf <- sf::st_sf(
  region = c("A", "A", "B", "B"),
  label  = c("A", "A", "B", "B"),
  geometry = sf::st_sfc(
    make_square(  0,   0),
    make_square(3e5,   0),
    make_square(  0, 3e5),
    make_square(3e5, 3e5),
    crs = 3857
  )
)
render_dragged_map(
  disjoint_sf,
  region_col = "region",
  label_col  = "label",
  title      = "Disjoint polygons per group",
  file       = png_out("edge_disjoint"),
  width = 5, height = 5, dpi = 150
)
message("14e OK edge case: disjoint polygons per region group")

# 14f. labels = FALSE explicitly
render_dragged_map(
  hhs$states,
  region_offsets = hhs$region_offsets,
  region_col     = "hhs_region",
  labels         = FALSE,
  region_palette = hhs$region_colors,
  title          = "labels = FALSE",
  file           = png_out("edge_no_labels"),
  width = 7, height = 5, dpi = 150
)
message("14f OK edge case: labels = FALSE")

# 14g. Empty label_offsets — labels render at anchor positions
empty_label_off <- data.frame(
  label_id = character(), region = character(),
  dx_m = numeric(), dy_m = numeric(),
  stringsAsFactors = FALSE
)
render_dragged_map(
  hhs$states,
  region_offsets = hhs$region_offsets,
  region_col     = "hhs_region",
  labels         = hhs$labels,
  label_offsets  = empty_label_off,
  region_palette = hhs$region_colors,
  title          = "Empty label_offsets (anchor positions)",
  file           = png_out("edge_empty_label_offsets"),
  width = 9, height = 6, dpi = 150
)
message("14g OK edge case: empty label_offsets")


# =============================================================================
# Section 15: Deprecated aliases — must still work and warn
# =============================================================================

# 15a. make_labels() → make_region_labels()
old_labels <- withCallingHandlers(
  make_labels(hhs$states, region_col = "hhs_region"),
  warning = function(w) {
    stopifnot(grepl("Deprecated|deprecated|make_region_labels", conditionMessage(w)))
    invokeRestart("muffleWarning")
  }
)
stopifnot(nrow(old_labels) == nrow(hhs$labels))
message("15a OK deprecated alias: make_labels() warns + delegates")

# 15b. read_label_offsets() → read_label_state()
utils::write.csv(hhs$label_offsets, csv_out("legacy_label"), row.names = FALSE)
legacy_read <- withCallingHandlers(
  read_label_offsets(csv_out("legacy_label")),
  warning = function(w) {
    stopifnot(grepl("Deprecated|deprecated|read_label_state", conditionMessage(w)))
    invokeRestart("muffleWarning")
  }
)
stopifnot(nrow(legacy_read) == nrow(hhs$label_offsets))
message("15b OK deprecated alias: read_label_offsets() warns + delegates")

# 15c. apply_label_offsets() → apply_label_state()
shifted_legacy <- withCallingHandlers(
  apply_label_offsets(hhs$labels, hhs$label_offsets),
  warning = function(w) {
    stopifnot(grepl("Deprecated|deprecated|apply_label_state", conditionMessage(w)))
    invokeRestart("muffleWarning")
  }
)
stopifnot(nrow(shifted_legacy) == nrow(hhs$labels))
message("15c OK deprecated alias: apply_label_offsets() warns + delegates")


# =============================================================================
# Summary
# =============================================================================

outputs <- list.files(out, recursive = TRUE)
message(
  "\n=== All checks passed. ",
  length(outputs), " output files written to:\n",
  normalizePath(out, winslash = "/"), " ===\n"
)
for (f in sort(outputs)) message("  ", f)
