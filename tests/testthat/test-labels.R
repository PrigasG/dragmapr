test_that("make_region_labels derives one anchor per region", {
  x <- sf::st_sf(
    region = c("North", "South"),
    name = c("North label", "South label"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      sf::st_polygon(list(rbind(c(10, 10), c(14, 10), c(14, 14), c(10, 14), c(10, 10)))),
      crs = 3857
    )
  )

  labels <- make_region_labels(x, "region", "name")

  expect_equal(labels$label_id, c("North", "South"))
  expect_equal(labels$label, c("North label", "South label"))
  expect_true(all(c("x", "y") %in% names(labels)))
})

test_that("apply_label_offsets nudges labels independently", {
  labels <- data.frame(
    label_id = "North",
    region = "North",
    label = "North label",
    x = 10,
    y = 20
  )
  offsets <- data.frame(label_id = "North", region = "North", dx_m = 3, dy_m = -2)

  expect_warning(
    out <- apply_label_offsets(labels, offsets),
    "deprecated"
  )

  expect_equal(out$x, 13)
  expect_equal(out$y, 18)
})

test_that("legacy label helpers warn before forwarding", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  file <- tempfile(fileext = ".csv")
  writeLines("label_id,region,dx_m,dy_m\nNorth,North,1,2", file)

  expect_warning(labels <- make_labels(x, "region"), "deprecated")
  expect_warning(state <- read_label_offsets(file), "deprecated")

  expect_equal(labels$label_id, "North")
  expect_equal(state$dx_m, 1)
})

test_that("as_drag_labels preserves extra columns", {
  labels <- data.frame(
    label_id = "note-1",
    region = "North",
    label = "North label",
    x = 10,
    y = 20,
    tooltip = "Extra detail"
  )

  out <- as_drag_labels(labels)

  expect_equal(out$tooltip, "Extra detail")
})

test_that("as_drag_labels normalizes connector options", {
  labels <- data.frame(
    label_id = "note-1",
    region = "North",
    label = "North label",
    x = 10,
    y = 20,
    connector = "yes",
    connector_type = "elbow",
    connector_mid_x = 12,
    connector_mid_y = 22
  )

  out <- as_drag_labels(labels)

  expect_true(out$connector)
  expect_equal(out$connector_type, "elbow")
  expect_equal(out$connector_mid_x, 12)
  expect_equal(out$connector_mid_y, 22)
})

test_that("as_drag_labels accepts custom connector starts and squiggles", {
  labels <- data.frame(
    label_id = "note-1",
    region = "North",
    label = "North label",
    x = 10,
    y = 20,
    connector = TRUE,
    connector_type = "squiggle",
    connector_start_x = 8,
    connector_start_y = 18
  )

  out <- as_drag_labels(labels)

  expect_equal(out$connector_type, "squiggle")
  expect_equal(out$connector_start_x, 8)
  expect_equal(out$connector_start_y, 18)
})

test_that("as_drag_annotations creates box labels", {
  labels <- data.frame(
    label_id = "note-1",
    region = "North",
    label = "Longer explanatory text",
    x = 10,
    y = 20
  )

  out <- as_drag_annotations(labels, width_px = 140, height_px = 70)

  expect_equal(out$label_type, "box")
  expect_equal(out$width_px, 140)
  expect_equal(out$height_px, 70)
})

test_that("as_drag_annotations fills empty normalized dimensions", {
  labels <- as_drag_labels(data.frame(
    label_id = "note-1",
    region = "North",
    label = "Longer explanatory text",
    x = 10,
    y = 20
  ))

  out <- as_drag_annotations(labels, width_px = 140, height_px = 70)

  expect_equal(out$label_type, "box")
  expect_equal(out$width_px, 140)
  expect_equal(out$height_px, 70)
})

test_that("annotation boxes need useful dimensions", {
  labels <- data.frame(
    label_id = "note-1",
    region = "North",
    label = "Longer explanatory text",
    x = 10,
    y = 20,
    label_type = "box",
    width_px = 0,
    height_px = 70
  )

  expect_error(as_drag_labels(labels), "positive `width_px`")
})

test_that("render_dragged_map returns a ggplot with labels", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )

  plot <- render_dragged_map(
    x,
    region_offsets = data.frame(region = "North", dx_m = 5, dy_m = 10),
    region_col = "region",
    label_offsets = data.frame(label_id = "North", region = "North", dx_m = 1, dy_m = -1)
  )

  expect_s3_class(plot, "ggplot")
})

test_that("render_dragged_map accepts dragmapr_state", {
  x <- sf::st_sf(
    region = c("A", "B"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      sf::st_polygon(list(rbind(c(3, 0), c(4, 0), c(4, 1), c(3, 1), c(3, 0)))),
      crs = 3857
    )
  )
  state <- dragmapr_state(
    region_offsets = data.frame(region = "B", dx_m = 10, dy_m = 0)
  )

  plot <- render_dragged_map(x, region_col = "region", state = state, labels = FALSE)

  expect_s3_class(plot, "ggplot")
})

test_that("render_dragged_map can omit labels", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )

  plot <- render_dragged_map(x, region_col = "region", labels = FALSE)

  expect_s3_class(plot, "ggplot")
  expect_equal(length(plot$layers), 1)
})

test_that("render_dragged_map shows origin outlines only for moved regions", {
  x <- sf::st_sf(
    region = c("North", "South"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      sf::st_polygon(list(rbind(c(5, 0), c(9, 0), c(9, 4), c(5, 4), c(5, 0)))),
      crs = 3857
    )
  )
  offsets <- data.frame(
    region = c("North", "South"),
    dx_m = c(5, 0),
    dy_m = c(0, 0)
  )

  hidden <- render_dragged_map(
    x,
    region_offsets = offsets,
    region_col = "region",
    labels = FALSE
  )
  shown <- render_dragged_map(
    x,
    region_offsets = offsets,
    region_col = "region",
    labels = FALSE,
    show_origin_outlines = TRUE
  )
  zero <- render_dragged_map(
    x,
    region_col = "region",
    labels = FALSE,
    show_origin_outlines = TRUE
  )

  expect_equal(length(hidden$layers), 1)
  expect_equal(length(shown$layers), 2)
  expect_equal(as.character(shown$layers[[1]]$data$region), "North")
  expect_equal(shown$layers[[1]]$aes_params$linetype, "dashed")
  expect_equal(length(zero$layers), 1)
})

test_that("render_dragged_map shows movement connectors only for moved regions", {
  x <- sf::st_sf(
    region = c("North", "South"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      sf::st_polygon(list(rbind(c(5, 0), c(9, 0), c(9, 4), c(5, 4), c(5, 0)))),
      crs = 3857
    )
  )
  offsets <- data.frame(region = c("North", "South"), dx_m = c(5, 0), dy_m = c(2, 0))

  plot <- render_dragged_map(
    x,
    region_offsets = offsets,
    region_col = "region",
    labels = FALSE,
    show_movement_connectors = TRUE,
    movement_connector_color = "#123456",
    movement_connector_opacity = 0.4,
    movement_connector_linewidth = 1.2,
    movement_connector_linetype = "dotted",
    movement_connector_endpoint = "open"
  )

  expect_equal(length(plot$layers), 2)
  expect_equal(as.character(plot$layers[[1]]$data$region), "North")
  expect_equal(plot$layers[[1]]$data$xend - plot$layers[[1]]$data$x, 5)
  expect_equal(plot$layers[[1]]$data$yend - plot$layers[[1]]$data$y, 2)
  expect_equal(plot$layers[[1]]$aes_params$colour, "#123456")
  expect_equal(plot$layers[[1]]$aes_params$alpha, 0.4)
  expect_equal(plot$layers[[1]]$aes_params$linewidth, 1.2)
  expect_equal(plot$layers[[1]]$aes_params$linetype, "dotted")
  expect_equal(plot$layers[[1]]$geom_params$arrow$type, 1L)
})

test_that("render_dragged_map supports legend position and label colors", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  labels <- data.frame(
    label_id = "north",
    region = "North",
    label = "North",
    x = 2,
    y = 2,
    label_color = "#1D4ED8"
  )

  plot <- render_dragged_map(
    x,
    region_col = "region",
    labels = labels,
    legend_position = "right"
  )

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$theme$legend.position, "right")
})

test_that("render_dragged_map supports legend title, background, and connector styling", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  labels <- as_drag_labels(data.frame(
    label_id = "north",
    region = "North",
    label = "North",
    x = 2,
    y = 2,
    connector = TRUE,
    connector_type = "straight"
  ))
  label_offsets <- data.frame(label_id = "north", region = "North", dx_m = 2, dy_m = 1)

  plot <- render_dragged_map(
    x,
    region_col = "region",
    labels = labels,
    label_offsets = label_offsets,
    show_legend = TRUE,
    legend_title = "Municipality",
    map_background = "light_grid",
    connector_color = "#AABBCC",
    connector_linetype = "dashed",
    connector_endpoint = "arrow"
  )

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$scales$scales[[1]]$name, "Municipality")
  expect_equal(plot$theme$panel.background$fill, "#f8fafc")
  expect_equal(plot$layers[[2]]$aes_params$linetype, "dashed")
  expect_equal(plot$layers[[2]]$aes_params$colour, "#AABBCC")
  expect_s3_class(plot$layers[[2]]$geom_params$arrow, "arrow")
  expect_error(
    render_dragged_map(x, region_col = "region", connector_linetype = "dashdot"),
    "should be one of"
  )

  dark_plot <- render_dragged_map(
    x,
    region_col = "region",
    show_legend = TRUE,
    title = "Dark map",
    map_background = "dark"
  )
  expect_equal(dark_plot$theme$plot.title$colour, "#f8fafc")
  expect_equal(dark_plot$theme$legend.title$colour, "#f8fafc")
  expect_equal(dark_plot$theme$legend.text$colour, "#f8fafc")
})

test_that("render_dragged_map can render annotation boxes", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  labels <- as_drag_annotations(data.frame(
    label_id = "note-1",
    region = "North",
    label = "A longer note for this region",
    x = 2,
    y = 2
  ))

  plot <- render_dragged_map(x, region_col = "region", labels = labels)

  expect_s3_class(plot, "ggplot")
  expect_equal(length(plot$layers), 2)
})

test_that("render_dragged_map can render connector lines", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  labels <- as_drag_annotations(data.frame(
    label_id = "note-1",
    region = "North",
    label = "A longer note for this region",
    x = 2,
    y = 2
  ), connector = TRUE, connector_type = "elbow")

  plot <- render_dragged_map(
    x,
    region_col = "region",
    labels = labels,
    label_offsets = data.frame(label_id = "note-1", region = "North", dx_m = 1, dy_m = 1)
  )

  expect_s3_class(plot, "ggplot")
  expect_equal(length(plot$layers), 4)
})

test_that("zero-length curve connectors are skipped", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  labels <- as_drag_annotations(data.frame(
    label_id = "note-1",
    region = "North",
    label = "A longer note for this region",
    x = 2,
    y = 2
  ), connector = TRUE, connector_type = "curve")

  plot <- render_dragged_map(x, region_col = "region", labels = labels)

  expect_s3_class(plot, "ggplot")
  expect_equal(length(plot$layers), 2)
})

test_that("straight connectors handle multiple labels", {
  x <- sf::st_sf(
    region = c("North", "South"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      sf::st_polygon(list(rbind(c(6, 0), c(10, 0), c(10, 4), c(6, 4), c(6, 0)))),
      crs = 3857
    )
  )
  labels <- as_drag_labels(data.frame(
    label_id = c("North", "South"),
    region = c("North", "South"),
    label = c("North", "South"),
    x = c(2, 8),
    y = c(2, 2),
    connector = TRUE,
    connector_type = "straight"
  ))

  plot <- render_dragged_map(
    x,
    region_col = "region",
    labels = labels,
    label_offsets = data.frame(
      label_id = c("North", "South"),
      region = c("North", "South"),
      dx_m = c(1, -1),
      dy_m = c(1, 1)
    )
  )

  expect_s3_class(plot, "ggplot")
  expect_equal(length(plot$layers), 4)
})

test_that("squiggle connectors render in static plots", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  labels <- as_drag_labels(data.frame(
    label_id = "North",
    region = "North",
    label = "North",
    x = 2,
    y = 2,
    connector = TRUE,
    connector_type = "squiggle"
  ))

  plot <- render_dragged_map(
    x,
    region_col = "region",
    labels = labels,
    label_offsets = data.frame(label_id = "North", region = "North", dx_m = 1, dy_m = 1)
  )

  expect_s3_class(plot, "ggplot")
  expect_equal(length(plot$layers), 4)
})

test_that("connector endpoints can be trimmed away from label centers", {
  anchors <- as_drag_labels(data.frame(
    label_id = "North",
    region = "North",
    label = "North",
    x = 0,
    y = 0,
    connector = TRUE,
    connector_type = "straight"
  ))
  labels <- anchors
  labels$x <- 10
  labels$y <- 0

  connectors <- make_connector_data(anchors, labels, end_gap = 2)

  expect_equal(connectors$straight$xend, 8)
  expect_equal(connectors$straight$yend, 0)
})

test_that("text labels can render without marker circles", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )

  plot <- render_dragged_map(x, region_col = "region", show_label_marker = FALSE)

  expect_s3_class(plot, "ggplot")
  expect_equal(length(plot$layers), 2)
})

test_that("text labels can render with rectangular markers", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )

  plot <- render_dragged_map(x, region_col = "region", label_marker_shape = "rect")

  label_layers <- vapply(plot$layers, function(layer) inherits(layer$geom, "GeomLabel"), logical(1))
  point_layers <- vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1))
  expect_true(any(label_layers))
  expect_false(any(point_layers))
})

test_that("invalid label type errors clearly", {
  labels <- data.frame(
    label_id = "note-1",
    region = "North",
    label = "A note",
    x = 2,
    y = 2,
    label_type = "banner"
  )

  expect_error(as_drag_labels(labels), "label_type")
})

test_that("connector breakpoints need both coordinates", {
  labels <- data.frame(
    label_id = "note-1",
    region = "North",
    label = "A note",
    x = 2,
    y = 2,
    connector = TRUE,
    connector_type = "curve",
    connector_mid_x = 3
  )

  expect_error(as_drag_labels(labels), "both `connector_mid_x` and `connector_mid_y`")
})

test_that("connector starts need both coordinates", {
  labels <- data.frame(
    label_id = "note-1",
    region = "North",
    label = "A note",
    x = 2,
    y = 2,
    connector = TRUE,
    connector_type = "straight",
    connector_start_x = 3
  )

  expect_error(as_drag_labels(labels), "both `connector_start_x` and `connector_start_y`")
})

test_that("make_region_labels returns groups in natural numeric order", {
  x <- sf::st_sf(
    region = c("10", "2", "1", "9"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0,0),c(1,0),c(1,1),c(0,1),c(0,0)))),
      sf::st_polygon(list(rbind(c(2,0),c(3,0),c(3,1),c(2,1),c(2,0)))),
      sf::st_polygon(list(rbind(c(4,0),c(5,0),c(5,1),c(4,1),c(4,0)))),
      sf::st_polygon(list(rbind(c(6,0),c(7,0),c(7,1),c(6,1),c(6,0)))),
      crs = 3857
    )
  )

  labels <- make_region_labels(x, "region")

  # Natural sort: "1", "2", "9", "10" not "1", "10", "2", "9"
  expect_equal(labels$label_id, c("1", "2", "9", "10"))
})

test_that("as_drag_annotations applies connector arg even when labels come from make_region_labels", {
  # Regression test: normalize_labels pre-fills connector = FALSE, which used
  # to cause as_drag_annotations to silently discard connector = TRUE.
  x <- sf::st_sf(
    region = c("A", "B"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0,0),c(1,0),c(1,1),c(0,1),c(0,0)))),
      sf::st_polygon(list(rbind(c(2,0),c(3,0),c(3,1),c(2,1),c(2,0)))),
      crs = 3857
    )
  )

  labels <- make_region_labels(x, "region")
  boxes  <- as_drag_annotations(labels,
                                 width_px = 120, height_px = 60,
                                 connector = TRUE,
                                 connector_type = "elbow")

  expect_true(all(boxes$connector))
  expect_true(all(boxes$connector_type == "elbow"))
})

test_that("example_hhs_layout is self-contained", {
  hhs <- example_hhs_layout()

  expect_s3_class(hhs$states, "sf")
  expect_equal(sort(names(hhs)), sort(c(
    "states", "labels", "region_offsets", "label_offsets",
    "region_names", "region_colors"
  )))
  expect_equal(sort(as.integer(unique(hhs$states$hhs_region))), 1:10)
  expect_equal(hhs$region_offsets$region, as.character(1:10))
  expect_equal(hhs$label_offsets$label_id, as.character(1:10))
})

test_that("example_panel_layout exercises non-map geometry", {
  panels <- example_panel_layout()

  expect_s3_class(panels$panels, "sf")
  expect_equal(sort(names(panels)), sort(c(
    "panels", "labels", "region_offsets", "label_offsets",
    "region_names", "region_colors"
  )))
  expect_equal(panels$region_offsets$region, LETTERS[1:5])

  plot <- render_dragged_map(
    panels$panels,
    region_offsets = panels$region_offsets,
    region_col = "group",
    labels = panels$labels,
    label_offsets = panels$label_offsets,
    region_palette = panels$region_colors,
    region_labels = panels$region_names,
    title = "Panel smoke test"
  )
  expect_s3_class(plot, "ggplot")
})

test_that("render_dragged_map filters legend keys and label IDs", {
  x <- sf::st_sf(
    region = c("North", "South"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0,0), c(4,0), c(4,4), c(0,4), c(0,0)))),
      sf::st_polygon(list(rbind(c(5,0), c(9,0), c(9,4), c(5,4), c(5,0)))),
      crs = 3857
    )
  )

  plot <- render_dragged_map(
    x,
    region_col = "region",
    legend_values = "South",
    label_values = "North"
  )

  expect_equal(plot$scales$scales[[1]]$breaks, "South")
  label_layers <- Filter(function(layer) "label_id" %in% names(layer$data), plot$layers)
  expect_true(length(label_layers) > 0L)
  expect_true(all(vapply(label_layers, function(layer) {
    all(as.character(layer$data$label_id) == "North")
  }, logical(1))))
})
