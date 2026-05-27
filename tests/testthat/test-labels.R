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

  out <- apply_label_offsets(labels, offsets)

  expect_equal(out$x, 13)
  expect_equal(out$y, 18)
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
