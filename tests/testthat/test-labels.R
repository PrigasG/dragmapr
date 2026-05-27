test_that("make_labels derives one anchor per region", {
  x <- sf::st_sf(
    region = c("North", "South"),
    name = c("North label", "South label"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      sf::st_polygon(list(rbind(c(10, 10), c(14, 10), c(14, 14), c(10, 14), c(10, 10)))),
      crs = 3857
    )
  )

  labels <- make_labels(x, "region", "name")

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
