test_that("feature styles validate and round-trip through state JSON", {
  styles <- d_styles(data.frame(
    id = c("A", "B"),
    fill = c("#112233", NA),
    stroke = c("#FFFFFF", "#000000"),
    stroke_width = c(1.2, NA),
    opacity = c(0.8, 1),
    label_visible = c(TRUE, FALSE),
    highlight = c(FALSE, TRUE)
  ), id_col = "id")
  state <- d_state(styles = styles)
  path <- tempfile(fileext = ".json")

  write_dragmapr_state(state, path)
  restored <- read_dragmapr_state(path)

  expect_s3_class(styles, "dragmapr_styles")
  expect_equal(restored$schema_version, "1.2.0")
  expect_equal(as.data.frame(restored$styles), as.data.frame(styles))
  expect_true(d_state_diff(restored, d_state())$styles_changed)
})

test_that("feature styles reject invalid visual values", {
  expect_error(
    d_styles(data.frame(region = "A", fill = "not-a-colour")),
    "Invalid.*fill"
  )
  expect_error(
    d_styles(data.frame(region = "A", opacity = 2)),
    "between 0 and 1"
  )
})

test_that("static rendering consumes state styles", {
  x <- sf::st_sf(
    region = c("A", "B"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0)))),
      sf::st_polygon(list(rbind(c(2, 0), c(3, 0), c(3, 1), c(2, 0)))),
      crs = 3857
    )
  )
  state <- d_state(
    region_col = "region",
    styles = d_styles(data.frame(
      region = c("A", "B"), fill = c("#112233", "#445566"),
      stroke = c("#FFFFFF", "#000000"), opacity = c(0.5, 1),
      label_visible = c(TRUE, FALSE)
    ))
  )
  plot <- render_dragged_map(x, region_col = "region", state = state)
  built <- ggplot2::ggplot_build(plot)

  expect_true(all(c("#112233", "#445566") %in% built$data[[1]]$fill))
  label_layers <- Filter(function(layer) "label_id" %in% names(layer$data), plot$layers)
  expect_true(all(vapply(label_layers, function(layer) {
    all(as.character(layer$data$region) == "A")
  }, logical(1))))
})
