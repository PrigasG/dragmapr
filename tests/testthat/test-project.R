test_that("render_dragmapr_project renders an extracted Spatial Studio bundle", {
  project_dir <- tempfile("dragmapr_project_")
  dir.create(project_dir)
  x <- sf::st_sf(
    region = c("North", "South"),
    name = c("North label", "South label"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      sf::st_polygon(list(rbind(c(6, 0), c(10, 0), c(10, 4), c(6, 4), c(6, 0)))),
      crs = 3857
    )
  )
  sf::st_write(x, file.path(project_dir, "source.gpkg"), driver = "GPKG", quiet = TRUE)
  utils::write.csv(
    data.frame(region = c("North", "South"), dx_m = c(1, 0), dy_m = c(0, 0)),
    file.path(project_dir, "drag_region_offsets.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(label_id = c("North", "South"), region = c("North", "South"), dx_m = c(0, 0), dy_m = c(0, 0)),
    file.path(project_dir, "drag_label_offsets.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    make_region_labels(x, "region", "name"),
    file.path(project_dir, "labels.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(region = c("North", "South"), color = c("#123456", "#ABCDEF")),
    file.path(project_dir, "palette.csv"),
    row.names = FALSE
  )
  writeLines(
    jsonlite::toJSON(
      list(
        region_col = "region",
        label_col = "name",
        title = "Project map",
        legend_values = "South",
        label_values = "North",
        show_origin_outlines = TRUE,
        show_movement_connectors = TRUE,
        connector_color = "#AABBCC",
        movement_connector_color = "#123456",
        movement_connector_opacity = 0.4,
        movement_connector_linewidth = 1.2,
        movement_connector_linetype = "dotted",
        movement_connector_endpoint = "open"
      ),
      auto_unbox = TRUE
    ),
    file.path(project_dir, "metadata.json")
  )

  plot <- render_dragmapr_project(project_dir, quiet = TRUE)

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$title, "Project map")
  expect_equal(as.character(plot$layers[[1]]$data$region), "North")
  expect_equal(plot$layers[[1]]$aes_params$linetype, "dashed")
  expect_equal(as.character(plot$layers[[2]]$data$region), "North")
  expect_equal(plot$layers[[2]]$aes_params$colour, "#123456")
  expect_equal(plot$layers[[2]]$aes_params$linetype, "dotted")
  expect_equal(plot$scales$scales[[1]]$breaks, "South")
  label_layers <- Filter(function(layer) "label_id" %in% names(layer$data), plot$layers)
  expect_true(all(vapply(label_layers, function(layer) {
    all(as.character(layer$data$label_id) == "North")
  }, logical(1))))
})

test_that("render_dragmapr_project gives a useful error for missing region columns", {
  project_dir <- tempfile("dragmapr_project_")
  dir.create(project_dir)
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  sf::st_write(x, file.path(project_dir, "source.gpkg"), driver = "GPKG", quiet = TRUE)
  writeLines(
    jsonlite::toJSON(list(region_col = "wrong_column"), auto_unbox = TRUE),
    file.path(project_dir, "metadata.json")
  )

  expect_error(
    render_dragmapr_project(project_dir, quiet = TRUE),
    "wrong_column.*not present in source.gpkg"
  )
})

test_that("project bundles persist state hierarchy and feature styles", {
  x <- sf::st_sf(
    region = c("A", "B"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0)))),
      sf::st_polygon(list(rbind(c(2, 0), c(3, 0), c(3, 1), c(2, 0)))),
      crs = 3857
    )
  )
  styles <- d_styles(data.frame(
    region = c("A", "B"), fill = c("#112233", "#445566"),
    label_visible = c(TRUE, FALSE)
  ))
  state <- d_state(
    region_col = "region",
    region_offsets = data.frame(region = "A", dx_m = 10, dy_m = 0),
    styles = styles
  )
  hierarchy <- d_hierarchy_state(
    root = state,
    children = list("region:A" = d_state(level = "child")),
    active_path = "region:A"
  )
  path <- tempfile(fileext = ".zip")

  write_dragmapr_project(
    x, region_col = "region", file = path,
    state = state, hierarchy = hierarchy
  )
  bundle <- read_dragmapr_project(path)

  expect_s3_class(bundle$state, "dragmapr_state")
  expect_s3_class(bundle$hierarchy, "dragmapr_hierarchy_state")
  expect_equal(as.data.frame(bundle$styles), as.data.frame(styles))
  expect_equal(bundle$hierarchy$active_path, "region:A")
  expect_equal(bundle$state$region_offsets$dx_m, 10)
})
