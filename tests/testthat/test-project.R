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
    jsonlite::toJSON(list(region_col = "region", label_col = "name", title = "Project map"), auto_unbox = TRUE),
    file.path(project_dir, "metadata.json")
  )

  plot <- render_dragmapr_project(project_dir, quiet = TRUE)

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$title, "Project map")
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
