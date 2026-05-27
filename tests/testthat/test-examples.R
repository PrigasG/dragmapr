test_that("bundled example scripts exist", {
  example_dir <- system.file("examples", package = "dragmapr")
  expect_true(dir.exists(example_dir))
  expect_true(all(file.exists(file.path(example_dir, c(
    "basic_draggable_map.R",
    "explodemap_hhs_labels.R",
    "label_nudging.R",
    "non_map_panels.R",
    "roundtrip_csv.R",
    "shiny_custom_labels.R",
    "shiny_draggable_export.R",
    "shiny_draggable_plot.R",
    "shiny_spatial_studio.R",
    "shiny_static_export.R",
    "smoke_examples.R"
  )))))
})
