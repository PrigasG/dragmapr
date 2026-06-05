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

test_that("RStudio addin is registered", {
  addin_file <- system.file("rstudio", "addins.dcf", package = "dragmapr")
  expect_true(file.exists(addin_file))

  addins <- read.dcf(addin_file)
  expect_true("dragmapr_addin" %in% addins[, "Binding"])
  expect_true("true" %in% tolower(addins[, "Interactive"]))
})

test_that("RStudio addin exposes newer helper controls", {
  candidates <- c(
    file.path(getwd(), "R", "addin.R"),
    file.path(getwd(), "..", "R", "addin.R"),
    test_path("..", "..", "R", "addin.R")
  )
  addin_file <- candidates[file.exists(candidates)][1L]
  skip_if(is.na(addin_file), "addin source file is not available")
  addin_code <- paste(readLines(addin_file, warn = FALSE), collapse = "\n")

  expect_match(addin_code, 'textInput("legend_title", "Legend title"', fixed = TRUE)
  expect_match(addin_code, 'selectInput(\n        "visible_legend_values"', fixed = TRUE)
  expect_match(addin_code, 'selectInput(\n        "visible_labels"', fixed = TRUE)
  expect_match(addin_code, 'multiple = TRUE', fixed = TRUE)

  expect_match(addin_code, 'textInput("connector_color", "Connector color"', fixed = TRUE)
  expect_match(addin_code, '"connector_linetype", "Line style"', fixed = TRUE)
  expect_match(addin_code, '"connector_endpoint", "Arrow"', fixed = TRUE)
  expect_match(addin_code, "connector_color = valid_color_or_default", fixed = TRUE)
  expect_match(addin_code, "connector_linetype = input$connector_linetype", fixed = TRUE)
  expect_match(addin_code, "connectorEndpoint = input$connector_endpoint", fixed = TRUE)

  expect_match(addin_code, 'checkboxInput("show_origin_outlines", "Show origin outlines", value = FALSE)', fixed = TRUE)
  expect_match(addin_code, 'checkboxInput("show_movement_connectors", "Show movement connectors", value = FALSE)', fixed = TRUE)
  expect_match(addin_code, 'checkboxInput("show_drag_trail", "Show drag preview trail", value = FALSE)', fixed = TRUE)
  expect_match(addin_code, "show_origin_outlines = isTRUE(input$show_origin_outlines)", fixed = TRUE)
  expect_match(addin_code, "movementConnectorColor = valid_color_or_default", fixed = TRUE)
  expect_match(addin_code, "showDragTrail = isTRUE(input$show_drag_trail)", fixed = TRUE)

  expect_match(addin_code, 'dragmapr-set-label-values', fixed = TRUE)
  expect_match(addin_code, 'dragmapr-set-legend-values', fixed = TRUE)
  expect_match(addin_code, "legend_values = visible_region_values", fixed = TRUE)
  expect_match(addin_code, "label_values = visible_region_values", fixed = TRUE)
})
