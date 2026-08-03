test_that("prepare_dragmapr_sf transforms longlat polygon data", {
  x <- sf::st_sf(
    region = "A",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(-1, 0), c(1, 0), c(1, 1), c(-1, 1), c(-1, 0)))),
      crs = 4326
    )
  )

  out <- prepare_dragmapr_sf(x)

  expect_s3_class(out, "sf")
  expect_false(sf::st_is_longlat(out))
})

test_that("prepare_dragmapr_sf drops non-polygon features", {
  x <- sf::st_sf(
    region = c("A", "B"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      sf::st_point(c(2, 2)),
      crs = 3857
    )
  )

  out <- suppressMessages(prepare_dragmapr_sf(x))

  expect_equal(nrow(out), 1)
  expect_equal(out$region, "A")
})

test_that("prepare_dragmapr_sf warns before assuming missing CRS", {
  x <- sf::st_sf(
    region = "A",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
    )
  )

  expect_warning(
    out <- prepare_dragmapr_sf(x),
    "no CRS found"
  )
  expect_equal(sf::st_crs(out)$epsg, 3857)
})

test_that("read_dragmapr_sf_upload returns NULL for no upload", {
  expect_null(read_dragmapr_sf_upload(NULL))
  expect_null(read_dragmapr_sf_upload(data.frame()))
})

test_that("read_dragmapr_sf_upload validates Shiny upload shape", {
  upload <- data.frame(name = "regions.geojson")

  expect_error(
    read_dragmapr_sf_upload(upload),
    "Shiny fileInput"
  )
})

test_that("spatial file detection errors guide users", {
  dir <- tempfile("dragmapr_empty_upload_")
  dir.create(dir)
  writeLines("not spatial", file.path(dir, "notes.txt"))

  expect_error(
    dragmapr:::.detect_spatial_file(dir),
    "No supported spatial file found"
  )
})

test_that("invalid spatial files produce actionable read errors", {
  file <- tempfile(fileext = ".geojson")
  writeLines("not geojson", file)

  expect_error(
    dragmapr:::.read_spatial_file(file, source = "test file"),
    "Could not read the test file as spatial data"
  )
})

test_that("dragmapr_iframe_bridge names configured Shiny inputs", {
  js <- dragmapr_iframe_bridge(region_input = "regions", label_input = "labels")

  expect_match(js, '"regions"', fixed = TRUE)
  expect_match(js, '"labels"', fixed = TRUE)
  expect_match(js, "dragmapr-request-state", fixed = TRUE)
  expect_match(js, "_dragmaprOriginAllowed", fixed = TRUE)
  expect_match(js, "_dragmaprBridgeStop", fixed = TRUE)
  expect_match(js, "beforeunload", fixed = TRUE)
})

test_that("dragmapr_iframe_bridge respects custom iframe_selector", {
  js_default  <- dragmapr_iframe_bridge()
  js_specific <- dragmapr_iframe_bridge(iframe_selector = "iframe.my-map-frame")
  js_id       <- dragmapr_iframe_bridge(iframe_selector = "#my-helper")

  # Default should use plain "iframe" selector
  expect_match(js_default, '"iframe"', fixed = TRUE)

  # Custom selectors appear in the generated JS
  expect_match(js_specific, '"iframe.my-map-frame"', fixed = TRUE)
  expect_match(js_id,       '"#my-helper"',          fixed = TRUE)

  # Selector is wired into the poll function
  expect_match(js_specific, "_dragmaprIframeSelector", fixed = TRUE)

  # Empty selector is rejected
  expect_error(dragmapr_iframe_bridge(iframe_selector = ""), "non-empty CSS selector")
})

test_that("upload file names are sanitized", {
  expect_equal(
    dragmapr:::sanitize_upload_names(c("../regions.geojson", "C:\\fake\\map.shp")),
    c("regions.geojson", "map.shp")
  )
  expect_error(dragmapr:::sanitize_upload_names(".."), "non-empty base names")
})

test_that("zip archives reject unsafe paths", {
  skip_if_not(nzchar(Sys.which("zip")), "zip command not available")
  parent <- tempfile("dragmapr_zip_parent_")
  child <- file.path(parent, "child")
  dir.create(child, recursive = TRUE)
  writeLines("not spatial", file.path(parent, "outside.geojson"))
  old <- setwd(child)
  on.exit(setwd(old), add = TRUE)
  system2("zip", c("-q", "unsafe.zip", "../outside.geojson"))

  expect_error(
    dragmapr:::.unzip_spatial_archive("unsafe.zip", tempfile("dragmapr_unzip_"), "test zip"),
    "unsafe paths"
  )
})

test_that("spatial studio falls back from stale demo columns", {
  env <- new.env(parent = globalenv())
  suppressWarnings(
    source(system.file("examples", "shiny_spatial_studio.R", package = "dragmapr"), local = env)
  )

  expect_equal(env$choose_column("hhs_region", c("county", "name")), "county")
  expect_equal(env$choose_column("name", c("county", "name")), "name")
  expect_equal(env$choose_column(NULL, c("county", "name"), fallback = "name"), "name")
})

test_that("spatial studio creates valid empty label offsets", {
  env <- new.env(parent = globalenv())
  suppressWarnings(
    source(system.file("examples", "shiny_spatial_studio.R", package = "dragmapr"), local = env)
  )

  labels <- data.frame(
    label_id = character(),
    region = character(),
    label = character(),
    stringsAsFactors = FALSE
  )

  out <- env$empty_label_offsets(labels)

  expect_equal(nrow(out), 0)
  expect_named(out, c("label_id", "region", "dx_m", "dy_m"))
})

test_that("spatial studio smart connectors do not depend on package internals", {
  env <- new.env(parent = globalenv())
  suppressWarnings(
    source(system.file("examples", "shiny_spatial_studio.R", package = "dragmapr"), local = env)
  )
  labels <- data.frame(
    label_id = c("a", "b"),
    region = c("A", "B"),
    connector_type = c("straight", "straight"),
    stringsAsFactors = FALSE
  )
  offsets <- data.frame(
    label_id = c("a", "b"),
    region = c("A", "B"),
    dx_m = c(1000, 50000),
    dy_m = c(1000, 1000),
    stringsAsFactors = FALSE
  )

  out <- env$apply_smart_connector_types(labels, offsets)

  expect_equal(out$connector_type, c("straight", "elbow"))
})

test_that("spatial studio finishes ingest when an upload cannot build a helper", {
  env <- new.env(parent = globalenv())
  suppressWarnings(
    source(system.file("examples", "shiny_spatial_studio.R", package = "dragmapr"), local = env)
  )
  x <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      crs = 4326
    )
  )
  path <- tempfile(fileext = ".geojson")
  sf::st_write(x, path, driver = "GeoJSON", quiet = TRUE)
  upload <- data.frame(
    name = basename(path),
    size = file.info(path)$size,
    type = "application/geo+json",
    datapath = path,
    stringsAsFactors = FALSE
  )

  shiny::testServer(env$server, {
    session$setInputs(spatial_upload = upload)
    session$flushReact()

    expect_equal(state$source_version, 1L)
    expect_false(state$helper_loading)
    expect_false(state$helper_building)
    expect_match(state$status, "no usable attribute columns", fixed = TRUE)
  })
})

test_that("spatial studio keeps loading while a valid upload helper is rebuilding", {
  env <- new.env(parent = globalenv())
  suppressWarnings(
    source(system.file("examples", "shiny_spatial_studio.R", package = "dragmapr"), local = env)
  )
  x <- sf::st_sf(
    group = "A",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      crs = 4326
    )
  )
  path <- tempfile(fileext = ".geojson")
  sf::st_write(x, path, driver = "GeoJSON", quiet = TRUE)
  upload <- data.frame(
    name = basename(path),
    size = file.info(path)$size,
    type = "application/geo+json",
    datapath = path,
    stringsAsFactors = FALSE
  )

  shiny::testServer(env$server, {
    session$setInputs(spatial_upload = upload)
    session$flushReact()

    expect_equal(state$source_version, 1L)
    expect_true(state$helper_loading)
    expect_equal(state$helper_loading_generation, state$helper_token)
  })
})

test_that("spatial studio uses natural stable display factors", {
  env <- new.env(parent = globalenv())
  suppressWarnings(
    source(system.file("examples", "shiny_spatial_studio.R", package = "dragmapr"), local = env)
  )

  out <- env$factor_for_display(c("B", "A", "B", "10", "2", "1", "101", "100"))

  expect_true(is.ordered(out))
  expect_equal(levels(out), c("1", "2", "10", "100", "101", "A", "B"))
  expect_equal(env$stable_unique(c("Region 10", "Region 2", "Region 1")), c("Region 1", "Region 2", "Region 10"))
})

test_that("spatial studio edits one selected label color at a time", {
  studio_file <- system.file("examples", "shiny_spatial_studio.R", package = "dragmapr")
  studio_code <- paste(readLines(studio_file, warn = FALSE), collapse = "\n")

  expect_match(studio_code, "label_color_group")
  expect_match(studio_code, "label_color_value")
  expect_false(grepl("label_cycle_color_", studio_code, fixed = TRUE))
  expect_false(grepl("label_color_input_id", studio_code, fixed = TRUE))
})

test_that("spatial studio has a fallback for helper readiness", {
  studio_file <- system.file("examples", "shiny_spatial_studio.R", package = "dragmapr")
  studio_code <- paste(readLines(studio_file, warn = FALSE), collapse = "\n")

  expect_match(studio_code, "function markHelperReady(generation)", fixed = TRUE)
  expect_match(studio_code, "function scheduleHelperFallback(generation)", fixed = TRUE)
  expect_match(studio_code, "scheduleHelperFallback(generation);", fixed = TRUE)
  expect_match(studio_code, "scheduleHelperFallback(currentHelperGeneration());", fixed = TRUE)
  expect_match(studio_code, "event.target.matches('iframe.studio-helper-frame')", fixed = TRUE)
  expect_match(studio_code, "generation !== helperState.activeGeneration", fixed = TRUE)
  expect_match(studio_code, "}, 1500);", fixed = TRUE)
  expect_match(studio_code, "studioBusy.safetyTimer", fixed = TRUE)
  expect_match(studio_code, "}, 120000);", fixed = TRUE)
  expect_match(studio_code, "session$onFlushed(function()", fixed = TRUE)
  expect_match(
    studio_code,
    "isolate(isTRUE(state$helper_loading) || isTRUE(state$helper_building))",
    fixed = TRUE
  )
  expect_match(studio_code, "req(state$helper_token > 0L, file.exists(helper_file))", fixed = TRUE)
})

test_that("connect cloud wrapper returns a shiny app object", {
  wrapper <- file.path(
    testthat::test_path("..", ".."),
    "connect-cloud",
    "spatial-studio",
    "app.R"
  )
  skip_if_not(file.exists(wrapper))

  obj <- suppressWarnings(source(wrapper, local = new.env(parent = globalenv()))$value)

  expect_s3_class(obj, "shiny.appobj")
})

test_that("spatial studio applies text edits explicitly and locks controls during rebuilds", {
  studio_file <- system.file("examples", "shiny_spatial_studio.R", package = "dragmapr")
  studio_code <- paste(readLines(studio_file, warn = FALSE), collapse = "\n")

  expect_match(studio_code, 'actionButton("apply_label_text"', fixed = TRUE)
  expect_match(studio_code, 'observeEvent(input$apply_label_text', fixed = TRUE)
  expect_false(grepl('observeEvent(input$edit_label_text', studio_code, fixed = TRUE))
  expect_match(studio_code, 'actionButton("apply_static_title"', fixed = TRUE)
  expect_match(studio_code, 'observeEvent(input$apply_static_title', fixed = TRUE)
  expect_match(studio_code, "studio-busy-processing", fixed = TRUE)
  expect_match(
    studio_code,
    "list(state$source_version, projected_sf(), region_col(), label_col(),",
    fixed = TRUE
  )
  expect_match(studio_code, "Shiny.addCustomMessageHandler('dragmapr-side-panel'", fixed = TRUE)
  expect_match(studio_code, '"dragmapr-side-panel"', fixed = TRUE)
  expect_false(grepl(
    "list(state$source_version, projected_sf(), region_col(), label_col(), input$show_helper_panel)",
    studio_code,
    fixed = TRUE
  ))
})

test_that("spatial studio places the offset panel toggle beside Drag tabs", {
  studio_file <- system.file("examples", "shiny_spatial_studio.R", package = "dragmapr")
  studio_code <- paste(readLines(studio_file, warn = FALSE), collapse = "\n")

  expect_match(studio_code, 'class = "workspace-tabs"', fixed = TRUE)
  expect_match(studio_code, 'id = "workspace_tab"', fixed = TRUE)
  expect_match(studio_code, 'condition = "input.workspace_tab === \'Drag\'"', fixed = TRUE)
  expect_match(studio_code, 'class = "workspace-offset-toggle"', fixed = TRUE)
  expect_match(studio_code, 'checkboxInput("show_helper_panel", "Offset panel", value = TRUE)', fixed = TRUE)
  expect_false(grepl(
    'checkboxInput("show_helper_panel", "Show offset panel in drag view", value = TRUE)',
    studio_code,
    fixed = TRUE
  ))
})

test_that("spatial studio can reset label positions without moving regions", {
  studio_file <- system.file("examples", "shiny_spatial_studio.R", package = "dragmapr")
  studio_code <- paste(readLines(studio_file, warn = FALSE), collapse = "\n")

  expect_match(
    studio_code,
    '"reset_label_positions", "Reset label positions", "btn-sm btn-default"',
    fixed = TRUE
  )
  expect_match(studio_code, "observeEvent(input$reset_label_positions", fixed = TRUE)
  expect_match(studio_code, "region_offsets <- region_state()", fixed = TRUE)
  expect_match(studio_code, "zero_labels <- empty_label_offsets(all_label_table())", fixed = TRUE)
  expect_match(studio_code, 'set_status("Reset label positions without moving regions.", "ok")', fixed = TRUE)
})

test_that("spatial studio demonstrates movement context controls", {
  studio_file <- system.file("examples", "shiny_spatial_studio.R", package = "dragmapr")
  studio_code <- paste(readLines(studio_file, warn = FALSE), collapse = "\n")

  expect_match(
    studio_code,
    'checkboxInput("show_origin_outlines", "Show origin outlines", value = FALSE)',
    fixed = TRUE
  )
  expect_match(studio_code, "show_origin_outlines = isTRUE(input$show_origin_outlines)", fixed = TRUE)
  expect_match(studio_code, "showOriginOutlines = isTRUE(input$show_origin_outlines)", fixed = TRUE)
  expect_match(studio_code, "metadata$show_origin_outlines %||% FALSE", fixed = TRUE)
  expect_match(
    studio_code,
    'checkboxInput("show_movement_connectors", "Show movement connectors", value = FALSE)',
    fixed = TRUE
  )
  expect_match(
    studio_code,
    'checkboxInput("show_drag_trail", "Show drag preview trail", value = FALSE)',
    fixed = TRUE
  )
  expect_match(studio_code, "showMovementConnectors = isTRUE(input$show_movement_connectors)", fixed = TRUE)
  expect_match(studio_code, "showDragTrail = isTRUE(input$show_drag_trail)", fixed = TRUE)
  expect_match(studio_code, "metadata$show_movement_connectors %||% FALSE", fixed = TRUE)
  expect_match(studio_code, 'studio_color_input("connector_color", "Line color", value = "#334155")', fixed = TRUE)
  expect_match(studio_code, 'studio_color_input("movement_connector_color", "Movement connector color", value = "#64748b")', fixed = TRUE)
  expect_match(studio_code, '"movement_connector_opacity", "Movement connector opacity"', fixed = TRUE)
  expect_match(studio_code, '"movement_connector_linewidth", "Movement connector thickness"', fixed = TRUE)
  expect_match(studio_code, '"movement_connector_linetype", "Movement connector line style"', fixed = TRUE)
  expect_match(studio_code, '"movement_connector_endpoint", "Movement connector arrow"', fixed = TRUE)
  expect_match(studio_code, "movementConnectorColor = studio_color_value", fixed = TRUE)
  expect_match(studio_code, "movement_connector_endpoint = input$movement_connector_endpoint", fixed = TRUE)
})

test_that("spatial studio exposes geography removal in the left sidebar", {
  studio_file <- system.file("examples", "shiny_spatial_studio.R", package = "dragmapr")
  studio_code <- paste(readLines(studio_file, warn = FALSE), collapse = "\n")

  expect_match(studio_code, '"Geography editing"', fixed = TRUE)
  expect_match(studio_code, '"Inspect or remove unneeded polygons"', fixed = TRUE)
  expect_match(studio_code, 'uiOutput("geography_edit_ui")', fixed = TRUE)
  expect_match(studio_code, 'tableOutput("selected_geography_table")', fixed = TRUE)
  expect_match(studio_code, 'actionButton("remove_selected_geography"', fixed = TRUE)
  expect_match(studio_code, "remove_selected_geography_now <- function", fixed = TRUE)
  expect_match(studio_code, "push_history(pre_delete, force = TRUE)", fixed = TRUE)
  expect_match(studio_code, "Undo restores removed geography", fixed = TRUE)
})

test_that("spatial studio sends legend and label value filters live", {
  studio_file <- system.file("examples", "shiny_spatial_studio.R", package = "dragmapr")
  studio_code <- paste(readLines(studio_file, warn = FALSE), collapse = "\n")

  expect_match(studio_code, "Shiny.addCustomMessageHandler('dragmapr-legend-values'", fixed = TRUE)
  expect_match(studio_code, "Shiny.addCustomMessageHandler('dragmapr-label-values'", fixed = TRUE)
  expect_match(studio_code, '"dragmapr-legend-values"', fixed = TRUE)
  expect_match(studio_code, '"dragmapr-label-values"', fixed = TRUE)
  expect_match(studio_code, "legend_values       = legend_values()", fixed = TRUE)
  expect_match(studio_code, "label_values        = visible_label_ids()", fixed = TRUE)
})

test_that("spatial studio round-trips saved project display metadata", {
  studio_file <- system.file("examples", "shiny_spatial_studio.R", package = "dragmapr")
  studio_code <- paste(readLines(studio_file, warn = FALSE), collapse = "\n")

  exported <- c(
    "show_connectors     = isTRUE(input$show_connectors)",
    "map_background      = input$map_background",
    "annotation_mode     = input$annotation_mode",
    "label_text_size     = input$label_text_size",
    "label_radius        = input$label_radius",
    "label_width         = input$label_width",
    "label_height        = input$label_height",
    "box_width           = input$box_width",
    "box_height          = input$box_height",
    "connector_type      = input$connector_type",
    "connector_linetype  = input$connector_linetype",
    "connector_endpoint  = input$connector_endpoint",
    "connector_smart     = isTRUE(input$connector_smart)"
  )
  for (pattern in exported) {
    expect_match(studio_code, pattern, fixed = TRUE)
  }

  restored <- c(
    'updateCheckboxInput(session, "show_labels"',
    'updateCheckboxInput(session, "show_connectors"',
    'updateSelectInput(session, "map_background", selected = metadata$map_background',
    'updateSelectInput(session, "annotation_mode", selected = metadata$annotation_mode',
    'updateSelectInput(session, "label_marker_shape", selected = metadata$label_marker_shape',
    'updateSliderInput(session, "label_text_size", value = metadata$label_text_size',
    'updateSliderInput(session, "label_radius", value = metadata$label_radius',
    'updateSliderInput(session, "label_width", value = metadata$label_width',
    'updateSliderInput(session, "label_height", value = metadata$label_height',
    'updateSliderInput(session, "box_width", value = metadata$box_width',
    'updateSliderInput(session, "box_height", value = metadata$box_height',
    'updateSliderInput(session, "connector_linewidth", value = metadata$connector_linewidth',
    'updateSelectInput(session, "connector_type", selected = metadata$connector_type',
    'updateSelectInput(session, "connector_linetype", selected = metadata$connector_linetype',
    'updateSelectInput(session, "connector_endpoint", selected = metadata$connector_endpoint',
    'updateCheckboxInput(session, "connector_smart", value = isTRUE(metadata$connector_smart'
  )
  for (pattern in restored) {
    expect_match(studio_code, pattern, fixed = TRUE)
  }
  expect_match(studio_code, "metadata$marker_size", fixed = TRUE)
})

test_that("spatial studio initializes legend and label multiselects with all choices", {
  studio_file <- system.file("examples", "shiny_spatial_studio.R", package = "dragmapr")
  studio_code <- paste(readLines(studio_file, warn = FALSE), collapse = "\n")

  expect_match(studio_code, "legend_filter_selection = NULL", fixed = TRUE)
  expect_match(studio_code, "label_filter_selection = NULL", fixed = TRUE)
  expect_match(studio_code, "'studio_filter_change'", fixed = TRUE)
  expect_match(studio_code, "observeEvent(input$studio_filter_change", fixed = TRUE)
  expect_match(studio_code, "inputId = dropdown.id.slice", fixed = TRUE)
  expect_match(studio_code, "document.getElementById(inputId + '-dropdown')", fixed = TRUE)
  expect_match(studio_code, "selected <- state$legend_filter_selection", fixed = TRUE)
  expect_match(studio_code, "selected <- state$label_filter_selection", fixed = TRUE)
  expect_false(grepl(
    "state$pending_legend_values",
    studio_code,
    fixed = TRUE
  ))
  expect_false(grepl(
    "state$pending_label_values",
    studio_code,
    fixed = TRUE
  ))
})

test_that("spatial studio guards confirmed demo stability issues", {
  studio_file <- system.file("examples", "shiny_spatial_studio.R", package = "dragmapr")
  studio_code <- paste(readLines(studio_file, warn = FALSE), collapse = "\n")

  expect_match(studio_code, "if (nrow(x) == 0L)", fixed = TRUE)
  expect_match(studio_code, "}, ignoreInit = TRUE)\n\n  build_plot <- function", fixed = TRUE)
  expect_match(studio_code, "choose_column(isolate(input$region_col)", fixed = TRUE)
  expect_match(studio_code, "choose_column(isolate(input$label_col)", fixed = TRUE)
  expect_match(studio_code, "pal <- isolate(region_palette())", fixed = TRUE)
  expect_match(studio_code, "selected <- isolate(input$region_color_group)", fixed = TRUE)
  expect_match(studio_code, "selected <- isolate(input$edit_label_id)", fixed = TRUE)
  expect_match(studio_code, "Project legend selection did not match", fixed = TRUE)
  expect_match(studio_code, "Project label selection did not match", fixed = TRUE)
  expect_match(studio_code, "req(file.exists(helper_file))", fixed = TRUE)
})

test_that("spatial studio avoids id-like columns as upload defaults", {
  studio_file <- system.file("examples", "shiny_spatial_studio.R", package = "dragmapr")
  studio_code <- paste(readLines(studio_file, warn = FALSE), collapse = "\n")

  expect_match(studio_code, "default_studio_region_col <- function", fixed = TRUE)
  expect_match(studio_code, 'exact_priority <- c("hhs_region", "region", "group", "county"', fixed = TRUE)
  expect_match(studio_code, "id_like <- grepl", fixed = TRUE)
  expect_match(studio_code, "default_studio_region_col(projected_sf(), cols)", fixed = TRUE)
})
