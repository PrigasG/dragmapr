options(shiny.maxRequestSize = max(getOption("shiny.maxRequestSize", 5 * 1024^2), 250 * 1024^2))

library(shiny)
library(sf)
library(dragmapr)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

APP_TMP <- file.path(tempdir(), "branch_bloom_test_html")
dir.create(APP_TMP, recursive = TRUE, showWarnings = FALSE)
addResourcePath("branch_bloom_test", APP_TMP)

clean_key <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- "(missing)"
  x
}

safe_columns <- function(x) {
  geom_col <- attr(x, "sf_column")
  cols <- setdiff(names(x), geom_col)
  cols[vapply(cols, function(col) {
    z <- x[[col]]
    is.atomic(z) && length(unique(stats::na.omit(as.character(z)))) > 1L
  }, logical(1))]
}

pick_default_parent <- function(x, cols) {
  if (length(cols) == 0L) return(NULL)
  lower <- tolower(cols)
  priority <- c("county", "region", "district", "state", "zone", "group")
  for (p in priority) {
    idx <- which(lower == p | grepl(p, lower, fixed = TRUE))
    if (length(idx) > 0L) return(cols[idx[1L]])
  }
  n <- max(1L, nrow(x))
  card <- vapply(cols, function(col) length(unique(stats::na.omit(clean_key(x[[col]])))), integer(1))
  candidates <- which(card >= 2L & card <= max(2L, min(80L, ceiling(n / 2))))
  if (length(candidates) > 0L) return(cols[candidates[order(card[candidates])][1L]])
  cols[1L]
}

pick_default_child <- function(x, cols, parent_col) {
  if (length(cols) == 0L) return(NULL)
  parent_card <- if (!is.null(parent_col) && parent_col %in% cols) {
    length(unique(stats::na.omit(clean_key(x[[parent_col]]))))
  } else 0L
  lower <- tolower(cols)
  priority <- c("mun", "municip", "city", "town", "tract", "name", "facility")
  for (p in priority) {
    idx <- which(cols != parent_col & grepl(p, lower, fixed = TRUE))
    if (length(idx) > 0L) return(cols[idx[1L]])
  }
  card <- vapply(cols, function(col) length(unique(stats::na.omit(clean_key(x[[col]])))), integer(1))
  candidates <- which(cols != parent_col & card > parent_card)
  if (length(candidates) > 0L) return(cols[candidates[order(card[candidates])][1L]])
  setdiff(cols, parent_col)[1L] %||% parent_col
}

read_spatial_upload <- function(upload) {
  if (is.null(upload) || nrow(upload) == 0L) {
    stop("Upload a spatial file first.", call. = FALSE)
  }

  work <- tempfile("spatial_upload_")
  dir.create(work, recursive = TRUE)

  for (i in seq_len(nrow(upload))) {
    file.copy(upload$datapath[i], file.path(work, upload$name[i]), overwrite = TRUE)
  }

  files <- file.path(work, upload$name)
  if (length(files) == 1L && grepl("\\.zip$", files, ignore.case = TRUE)) {
    unzip(files, exdir = work)
  }

  all_files <- list.files(work, recursive = TRUE, full.names = TRUE)
  candidates <- all_files[grepl("\\.(gpkg|geojson|json|shp)$", all_files, ignore.case = TRUE)]
  if (length(candidates) == 0L) {
    stop("No .gpkg, .geojson, .json, or .shp file was found. If this is a shapefile, upload a ZIP containing the .shp, .shx, .dbf, and .prj files.", call. = FALSE)
  }

  ext <- tolower(tools::file_ext(candidates))
  ord <- order(match(ext, c("gpkg", "geojson", "json", "shp")), candidates)
  target <- candidates[ord][1L]

  x <- sf::st_read(target, quiet = TRUE, stringsAsFactors = FALSE)
  if (!inherits(x, "sf") || nrow(x) == 0L) {
    stop("The uploaded spatial file was read, but it has no features.", call. = FALSE)
  }

  x <- tryCatch(sf::st_zm(x, drop = TRUE, what = "ZM"), error = function(e) x)
  x <- tryCatch(sf::st_make_valid(x), error = function(e) x)

  if (is.na(sf::st_crs(x))) {
    attr(x, "branch_bloom_crs_note") <- "CRS is missing. If the map looks stretched or tiny, assign a projected CRS before testing."
  } else if (sf::st_is_longlat(x)) {
    x <- sf::st_transform(x, 3857)
  }

  x
}

make_region_palette <- function(values) {
  base <- c(
    "#4C78A8", "#F58518", "#54A24B", "#B279A2", "#E45756",
    "#72B7B2", "#EECA3B", "#B74F6F", "#8CD17D", "#79706E",
    "#2563EB", "#059669", "#DC2626", "#7C3AED", "#0891B2"
  )
  values <- sort(unique(as.character(values)))
  pal <- rep(base, length.out = length(values))
  stats::setNames(pal, values)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("\n      body { background: #f8fafc; color: #172033; }\n      .tester-shell { display: grid; grid-template-columns: 360px minmax(0, 1fr); gap: 14px; min-height: calc(100vh - 30px); }\n      .tester-panel { background: white; border: 1px solid #e5e7eb; border-radius: 10px; padding: 14px; box-shadow: 0 1px 4px rgba(15, 23, 42, 0.06); max-height: calc(100vh - 30px); overflow-y: auto; }\n      .tester-panel h2 { margin-top: 0; font-size: 1.1rem; }\n      .tester-help { color: #64748b; font-size: 0.86rem; line-height: 1.45; }\n      .tester-status { padding: 8px 10px; border-radius: 8px; background: #eff6ff; border: 1px solid #bfdbfe; color: #1d4ed8; font-size: 0.84rem; line-height: 1.4; margin-bottom: 10px; }\n      .tester-map { position: relative; background: white; border: 1px solid #e5e7eb; border-radius: 10px; overflow: hidden; min-height: 760px; }\n      .tester-frame { width: 100%; height: calc(100vh - 44px); min-height: 760px; border: 0; display: block; }\n      .tiny { font-size: 0.8rem; color: #64748b; }\n      .hr { border-top: 1px dashed #e5e7eb; margin: 12px 0; }\n      .btn-primary { width: 100%; }\n      .shiny-bound-output.recalculating, .recalculating { opacity: 1 !important; }\n      .tester-overlay { position: absolute; inset: 0; z-index: 20; display: flex; align-items: center; justify-content: center; background: rgba(248, 250, 252, 0.78); backdrop-filter: blur(2px); opacity: 0; visibility: hidden; pointer-events: none; transition: opacity 0.16s ease, visibility 0s linear 0.16s; }\n      body.tester-busy .tester-overlay { opacity: 1; visibility: visible; pointer-events: auto; transition-delay: 0s; }\n      body.tester-busy .tester-frame { filter: saturate(0.94); }\n      .tester-overlay-card { min-width: 260px; max-width: 360px; padding: 14px 16px; border: 1px solid #dbe3ee; border-radius: 10px; background: #fff; box-shadow: 0 14px 38px rgba(15, 23, 42, 0.16); display: flex; gap: 12px; align-items: center; }\n      .tester-spinner { width: 22px; height: 22px; border-radius: 999px; border: 3px solid #dbeafe; border-top-color: #2563eb; animation: tester-spin 0.85s linear infinite; flex: 0 0 auto; }\n      .tester-overlay-card strong { display: block; font-size: 0.92rem; color: #172033; }\n      .tester-overlay-card span { display: block; margin-top: 2px; font-size: 0.78rem; color: #64748b; }\n      @keyframes tester-spin { to { transform: rotate(360deg); } }\n      @media (max-width: 900px) { .tester-shell { grid-template-columns: 1fr; } .tester-frame { height: 740px; } }\n    ")),
    tags$script(HTML("\n      (function() {\n        function setText(id, value) {\n          var node = document.getElementById(id);\n          if (node) node.textContent = value;\n        }\n        window.branchBloomTesterBusy = function(message, detail) {\n          setText('tester-overlay-title', message || 'Building test map');\n          setText('tester-overlay-detail', detail || 'Preparing spatial layers and labels...');\n          document.body.classList.add('tester-busy');\n        };\n        window.branchBloomTesterHideBusy = function() {\n          document.body.classList.remove('tester-busy');\n          window.__branchTesterBuilt = true;\n        };\n        document.addEventListener('click', function(e) {\n          if (e.target && e.target.id === 'build') {\n            window.branchBloomTesterBusy('Building test map', 'Preparing parent shells, child labels, and animation payload...');\n          }\n        }, true);\n        document.addEventListener('change', function(e) {\n          var ids = [\n            'animation_mode', 'duration', 'easing', 'leaf_strength', 'leaf_child_scale',\n            'show_parent_ghost', 'parent_ghost_opacity', 'show_parent_labels',\n            'show_child_labels', 'parent_label_col', 'child_label_col', 'label_shape',\n            'enable_dotted_drag', 'dotted_drag_threshold'\n          ];\n          if (window.__branchTesterBuilt && e.target && ids.indexOf(e.target.id) >= 0) {\n            window.branchBloomTesterBusy('Refreshing test map', 'Applying updated animation and label settings...');\n          }\n        }, true);\n        if (window.Shiny) {\n          Shiny.addCustomMessageHandler('testerBusy', function(x) {\n            if (x && x.show) window.branchBloomTesterBusy(x.message, x.detail);\n            else window.branchBloomTesterHideBusy();\n          });\n        }\n      })();\n    "))
  ),
  div(
    class = "tester-shell",
    div(
      class = "tester-panel",
      h2("Branch bloom tester"),
      p(class = "tester-help", "Upload a spatial file, choose parent and child columns, then build a test map. This isolates branch-bloom, D3-style leaf flip, labels, and drag shadow behavior outside Spatial Studio."),
      uiOutput("status"),
      fileInput(
        "spatial",
        "Spatial file",
        multiple = TRUE,
        accept = c(".zip", ".gpkg", ".geojson", ".json", ".shp", ".dbf", ".shx", ".prj")
      ),
      uiOutput("column_controls"),
      div(class = "hr"),
      tags$strong("Labels"),
      checkboxInput("show_parent_labels", "Show parent labels", value = TRUE),
      checkboxInput("show_child_labels", "Show child labels", value = TRUE),
      selectInput("label_shape", "Label marker", choices = c("rect", "circle", "none"), selected = "rect"),
      checkboxInput("show_legend", "Show parent legend", value = TRUE),
      checkboxInput("helper_panel", "Show helper offset panel", value = FALSE),
      div(class = "hr"),
      tags$strong("Animation"),
      sliderInput("duration", "Animation duration", min = 225, max = 900, value = 375, step = 25, post = " ms"),
      selectInput("easing", "Easing", choices = c("cubic-out", "cubic-in-out", "linear"), selected = "cubic-out"),
      selectInput(
        "animation_mode",
        "Animation style",
        choices = c(
          "Clean branch bloom" = "branch_bloom",
          "D3 leaf flip test" = "leaf_flip"
        ),
        selected = "branch_bloom"
      ),
      p(class = "tiny", "After the first build, animation and label changes refresh the test map with a clear overlay."),
      conditionalPanel(
        "input.animation_mode == 'leaf_flip'",
        sliderInput("leaf_strength", "Leaf flip strength", min = 0.05, max = 0.45, value = 0.16, step = 0.01),
        sliderInput("leaf_child_scale", "Leaf child visual size", min = 0.70, max = 1.00, value = 0.86, step = 0.02),
        p(class = "tiny", "Leaf mode now uses each child feature's actual polygon shape as the temporary proxy. Children flip back first, then the parent reappears.")
      ),
      div(class = "hr"),
      tags$strong("Dotted group drag"),
      checkboxInput("enable_dotted_drag", "Show dotted drag frame after expansion", value = TRUE),
      conditionalPanel(
        "input.enable_dotted_drag == true",
        sliderInput("dotted_drag_threshold", "Click vs drag threshold", min = 3, max = 18, value = 8, step = 1, post = " px"),
        p(class = "tiny", "After a branch finishes expanding, drag the dashed border to move the whole child group. Drag inside the frame to move individual children. A tiny click on a child or the dashed border compresses back to the parent.")
      ),
      div(class = "hr"),
      tags$strong("Shadow isolation"),
      p(class = "tiny", "Keep these off first. If a shadow remains, it is coming from the core helper animation or browser repaint, not optional trails/bands."),
      checkboxInput("show_parent_ghost", "Show parent ghost after bloom", value = FALSE),
      conditionalPanel(
        "input.show_parent_ghost == true",
        sliderInput("parent_ghost_opacity", "Parent ghost opacity", min = 0, max = 0.5, value = 0.18, step = 0.02)
      ),
      checkboxInput("show_drag_trail", "Show drag trail", value = FALSE),
      checkboxInput("show_origin_outlines", "Show origin outlines", value = FALSE),
      checkboxInput("show_movement_connectors", "Show movement connectors", value = FALSE),
      checkboxInput("show_movement_band", "Show movement band", value = FALSE),
      actionButton("build", "Build test map", class = "btn-primary"),
      div(class = "hr"),
      uiOutput("detected")
    ),
    div(
      class = "tester-map",
      uiOutput("helper"),
      div(
        class = "tester-overlay",
        div(
          class = "tester-overlay-card",
          div(class = "tester-spinner"),
          div(
            strong(id = "tester-overlay-title", "Building test map"),
            span(id = "tester-overlay-detail", "Preparing spatial layers and labels...")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  last_build_summary <- reactiveVal(NULL)

  source_sf <- reactive({
    req(input$spatial)
    read_spatial_upload(input$spatial)
  })

  observeEvent(source_sf(), {
    x <- source_sf()
    cols <- safe_columns(x)
    parent <- pick_default_parent(x, cols)
    child <- pick_default_child(x, cols, parent)
    updateSelectInput(session, "parent_col", choices = cols, selected = parent)
    updateSelectInput(session, "child_col",  choices = cols, selected = child)
    updateSelectInput(session, "parent_label_col", choices = cols, selected = parent)
    updateSelectInput(session, "child_label_col",  choices = cols, selected = child %||% parent)
  }, ignoreInit = TRUE)

  output$status <- renderUI({
    if (is.null(input$spatial)) {
      return(div(class = "tester-status", "Upload a zipped shapefile, GeoPackage, or GeoJSON to begin."))
    }
    x <- tryCatch(source_sf(), error = function(e) e)
    if (inherits(x, "error")) {
      return(div(class = "tester-status", style = "background:#fef2f2;border-color:#fecaca;color:#991b1b;", x$message))
    }
    note <- attr(x, "branch_bloom_crs_note")
    div(
      class = "tester-status",
      paste0("Loaded ", format(nrow(x), big.mark = ","), " features. Select columns and build the test map."),
      if (!is.null(note)) tags$div(style = "margin-top:4px;", note)
    )
  })

  output$column_controls <- renderUI({
    x <- tryCatch(source_sf(), error = function(e) NULL)
    if (is.null(x)) {
      return(tagList(
        selectInput("parent_col", "Parent column", choices = character()),
        selectInput("child_col",  "Child column", choices = character()),
        selectInput("parent_label_col", "Parent label column", choices = character()),
        selectInput("child_label_col",  "Child label column", choices = character())
      ))
    }
    cols <- safe_columns(x)
    parent <- pick_default_parent(x, cols)
    child <- pick_default_child(x, cols, parent)
    tagList(
      selectInput("parent_col", "Parent column", choices = cols, selected = parent),
      selectInput("child_col", "Child column", choices = cols, selected = child),
      selectInput("parent_label_col", "Parent label column", choices = cols, selected = parent),
      selectInput("child_label_col", "Child label column", choices = cols, selected = child %||% parent)
    )
  })

  build_nonce <- reactiveVal(0L)

  observeEvent(input$build, {
    session$sendCustomMessage("testerBusy", list(
      show = TRUE,
      message = "Building test map",
      detail = "Preparing parent shells, child labels, and animation payload..."
    ))
    build_nonce(build_nonce() + 1L)
  })

  observeEvent(
    list(
      input$animation_mode,
      input$leaf_strength,
      input$leaf_child_scale,
      input$duration,
      input$easing,
      input$show_parent_ghost,
      input$parent_ghost_opacity,
      input$show_parent_labels,
      input$show_child_labels,
      input$parent_label_col,
      input$child_label_col,
      input$label_shape,
      input$show_legend,
      input$enable_dotted_drag,
      input$dotted_drag_threshold
    ),
    {
      if (!is.null(input$build) && input$build > 0L) {
        session$sendCustomMessage("testerBusy", list(
          show = TRUE,
          message = "Refreshing test map",
          detail = "Applying updated animation and label settings..."
        ))
        build_nonce(build_nonce() + 1L)
      }
    },
    ignoreInit = TRUE
  )

  helper_file <- reactive({
    build_nonce()
    validate(need(!is.null(input$build) && input$build > 0L, "Click Build test map."))
    x <- source_sf()
    validate(need(input$parent_col %in% names(x), "Choose a valid parent column."))
    validate(need(input$child_col %in% names(x), "Choose a valid child column."))
    validate(need(!identical(input$parent_col, input$child_col), "Choose different parent and child columns for branch-bloom testing."))

    built <- dragmapr::build_branch_transition_data(
      x,
      parent_col = input$parent_col,
      child_col  = input$child_col,
      animation = input$animation_mode %||% "branch_bloom",
      duration_ms = input$duration %||% 400,
      easing = input$easing %||% "cubic-out",
      show_parent_ghost = isTRUE(input$show_parent_ghost),
      parent_ghost_opacity = if (isTRUE(input$show_parent_ghost)) {
        input$parent_ghost_opacity %||% 0.18
      } else {
        0
      },
      leaf_flip_strength = input$leaf_strength %||% 0.16,
      leaf_child_scale = input$leaf_child_scale %||% 0.86,
      leaf_expand_duration_factor = 0.82,
      leaf_collapse_duration_factor = 0.58,
      boundary = isTRUE(input$enable_dotted_drag),
      boundary_behavior = if (isTRUE(input$enable_dotted_drag)) "drag" else "none",
      boundary_label = "Drag to",
      boundary_drag_threshold = input$dotted_drag_threshold %||% 8
    )
    anim_sf <- built$sf

    child_sf <- anim_sf[anim_sf[[built$shell_col]] == 0L, , drop = FALSE]
    parent_values <- sort(unique(as.character(child_sf[[built$parent_key_col]])))
    child_values <- sort(unique(as.character(child_sf[[built$child_key_col]])))

    labels <- dragmapr::make_branch_bloom_labels(
      anim_sf,
      parent_key_col = built$parent_key_col,
      child_key_col = built$child_key_col,
      shell_col = built$shell_col,
      parent_label_col = if (input$parent_label_col %in% names(anim_sf)) input$parent_label_col else built$parent_key_col,
      child_label_col = if (input$child_label_col %in% names(anim_sf)) input$child_label_col else built$child_key_col,
      show_parent_labels = isTRUE(input$show_parent_labels),
      show_child_labels = isTRUE(input$show_child_labels)
    )
    if (!is.data.frame(labels) || nrow(labels) == 0L) {
      labels <- FALSE
    }

    html_file <- file.path(APP_TMP, paste0("branch-bloom-test-", as.integer(Sys.time()), "-", sample.int(999999, 1), ".html"))

    dragmapr::drag_map_prototype(
      anim_sf,
      region_col = built$parent_key_col,
      label_col = if (input$parent_label_col %in% names(anim_sf)) input$parent_label_col else built$parent_key_col,
      labels = labels,
      draggable_labels = TRUE,
      label_marker = !identical(input$label_shape, "none"),
      label_marker_shape = input$label_shape,
      label_text_size = 11,
      region_palette = make_region_palette(parent_values),
      show_legend = isTRUE(input$show_legend),
      max_legend_keys = Inf,
      legend_title = input$parent_col,
      legend_reflects_bloom = FALSE,
      map_background = "white",
      connector_linetype = "solid",
      connector_endpoint = "none",
      connector_smart = FALSE,
      show_origin_outlines = isTRUE(input$show_origin_outlines),
      show_movement_connectors = isTRUE(input$show_movement_connectors),
      show_movement_band = isTRUE(input$show_movement_band),
      show_drag_trail = isTRUE(input$show_drag_trail),
      side_panel = isTRUE(input$helper_panel),
      transition = c(built$transition, list(debug = FALSE)),
      file = html_file,
      open = FALSE
    )

    summary <- list(
      file = html_file,
      parents = length(parent_values),
      children = length(child_values),
      labels = if (is.data.frame(labels)) nrow(labels) else 0L,
      parent_labels = if (is.data.frame(labels)) sum(labels$label_level == "parent") else 0L,
      child_labels = if (is.data.frame(labels)) sum(labels$label_level == "child") else 0L,
      shell_rows = sum(anim_sf[[built$shell_col]] == 1L),
      feature_rows = nrow(anim_sf)
    )
    last_build_summary(summary)
    summary
  })

  output$helper <- renderUI({
    hf <- helper_file()
    src <- paste0("branch_bloom_test/", basename(hf$file), "?v=", as.integer(Sys.time()))
    tags$iframe(
      src = src,
      class = "tester-frame",
      title = "branch bloom test map",
      onload = "window.branchBloomTesterHideBusy && window.branchBloomTesterHideBusy();"
    )
  })

  output$detected <- renderUI({
    x <- tryCatch(source_sf(), error = function(e) NULL)
    if (is.null(x)) return(NULL)
    cols <- safe_columns(x)
    parent <- input$parent_col %||% pick_default_parent(x, cols)
    child <- input$child_col %||% pick_default_child(x, cols, parent)
    parent_n <- if (!is.null(parent) && parent %in% names(x)) length(unique(clean_key(x[[parent]]))) else 0L
    child_n <- if (!is.null(child) && child %in% names(x)) length(unique(paste(clean_key(x[[parent]]), clean_key(x[[child]]), sep = " / "))) else 0L
    hf <- last_build_summary()
    tagList(
      tags$strong("Current test"),
      tags$div(class = "tiny", paste("Features:", format(nrow(x), big.mark = ","))),
      tags$div(class = "tiny", paste("Columns:", length(cols))),
      tags$div(class = "tiny", paste("Parents:", parent_n)),
      tags$div(class = "tiny", paste("Composite children:", child_n)),
      tags$div(class = "tiny", paste("Parent label column:", input$parent_label_col %||% parent)),
      tags$div(class = "tiny", paste("Child label column:", input$child_label_col %||% child)),
      tags$div(class = "tiny", paste("Animation:", input$animation_mode %||% "branch_bloom")),
      tags$div(class = "tiny", paste("Leaf strength:", if (identical(input$animation_mode, "leaf_flip")) input$leaf_strength %||% 0.16 else "not used")),
      tags$div(class = "tiny", paste("Leaf child visual size:", if (identical(input$animation_mode, "leaf_flip")) input$leaf_child_scale %||% 0.86 else "not used")),
      tags$div(class = "tiny", paste("Dotted group drag:", if (isTRUE(input$enable_dotted_drag)) paste0("on, threshold ", input$dotted_drag_threshold %||% 8, " px") else "off")),
      tags$div(class = "tiny", paste("Parent ghost:", if (isTRUE(input$show_parent_ghost)) "on" else "off")),
      if (!is.null(hf)) tagList(
        tags$div(class = "tiny", paste("Helper rows:", hf$feature_rows)),
        tags$div(class = "tiny", paste("Shell rows:", hf$shell_rows)),
        tags$div(class = "tiny", paste("Mounted labels:", hf$labels)),
        tags$div(class = "tiny", paste("Parent labels:", hf$parent_labels)),
        tags$div(class = "tiny", paste("Child labels:", hf$child_labels))
      )
    )
  })
}

if (interactive()) {
  shinyApp(ui, server)
}
